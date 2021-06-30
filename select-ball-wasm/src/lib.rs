// SPDX-License-Identifier: MPL-2.0

use image::{DynamicImage, GenericImageView};
use image::{ImageBuffer, Pixel};
use nalgebra::{DMatrix, Vector6};
use serde::Deserialize;
use std::cell::RefCell;
use std::cmp;
use std::fmt;
use std::io::Cursor;
use std::rc::Rc;
use wasm_bindgen::prelude::*;

use select_ball::img::crop::{crop, recover_original_motion, Crop};
use select_ball::img::registration::{self, CanRegister};
use select_ball::interop::{IntoDMatrix, ToImage};
use select_ball::utils::CanEqualize;

#[macro_use]
mod utils; // define console_log! macro

#[wasm_bindgen(raw_module = "../worker.mjs")]
extern "C" {
    #[wasm_bindgen(js_name = "shouldStop")]
    async fn should_stop(step: &str, progress: Option<u32>) -> JsValue; // bool
}

// This wrapper trick is because we cannot have async functions referencing &self.
// https://github.com/rustwasm/wasm-bindgen/issues/1858
#[wasm_bindgen]
pub struct SelectBall(Rc<RefCell<SelectBallInner>>);

#[wasm_bindgen]
impl SelectBall {
    pub fn init() -> Self {
        SelectBall(Rc::new(RefCell::new(SelectBallInner::init())))
    }
    pub fn load(&mut self, id: String, img_file: &[u8]) -> Result<(), JsValue> {
        let inner = Rc::clone(&self.0);
        let result = (*inner).borrow_mut().load(id, img_file);
        result
    }
    pub fn run(&mut self, params: JsValue) -> js_sys::Promise {
        let inner = Rc::clone(&self.0);
        wasm_bindgen_futures::future_to_promise(async_run_rc(inner, params))
    }
    pub fn image_ids(&self) -> Result<JsValue, JsValue> {
        self.0.borrow().image_ids()
    }
    pub fn cropped_img_file(&self, i: usize) -> Result<Box<[u8]>, JsValue> {
        self.0.borrow().cropped_img_file(i)
    }
    pub fn register_and_save(&self, i: usize) -> Result<Box<[u8]>, JsValue> {
        self.0.borrow().register_and_save(i)
    }
}

async fn async_run_rc(
    mutself: Rc<RefCell<SelectBallInner>>,
    params: JsValue,
) -> Result<JsValue, JsValue> {
    let mut inner = (*mutself).borrow_mut();
    let result = inner.run(params);
    result.await
}

struct SelectBallInner {
    image_ids: Vec<String>,
    dataset: Vec<DynamicImage>,
    crop_registered: Vec<DynamicImage>,
    motion_vec: Option<Vec<Vector6<f32>>>,
}

// enum Dataset {
//     Empty,
//     // GrayImages(Vec<DMatrix<u8>>),
//     // GrayImagesU16(Vec<DMatrix<u16>>),
//     // RgbImages(Vec<DMatrix<(u8, u8, u8)>>),
//     // RgbImagesU16(Vec<DMatrix<(u16, u16, u16)>>),
//     GrayImages(Vec<DynamicImage>),
//     GrayImagesU16(Vec<DynamicImage>),
//     RgbImages(Vec<DynamicImage>),
//     RgbImagesU16(Vec<DynamicImage>),
// }

#[wasm_bindgen]
#[derive(Deserialize)]
/// Type holding the algorithm parameters
pub struct Args {
    pub config: registration::Config,
    pub equalize: Option<f32>,
    pub crop: Option<Crop>,
}

impl SelectBallInner {
    pub fn init() -> Self {
        utils::set_panic_hook();
        utils::WasmLogger::init().unwrap();
        utils::WasmLogger::setup(log::LevelFilter::Trace);
        Self {
            image_ids: Vec::new(),
            dataset: Vec::new(),
            crop_registered: Vec::new(),
            motion_vec: None,
        }
    }

    // Load and decode the images to be registered.
    pub fn load(&mut self, id: String, img_file: &[u8]) -> Result<(), JsValue> {
        // console_log!("Loading an image");
        let reader = image::io::Reader::new(Cursor::new(img_file))
            .with_guessed_format()
            .expect("Cursor io never fails");
        // let image = reader.decode().expect("Error decoding the image");
        let dyn_img = reader.decode().map_err(utils::report_error)?;

        match (&dyn_img, &mut self.dataset) {
            // Loading the first image (empty dataset)
            // (DynamicImage::ImageLuma8(_), Dataset::Empty) => {
            //     log::info!("Images are of type Gray u8");
            //     // self.dataset = Dataset::GrayImages(vec![dyn_img.into_dmatrix()]);
            //     self.dataset = Dataset::GrayImages(vec![dyn_img]);
            //     self.image_ids = vec![id];
            // }
            // // Loading of subsequent images
            // (DynamicImage::ImageLuma8(_), Dataset::GrayImages(imgs)) => {
            //     // imgs.push(dyn_img.into_dmatrix());
            //     imgs.push(dyn_img);
            //     self.image_ids.push(id);
            // }
            // // Loading the first image (empty dataset)
            // (DynamicImage::ImageLuma16(_), Dataset::Empty) => {
            //     log::info!("Images are of type Gray u16");
            //     // self.dataset = Dataset::GrayImagesU16(vec![dyn_img.into_dmatrix()]);
            //     self.dataset = Dataset::GrayImagesU16(vec![dyn_img]);
            //     self.image_ids = vec![id];
            // }
            // // Loading of subsequent images
            // (DynamicImage::ImageLuma16(_), Dataset::GrayImagesU16(imgs)) => {
            //     // imgs.push(dyn_img.into_dmatrix());
            //     imgs.push(dyn_img);
            //     self.image_ids.push(id);
            // }
            // // Loading the first image (empty dataset)
            // (DynamicImage::ImageRgb8(_), Dataset::Empty) => {
            //     log::info!("Images are of type RGB (u8, u8, u8)");
            //     // self.dataset = Dataset::RgbImages(vec![dyn_img.into_dmatrix()]);
            //     self.dataset = Dataset::RgbImages(vec![dyn_img]);
            //     self.image_ids = vec![id];
            // }
            // // Loading of subsequent images
            // (DynamicImage::ImageRgb8(_), Dataset::RgbImages(imgs)) => {
            //     // imgs.push(dyn_img.into_dmatrix());
            //     imgs.push(dyn_img);
            //     self.image_ids.push(id);
            // }
            // // Loading the first image (empty dataset)
            // (DynamicImage::ImageRgb16(_), Dataset::Empty) => {
            //     log::info!("Images are of type RGB (u16, u16, u16)");
            //     // self.dataset = Dataset::RgbImagesU16(vec![dyn_img.into_dmatrix()]);
            //     self.dataset = Dataset::RgbImagesU16(vec![dyn_img]);
            //     self.image_ids = vec![id];
            // }
            // // Loading of subsequent images
            // (DynamicImage::ImageRgb16(_), Dataset::RgbImagesU16(imgs)) => {
            //     // imgs.push(dyn_img.into_dmatrix());
            //     imgs.push(dyn_img);
            //     self.image_ids.push(id);
            // }
            (DynamicImage::ImageBgr8(_), _) => return Err("BGR order not supported".into()),
            (DynamicImage::ImageBgra8(_), _) => return Err("BGR order not supported".into()),
            (DynamicImage::ImageLumaA8(_), _) => return Err("Alpha channel not supported".into()),
            (DynamicImage::ImageLumaA16(_), _) => return Err("Alpha channel not supported".into()),
            (DynamicImage::ImageRgba8(_), _) => return Err("Alpha channel not supported".into()),
            (DynamicImage::ImageRgba16(_), _) => return Err("Alpha channel not supported".into()),
            (_, _) => {
                log::info!("New images treated");
                self.dataset.push(dyn_img);
                self.image_ids.push(id);
            } // _ => return Err("Images are not all of the same type".into()),
        }

        Ok(())
    }

    // Run the main select_ball registration algorithm.
    //                                                 Vec<f32>
    async fn run(&mut self, params: JsValue) -> Result<JsValue, JsValue> {
        self.motion_vec = None;
        self.crop_registered.clear();
        let args: Args = params.into_serde().unwrap();
        utils::WasmLogger::setup(utils::verbosity_filter(args.config.verbosity));

        if self.dataset.is_empty() {
            self.crop_registered = Vec::new();
        } else {
            self.crop_registered = crop_and_register(&args, &self.dataset).await
        }
        // Use the algorithm corresponding to the type of data.
        // let motion_vec = match &self.dataset {
        //     Dataset::Empty => 0,
        //     Dataset::GrayImages(gray_imgs) => {
        //         let cropped_eq_imgs = crop_and_register(&args, gray_imgs.clone(), 40);
        //         log::info!("Applying registration on cropped images ...");
        //         // self.crop_registered = registration::reproject_may_stop::<u8, f32, u8, _>(
        //         //     &cropped_eq_imgs,
        //         //     &motion_vec_crop,
        //         //     should_stop_bool,
        //         // )
        //         // .await
        //         // .map_err(utils::report_error)?;
        //         0
        //     }
        //     Dataset::GrayImagesU16(gray_imgs) => {
        //         let cropped_eq_imgs = crop_and_register(&args, gray_imgs.clone(), 10 * 256);
        //         log::info!("Applying registration on cropped images ...");
        //         // let cropped_u8: Vec<_> = cropped_eq_imgs.into_iter().map(into_gray_u8).collect();
        //         // self.crop_registered = registration::reproject_may_stop::<u8, f32, u8, _>(
        //         //     &cropped_u8,
        //         //     &motion_vec_crop,
        //         //     should_stop_bool,
        //         // )
        //         // .await
        //         // .map_err(utils::report_error)?;
        //         0
        //     }
        //     Dataset::RgbImages(imgs) => {
        //         let cropped_eq_imgs = crop_and_register(&args, imgs.clone(), 40);
        //         log::info!("Applying registration on cropped images ...");
        //         // self.crop_registered = registration::reproject_may_stop::<u8, f32, u8, _>(
        //         //     &cropped_eq_imgs,
        //         //     &motion_vec_crop,
        //         //     should_stop_bool,
        //         // )
        //         // .await
        //         // .map_err(utils::report_error)?;
        //         0
        //     }
        //     Dataset::RgbImagesU16(imgs) => {
        //         let cropped_eq_imgs = crop_and_register(&args, imgs.clone(), 10 * 256);
        //         log::info!("Applying registration on cropped images ...");
        //         // let cropped_u8: Vec<_> = cropped_eq_imgs.into_iter().map(into_gray_u8).collect();
        //         // self.crop_registered = registration::reproject_may_stop::<u8, f32, u8, _>(
        //         //     &cropped_u8,
        //         //     &motion_vec_crop,
        //         //     should_stop_bool,
        //         // )
        //         // .await
        //         // .map_err(utils::report_error)?;
        //         0
        //     }
        // };

        // let flat_motion_vec: Vec<f32> = motion_vec.iter().flatten().cloned().collect();
        // self.motion_vec = Some(motion_vec);
        // JsValue::from_serde(&flat_motion_vec).map_err(utils::report_error)
        let false_vector: Vec<f32> = Vec::new();
        return JsValue::from_serde(&false_vector).map_err(utils::report_error);
    }

    // Return the ids of loaded images: [string]
    pub fn image_ids(&self) -> Result<JsValue, JsValue> {
        JsValue::from_serde(&self.image_ids).map_err(utils::report_error)
    }

    // Retrieve the cropped registered images.
    pub fn cropped_img_file(&self, i: usize) -> Result<Box<[u8]>, JsValue> {
        encode(i, &self.crop_registered[i]).map_err(utils::report_error)
    }

    // Register and save that image.
    pub fn register_and_save(&self, i: usize) -> Result<Box<[u8]>, JsValue> {
        log::info!("Registering image {}", i);
        // match (&self.motion_vec, &self.dataset) {
        //     (_, Dataset::Empty) => {
        //         Err(anyhow!("Images not loaded yet")).map_err(utils::report_error)
        //     }
        //     (None, _) => {
        //         Err(anyhow!("Registration parameters unknown")).map_err(utils::report_error)
        //     }
        //     (Some(all_motion), Dataset::GrayImages(images)) => {
        //         let registered: DMatrix<u8> =
        //             select_ball::img::registration::warp(&images[i], &all_motion[i]);
        //         encode(i, &registered).map_err(utils::report_error)
        //     }
        //     (Some(all_motion), Dataset::GrayImagesU16(images)) => {
        //         let registered: DMatrix<u16> =
        //             select_ball::img::registration::warp(&images[i], &all_motion[i]);
        //         encode(i, &registered).map_err(utils::report_error)
        //     }
        //     (Some(all_motion), Dataset::RgbImages(images)) => {
        //         let registered: DMatrix<(u8, u8, u8)> =
        //             select_ball::img::registration::warp(&images[i], &all_motion[i]);
        //         encode(i, &registered).map_err(utils::report_error)
        //     }
        //     (Some(all_motion), Dataset::RgbImagesU16(images)) => {
        //         let registered: DMatrix<(u16, u16, u16)> =
        //             select_ball::img::registration::warp(&images[i], &all_motion[i]);
        //         encode(i, &registered).map_err(utils::report_error)
        //     }
        // }
        return encode(i, &self.dataset[i]).map_err(utils::report_error);
    }
}

fn encode(i: usize, mat: &DynamicImage) -> anyhow::Result<Box<[u8]>> {
    log::debug!("Encoding image {}", i);
    // let img = mat.to_image();
    let img = mat;
    let mut buffer: Vec<u8> = Vec::new();
    img.write_to(&mut buffer, image::ImageOutputFormat::Png)?;
    Ok(buffer.into_boxed_slice())
}

#[allow(clippy::type_complexity)]
async fn crop_and_register(args: &Args, gray_imgs: &[DynamicImage]) -> Vec<DynamicImage> {
    // Extract the cropped area from the images.
    match args.crop {
        None => gray_imgs.iter().cloned().collect(),
        Some(frame) => {
            log::info!("Cropping images ...");
            gray_imgs
                .iter()
                .map(|im| {
                    log::info!("Compute ray...");
                    let left: f32 = frame.left as f32;
                    let right: f32 = frame.right as f32;
                    let top: f32 = frame.top as f32;
                    let bottom: f32 = frame.bottom as f32;
                    let diag_x: f32 = right - left;
                    let diag_y: f32 = bottom - top;
                    let center_x: f32 = (right + left) / 2.;
                    let center_y: f32 = (bottom + top) / 2.;
                    let mut ray: f32 = diag_x * diag_x + diag_y * diag_y;
                    ray = (ray / 4.0).sqrt();
                    log::info!("Compute true crop frame.");
                    let true_left: u32 = (center_x - ray).round() as u32;
                    let true_top: u32 = (center_y - ray).round() as u32;
                    let true_right: u32 = (center_x + ray).round() as u32;
                    let true_bottom: u32 = (center_y + ray).round() as u32;
                    log::info!(
                        "(l : {}, t : {}, r : {}, b : {})",
                        true_left,
                        true_top,
                        true_right,
                        true_bottom
                    );
                    log::info!("ray : {}", ray);
                    log::info!("Center :({}, {})", center_x, center_y);
                    log::info!("About to crop !");
                    let cropped = im.crop_imm(
                        true_left,
                        true_top,
                        true_right - true_left,
                        true_bottom - true_top,
                    );
                    log::info!("Cropped OK");
                    let image_blured = cropped.blur(args.config.sigma).to_rgb8();
                    log::info!("Blured ok");
                    let only_ball = ImageBuffer::from_fn(
                        image_blured.width(),
                        image_blured.height(),
                        |x, y| {
                            let dx: f32 = (x + true_left) as f32 - center_x;
                            let dy: f32 = (y + true_top) as f32 - center_y;
                            if (dx * dx + dy * dy)
                                <= ray * ray * args.config.mask_ray * args.config.mask_ray
                            {
                                let px = image_blured[(x, y)];
                                if px.to_luma()[0] as f32 > 255.0 * args.config.threshold {
                                    px
                                } else {
                                    image::Rgb([50u8, 50u8, 50u8])
                                }
                            } else {
                                image::Rgb([255u8, 255u8, 255u8])
                            }
                        },
                    );
                    log::info!("Mask applied");
                    DynamicImage::ImageRgb8(only_ball)
                })
                .collect()
        }
    }
}

async fn should_stop_bool(step: &str, progress: Option<u32>) -> bool {
    let js_bool = should_stop(step, progress).await;
    js_bool.as_bool().unwrap()
}

fn original_motion(crop: Option<Crop>, motion_vec_crop: Vec<Vector6<f32>>) -> Vec<Vector6<f32>> {
    // Recover motion parameters in the frame of the full image from the one in the cropped frame.
    match crop {
        None => motion_vec_crop,
        Some(frame) => recover_original_motion(frame, &motion_vec_crop),
    }
}

fn into_gray_u8(m: DMatrix<u16>) -> DMatrix<u8> {
    m.map(|x| (x / 256) as u8)
}
