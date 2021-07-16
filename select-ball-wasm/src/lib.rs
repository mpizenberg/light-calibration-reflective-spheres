// SPDX-License-Identifier: MPL-2.0

use image::DynamicImage;
use image::{ImageBuffer, Pixel};
use nalgebra::{Vector2, Vector3};
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::io::Cursor;
use std::rc::Rc;
use wasm_bindgen::prelude::*;

use select_ball::img::crop::Crop;
use select_ball::img::registration;
// use select_ball::lightsource::{intersection, light_dir};
use crate::lightsource::{intersection, light_dir};

#[macro_use]
mod utils; // define console_log! macro
mod lightsource; // functions to compute light source pos

#[wasm_bindgen(raw_module = "../worker.mjs")]
extern "C" {
    #[wasm_bindgen(js_name = "shouldStop")]
    async fn should_stop(step: &str, progress: Option<u32>) -> JsValue; // bool
}

// This wrapper trick is because we cannot have async functions referencing &self.
// https://github.com/rustwasm/wasm-bindgen/issues/1858
#[wasm_bindgen]
pub struct SelectBall(Rc<RefCell<SelectBallInner>>);

#[derive(Serialize)]
pub struct Point2D {
    pub x: u32,
    pub y: u32,
}

#[derive(Serialize)]
pub struct Point3D {
    pub x: f32,
    pub y: f32,
    pub z: f32,
}

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
    pub fn cropped_img_file(&self, i: usize, channel: usize) -> Result<Box<[u8]>, JsValue> {
        self.0.borrow().cropped_img_file(i, channel)
    }
    pub fn lobes(&self, i: usize, channel: usize) -> JsValue {
        self.0.borrow().lobes(i, channel)
    }
    pub fn register_and_save(&self, i: usize) -> Result<Box<[u8]>, JsValue> {
        self.0.borrow().register_and_save(i)
    }

    pub fn light_vector(&self, i: usize) -> JsValue {
        self.0.borrow().light_vector(i)
    }

    pub fn light_pos(&self, i: usize) -> JsValue {
        self.0.borrow().light_pos(i)
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
    crops_registered: Vec<Vec<DynamicImage>>,
    lobes_centers: Vec<Vec<(u32, u32)>>,
    light_dirs: Vec<Vec<Vector3<f32>>>,
    light_sources: Vec<Vec<Vector3<f32>>>,
}

#[wasm_bindgen]
#[derive(Deserialize)]
/// Type holding the algorithm parameters
pub struct Args {
    pub config: registration::Config,
    pub equalize: Option<f32>,
    pub crop_t_l: Option<Crop>,
    pub crop_t_r: Option<Crop>,
    pub crop_b_l: Option<Crop>,
    pub crop_b_r: Option<Crop>,
}

impl SelectBallInner {
    pub fn init() -> Self {
        utils::set_panic_hook();
        utils::WasmLogger::init().unwrap();
        utils::WasmLogger::setup(log::LevelFilter::Trace);
        Self {
            image_ids: Vec::new(),
            dataset: Vec::new(),
            // 4 channels :
            //      0 -> top left
            //      1 -> top right
            //      2 -> bottom left
            //      3 -> bottom right
            crops_registered: vec![Vec::new(), Vec::new(), Vec::new(), Vec::new()],
            lobes_centers: vec![Vec::new(), Vec::new(), Vec::new(), Vec::new()],
            light_dirs: Vec::new(),
            light_sources: Vec::new(),
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
            }
        }

        Ok(())
    }

    // Run the main select_ball registration algorithm.
    //                                                 Vec<f32>
    async fn run(&mut self, params: JsValue) -> Result<JsValue, JsValue> {
        self.crops_registered[0].clear();
        self.crops_registered[1].clear();
        self.crops_registered[2].clear();
        self.crops_registered[3].clear();
        self.lobes_centers[0].clear();
        self.lobes_centers[1].clear();
        self.lobes_centers[2].clear();
        self.lobes_centers[3].clear();
        self.light_dirs.clear();
        self.light_sources.clear();
        let args: Args = params.into_serde().unwrap();
        utils::WasmLogger::setup(utils::verbosity_filter(args.config.verbosity));

        if self.dataset.is_empty() {
            self.crops_registered = vec![Vec::new(), Vec::new(), Vec::new(), Vec::new()];
            self.lobes_centers = vec![Vec::new(), Vec::new(), Vec::new(), Vec::new()];
        } else {
            let mut channel = 0;
            let nb_images: usize = self.dataset.len();

            for corner in crop_and_register(&args, &self.dataset).await {
                let mut list_light_vecs: Vec<Vector3<f32>> = Vec::new();
                let mut list_light_sources: Vec<Vector3<f32>> = Vec::new();
                log::info!("Register channel {}", channel);
                match corner {
                    None => {
                        self.lobes_centers[channel] = Vec::new();
                        //(1..(self.dataset.len())).map(|_| (0, 0)).collect();
                        self.crops_registered[channel] = Vec::new();
                    }
                    Some(centers_and_image_by_corner) => {
                        self.lobes_centers[channel] = centers_and_image_by_corner.0;
                        self.crops_registered[channel] = centers_and_image_by_corner.1;
                        for (vect, pt) in centers_and_image_by_corner.2.iter() {
                            list_light_sources.push(Vector3::new(pt.x, pt.y, pt.z));
                            list_light_vecs.push(Vector3::new(vect.x, vect.y, vect.z));
                        }
                    }
                };
                channel += 1;
                self.light_dirs.push(list_light_vecs);
                self.light_sources.push(list_light_sources);
            }

            self.light_dirs = transpose_records(&self.light_dirs, nb_images);
            self.light_sources = transpose_records(&self.light_sources, nb_images);
        }
        let false_vector: Vec<f32> = Vec::new();
        return JsValue::from_serde(&false_vector).map_err(utils::report_error);
    }

    // Return the ids of loaded images: [string]
    pub fn image_ids(&self) -> Result<JsValue, JsValue> {
        JsValue::from_serde(&self.image_ids).map_err(utils::report_error)
    }

    // Retrieve the cropped registered images.
    pub fn cropped_img_file(&self, i: usize, channel: usize) -> Result<Box<[u8]>, JsValue> {
        if self.crops_registered[channel].is_empty() {
            //let false_im = image::io::Reader::new(Cursor::new(buffer));
            let mut false_im = image::RgbImage::new(2, 2);
            false_im.put_pixel(0, 0, image::Rgb([255, 255, 0]));
            false_im.put_pixel(0, 1, image::Rgb([0, 0, 0]));
            false_im.put_pixel(1, 0, image::Rgb([0, 0, 0]));
            false_im.put_pixel(1, 1, image::Rgb([255, 255, 0]));
            // match false_im.decode() {
            //     Err(e) => Err(JsValue::from(e.to_string())),
            //     Ok(ok_im) => encode(i, &ok_im).map_err(utils::report_error),
            // }
            let false_im_rgb = DynamicImage::ImageRgb8(false_im);
            encode(i, &false_im_rgb).map_err(utils::report_error)
        } else {
            // The ith image in the channel
            encode(i, &self.crops_registered[channel][i]).map_err(utils::report_error)
        }
    }

    // Retrieve the centers of the ith image lobes.
    //                                      (u32, u32)
    pub fn lobes(&self, i: usize, channel: usize) -> JsValue {
        //JsValue::from_serde(&self.lobes_center[i]).map_err(utils::report_error)
        if self.lobes_centers[channel].is_empty() {
            let pt: Point2D = Point2D { x: 0, y: 0 };
            JsValue::from_serde(&pt).unwrap()
            // (vec![0, 0]).into_iter().map(JsValue::from).collect()
        } else {
            let c: (u32, u32) = self.lobes_centers[channel][i];
            let pt: Point2D = Point2D { x: c.0, y: c.1 };
            JsValue::from_serde(&pt).unwrap()
            //  (vec![c.0, c.1])
            //  .into_iter()
            //  .map(JsValue::from)
            //  .collect()
        }
    }

    // Register and save that image.
    pub fn register_and_save(&self, i: usize) -> Result<Box<[u8]>, JsValue> {
        log::info!("Registering image {}", i);
        return encode(i, &self.dataset[i]).map_err(utils::report_error);
    }

    // Send the led position in 3D
    pub fn light_pos(&self, i: usize) -> JsValue {
        let lobes_in_img: &Vec<Vector3<f32>> = &self.light_sources[i];
        let rays: &Vec<Vector3<f32>> = &self.light_dirs[i];
        let vect: Vector3<f32> = match intersection(lobes_in_img, rays) {
            Some(v) => v,
            None => Vector3::new(0., 0., 0.),
        };

        let pt: Point3D = Point3D {
            x: vect.x,
            y: vect.y,
            z: vect.z,
        };
        log::info!("Light point : x : {}, y : {}, z : {}", pt.x, pt.y, pt.z);
        JsValue::from_serde(&pt).unwrap()
    }

    // Send the mean light direction
    pub fn light_vector(&self, i: usize) -> JsValue {
        let rays: &Vec<Vector3<f32>> = &self.light_dirs[i];
        let mut mean_ray: Vector3<f32> = rays.iter().sum();
        mean_ray.normalize_mut();
        let vect: Point3D = Point3D {
            x: mean_ray.x,
            y: mean_ray.y,
            z: mean_ray.z,
        };
        JsValue::from_serde(&vect).unwrap()
    }
}

/// https://stackoverflow.com/questions/29669287/how-can-i-zip-more-than-two-iterators
/// Transposes an N-sized vector of M-sized vectors to an M-sized vector of N-sized vectors.
/// Pretty much like matlab would with a vcat/hacat-ed matrix.
fn transpose_records<T: Clone>(records: &Vec<Vec<T>>, nb_images: usize) -> Vec<Vec<T>> {
    let mut transposed: Vec<Vec<T>> = vec![Vec::new(); nb_images]; //records[0].len()];

    for record in records {
        for (index, element) in record.iter().enumerate() {
            transposed[index].push(element.clone());
        }
    }

    transposed
}

fn encode(i: usize, mat: &DynamicImage) -> anyhow::Result<Box<[u8]>> {
    log::debug!("Encoding image {}", i);
    // let img = mat.to_image();
    let img = mat;
    let mut buffer: Vec<u8> = Vec::new();
    img.write_to(&mut buffer, image::ImageOutputFormat::Png)?;
    Ok(buffer.into_boxed_slice())
}

async fn maxes(
    crop: Option<Crop>,
    og_imgs: &[DynamicImage],
    sigma: f32,
    threshold: f32,
    mask_ray: f32,
) -> Option<(Vec<(u32, u32)>, Vec<DynamicImage>, Vec<(Point3D, Point3D)>)> {
    let mut max_coords: Vec<(u32, u32)> = Vec::new();
    let mut light_vec_and_source: Vec<(Point3D, Point3D)> = Vec::new();
    let final_imgs: Vec<DynamicImage> = match crop {
        None => {
            log::info!("==== (No crop here)");
            return None;
        }
        Some(frame) => {
            log::info!("==== Cropping images...");
            og_imgs
                .iter()
                .map(|im| {
                    log::info!("==== Compute ray...");
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
                    log::info!("==== Compute true crop frame.");
                    let true_left: u32 = (center_x - ray).round() as u32;
                    let true_top: u32 = (center_y - ray).round() as u32;
                    let true_right: u32 = (center_x + ray).round() as u32;
                    let true_bottom: u32 = (center_y + ray).round() as u32;
                    let nalgebra_center: Vector2<i32> = Vector2::new(
                        (center_x as i32) - (true_left as i32),
                        (center_y as i32) - (true_top as i32),
                    );
                    let nalgebra_sphere_pos: Vector2<i32> =
                        Vector2::new(true_left as i32, true_top as i32);
                    let cropped = im.crop_imm(
                        true_left,
                        true_top,
                        true_right - true_left,
                        true_bottom - true_top,
                    );
                    log::info!("==== Cropped OK");
                    let image_blured = cropped.blur(sigma).to_rgb8();
                    log::info!("==== Blured ok");
                    let mut pixel_list: Vec<((u32, u32), f32)> = Vec::new();
                    let only_ball = ImageBuffer::from_fn(
                        image_blured.width(),
                        image_blured.height(),
                        |x, y| {
                            let dx: f32 = (x + true_left) as f32 - center_x;
                            let dy: f32 = (y + true_top) as f32 - center_y;
                            if (dx * dx + dy * dy) <= ray * ray * mask_ray * mask_ray {
                                let px = image_blured[(x, y)];
                                let intensity: f32 = px.to_luma()[0] as f32;
                                if intensity > 255.0 * threshold {
                                    pixel_list.push(((x, y), intensity));
                                    px
                                } else {
                                    image::Rgb([50u8, 50u8, 50u8])
                                }
                            } else {
                                image::Rgb([255u8, 255u8, 255u8])
                            }
                        },
                    );
                    log::info!("==== Mask applied");
                    let mut mean_pixel: (u32, u32) = (0, 0);
                    let mut total_weight: f32 = 0.0;
                    for px in &pixel_list {
                        mean_pixel = (
                            mean_pixel.0 + (px.0 .0 as f32 * px.1).round() as u32,
                            mean_pixel.1 + (px.0 .1 as f32 * px.1).round() as u32,
                        );
                        total_weight += px.1;
                    }
                    mean_pixel = (
                        (mean_pixel.0 as f32 / total_weight).round() as u32,
                        (mean_pixel.1 as f32 / total_weight).round() as u32,
                    );
                    max_coords.push(mean_pixel);
                    let nalgebra_light_bulb: Vector2<i32> =
                        Vector2::new(mean_pixel.0 as i32, mean_pixel.1 as i32);

                    let result_light_dir = light_dir(
                        ray as i32,
                        nalgebra_light_bulb,
                        nalgebra_center,
                        nalgebra_sphere_pos,
                    );

                    light_vec_and_source.push((
                        Point3D {
                            x: result_light_dir.0.x,
                            y: result_light_dir.0.y,
                            z: result_light_dir.0.z,
                        },
                        Point3D {
                            x: result_light_dir.1.x,
                            y: result_light_dir.1.y,
                            z: result_light_dir.1.z,
                        },
                    ));

                    DynamicImage::ImageRgb8(only_ball)
                })
                .collect()
        }
    };
    Some((max_coords, final_imgs, light_vec_and_source))
}

#[allow(clippy::type_complexity)]
async fn crop_and_register(
    args: &Args,
    og_imgs: &[DynamicImage],
) -> Vec<Option<(Vec<(u32, u32)>, Vec<DynamicImage>, Vec<(Point3D, Point3D)>)>> {
    // Extract the cropped area from the images.
    let crop_t_l: Option<Crop> = args.crop_t_l;
    let crop_t_r: Option<Crop> = args.crop_t_r;
    let crop_b_l: Option<Crop> = args.crop_b_l;
    let crop_b_r: Option<Crop> = args.crop_b_r;
    log::info!("Top left crop ---");
    let top_left_centers: Option<(Vec<(u32, u32)>, Vec<DynamicImage>, Vec<(Point3D, Point3D)>)> =
        maxes(
            crop_t_l,
            og_imgs,
            args.config.sigma,
            args.config.sigma,
            args.config.mask_ray,
        )
        .await;
    log::info!("Top right crop ---");
    let top_right_centers: Option<(Vec<(u32, u32)>, Vec<DynamicImage>, Vec<(Point3D, Point3D)>)> =
        maxes(
            crop_t_r,
            og_imgs,
            args.config.sigma,
            args.config.sigma,
            args.config.mask_ray,
        )
        .await;
    log::info!("Bottom left crop ---");
    let bottom_left_centers: Option<(Vec<(u32, u32)>, Vec<DynamicImage>, Vec<(Point3D, Point3D)>)> =
        maxes(
            crop_b_l,
            og_imgs,
            args.config.sigma,
            args.config.sigma,
            args.config.mask_ray,
        )
        .await;
    log::info!("Bottom right crop ---");
    let bottom_right_centers: Option<(
        Vec<(u32, u32)>,
        Vec<DynamicImage>,
        Vec<(Point3D, Point3D)>,
    )> = maxes(
        crop_b_r,
        og_imgs,
        args.config.sigma,
        args.config.sigma,
        args.config.mask_ray,
    )
    .await;
    log::info!("All computed, sending results to web page. ---");
    // 4 channels :
    //      0 -> top left
    //      1 -> top right
    //      2 -> bottom left
    //      3 -> bottom right
    vec![
        top_left_centers,
        top_right_centers,
        bottom_left_centers,
        bottom_right_centers,
    ]
}

// async fn should_stop_bool(step: &str, progress: Option<u32>) -> bool {
//     let js_bool = should_stop(step, progress).await;
//     js_bool.as_bool().unwrap()
// }
//
// fn original_motion(crop: Option<Crop>, motion_vec_crop: Vec<Vector6<f32>>) -> Vec<Vector6<f32>> {
//     // Recover motion parameters in the frame of the full image from the one in the cropped frame.
//     match crop {
//         None => motion_vec_crop,
//         Some(frame) => recover_original_motion(frame, &motion_vec_crop),
//     }
// }
//
// fn into_gray_u8(m: DMatrix<u16>) -> DMatrix<u8> {
//     m.map(|x| (x / 256) as u8)
// }
