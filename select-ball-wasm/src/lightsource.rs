//! Computes the light source given the calibration ball center,
//! its radius, and the light bulb position.
//! The computations are made in the referencial frame of the crop around the ball, and
//! fully in the carthesian coordinates system of the pixels positions.
//! One big approximation is made right now : it is assumed that the viewer position is right on
//! top of the ball, so that the vector \vec{v} is [0, 0, 1].

use nalgebra::{Matrix3, Vector2, Vector3};

/// computes the light direction
pub fn light_dir(
    radius: i32,
    light_bulb: Vector2<i32>,
    center: Vector2<i32>,
    sphere_pos: Vector2<i32>,
) -> (Vector3<f32>, Vector3<f32>) {
    // In matlab code :
    // xy -> light_bulb - center
    // rad -> center
    // points -> [source_point...]
    // spheres_pos -> [sphere_pos...]
    // All 3D points are considered as 3D vectors (as matlab doesn't care about the difference).
    // Origin of the frame = topleft of the crop
    let xy: Vector2<i32> = light_bulb - center;
    log::info!("ray -> {}", radius);
    log::info!("light_bulb -> {}|{}", light_bulb.x, light_bulb.y);
    log::info!("center -> {}|{}", center.x, center.y);
    log::info!("xy -> {}|{}", xy.x, xy.y);
    log::info!("Debug : {} - {}", radius * radius, xy.dot(&xy));
    let z: f32 = ((radius * radius - xy.dot(&xy)) as f32).sqrt();
    let spot: Vector3<f32> = Vector3::new(xy.x as f32, xy.y as f32, z);
    log::info!("spot -> {}|{}|{}", spot.x, spot.y, spot.z);
    let n: Vector3<f32> = spot / (radius as f32);
    let v: Vector3<f32> = Vector3::z(); // orthographic approx
    let mut light_ray: Vector3<f32> = 2.0 * n.dot(&v) * n - v;
    light_ray.normalize_mut();
    log::info!(
        "light_ray -> {}|{}|{}",
        light_ray.x,
        light_ray.y,
        light_ray.z
    );
    let point: Vector3<f32> = spot + Vector3::new(sphere_pos.x as f32, sphere_pos.y as f32, 0.0);
    (light_ray, point)
}

/// computes the 3D intersection of rays
pub fn intersection(
    lobes_in_img: &Vec<Vector3<f32>>,
    rays: &Vec<Vector3<f32>>,
) -> Option<Vector3<f32>> {
    // In matlab code :
    // S -> s
    // C -> c
    // points -> lobes_in_img
    // All 3D points are considered as 3D vectors (as matlab doesn't care about the difference).
    // Origin of the frame = topleft of the image
    let mut s: Matrix3<f32> = Matrix3::zeros();
    let mut c: Vector3<f32> = Vector3::zeros();
    let mut delta_s: Matrix3<f32>;
    // We hope that len(rays) == len(lobes_in_img)
    for (ray, point) in rays.iter().zip(lobes_in_img.iter()) {
        delta_s = ray * ray.transpose() - Matrix3::identity();
        s += delta_s;
        c += delta_s * point;
    }
    // Solve intersection = S \ C
    let decomp = s.lu();
    decomp.solve(&c) // <- intersection
}
