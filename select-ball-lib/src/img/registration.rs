// SPDX-License-Identifier: MPL-2.0

//! Registration algorithm for a sequence of slightly misaligned images.

use image::Primitive;
use nalgebra::{DMatrix, Matrix6, Scalar, Vector6};
use std::ops::Add;
use thiserror::Error;

use crate::img::interpolation::CanLinearInterpolate;
use crate::interop::ToImage;

#[cfg(feature = "wasm-bindgen")]
use wasm_bindgen::prelude::*;

#[cfg(feature = "serde")]
use serde::Deserialize;

/// Configuration (parameters) of the registration algorithm.
#[cfg_attr(feature = "wasm-bindgen", wasm_bindgen)]
#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(Deserialize))]
pub struct Config {
    pub sigma: f32,
    pub threshold: f32,
    pub mask_ray: f32,
    pub verbosity: u32,
}

// --/// Type alias just to semantically differenciate Vec<Levels<_>> and Levels<Vec<_>>.
// --type Levels<T> = Vec<T>;

/// Trait for types that implement all the necessary stuff in order
/// to do registration on matrices of that type.
/// Basically u8 and u16 (only gray images supported for now).
pub trait CanRegister:
    Copy
    + Scalar
    + Primitive
    + crate::img::viz::IntoRgb8
    + crate::img::multires::Bigger
    + crate::img::gradients::Bigger<<Self as CanRegister>::Bigger>
    + CanLinearInterpolate<f32, f32>
    + CanLinearInterpolate<f32, Self>
where
    DMatrix<Self>: ToImage,
{
    type Bigger: Scalar + Copy + PartialOrd + Add<Output = Self::Bigger>;
}

impl CanRegister for u8 {
    type Bigger = u16;
}
impl CanRegister for u16 {
    type Bigger = u32;
}

#[derive(Error, Debug)]
pub enum RegistrationError {
    #[error("The algorithm was stopped by the caller")]
    StoppedByCaller,
    #[error("Error while trying to inverse the motion of the reference image: {0}")]
    InverseRefMotion(Vector6<f32>),
    #[error("Not enough pixels to perform a direct image alignment estimation: {0}")]
    NotEnoughPoints(u32),
    #[error("The Hessian matrix computed for the direct alignment is not definite positive so its Choleski decomposition failed: {0}")]
    NonDefinitePositiveHessian(Matrix6<f32>),
}
