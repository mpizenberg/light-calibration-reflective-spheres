// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/

// Import and initialize the WebAssembly module.
// Remark: ES modules are not supported in Web Workers,
// so you have to process this file with esbuild:
// esbuild worker.mjs --bundle --preserve-symlinks --outfile=worker.js
import { SelectBall as SelectBallWasm, default as init } from "./pkg/select_ball_wasm.js";

// Initialize the wasm module.
// Let's hope this finishes before someone needs to call a SelectBall method.
let SelectBall;
(async function () {
  await init("./pkg/select_ball_wasm_bg.wasm");
  SelectBall = SelectBallWasm.init();
})();

// Global module variable recording if the algorithm was asked to stop.
let stopOrder = false;

console.log("Hello from worker");

// Listener for messages containing data of the shape: { type, data }
// where type can be one of:
//   - "decode-image": decode an image provided with its url
//   - "run": run the algorithm on all images
//   - "stop": stop the alogorithm
onmessage = async function (event) {
  console.log(`worker message: ${event.data.type}`);
  if (event.data.type == "decode-image") {
    await decode(event.data.data);
    postMessage({ type: "image-decoded", data: event.data.data });
  } else if (event.data.type == "run") {
    await run(event.data.data);
  } else if (event.data.type == "warp-encode") {
    await warpEncode(event.data.data);
  } else if (event.data.type == "stop") {
    console.log("Received STOP in worker");
    stopOrder = true;
  }
};

// Load image into wasm memory and decode it.
async function decode({ id, url }) {
  console.log("Loading into wasm: " + id);
  const response = await fetch(url);
  const arrayBuffer = await response.arrayBuffer();
  SelectBall.load(id, new Uint8Array(arrayBuffer));
}

// Main algorithm with the parameters passed as arguments.
async function run(params) {
  console.log("worker running with parameters:", params);
  // Convert params to what is expected by the Rust code.
  const args = {
    config: {
	  sigma: params.sigma,
      threshold: params.threashold,
      verbosity: params.maxVerbosity,
	  mask_ray: params.maskRay,
    },
    equalize: 0.5,
    crop_t_l: params.crops.topLeft,
    crop_t_r: params.crops.topRight,
    crop_b_l: params.crops.bottomLeft,
    crop_b_r: params.crops.bottomRight,
  };

  // Run lowrr main registration algorithm.
  stopOrder = false;
  let motion = await SelectBall.run(args);

  // Send back to main thread all cropped images.
  const image_ids = SelectBall.image_ids();
  const imgCount = image_ids.length;
  console.log(`Encoding ${imgCount} cropped aligned images:`);
  for (let i = 0; i < imgCount; i++) {
    await shouldStop("encoding", i);
    const id = image_ids[i];
    console.log("   Encoding ", id, " ...");
    let croppedImgArrayU8_TL = SelectBall.cropped_img_file(i, 0);
    let croppedImgArrayU8_TR = SelectBall.cropped_img_file(i, 1);
    let croppedImgArrayU8_BL = SelectBall.cropped_img_file(i, 2);
    let croppedImgArrayU8_BR = SelectBall.cropped_img_file(i, 3);
	let lobes_centers_TL = SelectBall.lobes(i, 0);
	let lobes_centers_TR = SelectBall.lobes(i, 1);
	let lobes_centers_BL = SelectBall.lobes(i, 2);
	let lobes_centers_BR = SelectBall.lobes(i, 3);
	let light_pos = SelectBall.light_pos(i);
	let light_dir = SelectBall.light_vector(i);
	console.log("Lighting direction :\nx: ", light_dir.x, "\ny: ", light_dir.y, "\nz: ", light_dir.z);
	console.log("Point of lighting :\nx: ", light_pos.x, "\ny: ", light_pos.y, "\nz: ", light_pos.z);

    // Transfer the array buffer back to main thread.
    postMessage(
      {
        type: "cropped-image",
        data: { id, arrayBuffers: { TL: croppedImgArrayU8_TL.buffer, TR: croppedImgArrayU8_TR.buffer, BL: croppedImgArrayU8_BL.buffer, BR: croppedImgArrayU8_BR }, imgCount, lobes_centers: { TL: lobes_centers_TL, TR: lobes_centers_TR, BL: lobes_centers_BL, BR: lobes_centers_BR }, light_pos: light_pos, light_dir: light_dir },
      },
      [croppedImgArrayU8_TL.buffer, croppedImgArrayU8_TR.buffer, croppedImgArrayU8_BL.buffer, croppedImgArrayU8_BR.buffer]
    );
  }
  await shouldStop("done", null);
}

// Warp and encode images that have just been registered.
async function warpEncode({ imgCount }) {
  stopOrder = false;
  console.log("Warping and encoding registered images");
  for (let i = 0; i < imgCount; i++) {
    if (await shouldStop("saving", i)) {
      // If the user asked to stop the saving of images,
      // reset the runStep state to "done" and stop.
      appLog(0, "Saving stopped by user");
      await shouldStop("done", null);
      break;
    }
    // Warp and encode image in wasm.
    let imgArrayU8 = SelectBall.register_and_save(i);
    // Transfer the array buffer back to main thread.
    postMessage(
      {
        type: "registered-image",
        data: { index: i, arrayBuffer: imgArrayU8.buffer, imgCount },
      },
      [imgArrayU8.buffer]
    );
  }
  await shouldStop("done", null);
}

// Log something in the interface with the provided verbosity level.
export function appLog(lvl, content) {
  postMessage({ type: "log", data: { lvl, content } });
}

// Function regularly called in the algorithm to check if it should stop.
export async function shouldStop(step, progress) {
  postMessage({ type: "should-stop", data: { step, progress } });
  await sleep(0); // Force to give control back.
  return stopOrder;
}

// Small utility function.
function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
