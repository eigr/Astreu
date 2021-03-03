extern crate wasm_bindgen;
extern crate bytes;

use core::slice;
use bytes::Bytes;
use wasm_bindgen::prelude::*;

pub mod astreu {
    include!(concat!(env!("OUT_DIR"), "/astreu.protocol.rs"));
}

#[wasm_bindgen]
pub fn version(bytes: *const u8) -> *const u8 {
    let slice = unsafe { slice::from_raw_parts(bytes, 8) };

    let mut mem = Bytes::from(slice);
    b"Hello, World!\0".as_ptr()
}