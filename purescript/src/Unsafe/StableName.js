"use strict";

// module Unsafe.StableName

var id = 0;
const wm = new WeakMap();

exports.unsafeNameEq = function (x, y) {
  return x === y;
};

exports.unsafeNameHash = function (x) {
  return wm[x];
};

exports.makeStableName = function (x) {
  if (x in wm) return x;
  wm[x] = id + 1;
  id = id + 1;
  return x;
};