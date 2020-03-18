// https://d3js.org/d3-fetch/ v1.1.2 Copyright 2018 Mike Bostock
(function (global, factory) {
typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports, require('d3-dsv')) :
typeof define === 'function' && define.amd ? define(['exports', 'd3-dsv'], factory) :
(factory((global.d3 = global.d3 || {}),global.d3));
}(this, (function (exports,d3Dsv) { 'use strict';

function responseBlob(response) {
  if (!response.ok) throw new Error(response.status + " " + response.statusText);
  return response.blob();
}

function blob(input, init) {
  return fetch(input, init).then(responseBlob);
}

function responseArrayBuffer(response) {
  if (!response.ok) throw new Error(response.status + " " + response.statusText);
  return response.arrayBuffer();
}

function buffer(input, init) {
  return fetch(input, init).then(responseArrayBuffer);
}

function responseText(response) {
  if (!response.ok) throw new Error(response.status + " " + response.statusText);
  return response.text();
}

function text(input, init) {
  return fetch(input, init).then(responseText);
}

function dsvParse(parse) {
  return function(input, init, row) {
    if (arguments.length === 2 && typeof init === "function") row = init, init = undefined;
    return text(input, init).then(function(response) {
      return parse(response, row);
    });
  };
}

function dsv(delimiter, input, init, row) {
  if (arguments.length === 3 && typeof init === "function") row = init, init = undefined;
  var format = d3Dsv.dsvFormat(delimiter);
  return text(input, init).then(function(response) {
    return format.parse(response, row);
  });
}

var csv = dsvParse(d3Dsv.csvParse);
var tsv = dsvParse(d3Dsv.tsvParse);

function image(input, init) {
  return new Promise(function(resolve, reject) {
    var image = new Image;
    for (var key in init) image[key] = init[key];
    image.onerror = reject;
    image.onload = function() { resolve(image); };
    image.src = input;
  });
}

function responseJson(response) {
  if (!response.ok) throw new Error(response.status + " " + response.statusText);
  return response.json();
}

function json(input, init) {
  return fetch(input, init).then(responseJson);
}

function parser(type) {
  return function(input, init)  {
    return text(input, init).then(function(text$$1) {
      return (new DOMParser).parseFromString(text$$1, type);
    });
  };
}

var xml = parser("application/xml");

var html = parser("text/html");

var svg = parser("image/svg+xml");

exports.blob = blob;
exports.buffer = buffer;
exports.dsv = dsv;
exports.csv = csv;
exports.tsv = tsv;
exports.image = image;
exports.json = json;
exports.text = text;
exports.xml = xml;
exports.html = html;
exports.svg = svg;

Object.defineProperty(exports, '__esModule', { value: true });

})));
