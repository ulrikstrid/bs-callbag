// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");

function interval(period, _type) {
  var num = [0];
  if (typeof _type === "number") {
    return /* () */0;
  } else if (_type.tag) {
    return /* () */0;
  } else {
    var sink = _type[0];
    var intervalId = setInterval((function () {
            Curry._1(sink, /* Data */Block.__(1, [num[0]]));
            num[0] = num[0] + 1 | 0;
            return /* () */0;
          }), period);
    return Curry._1(sink, /* Start */Block.__(0, [(function (t) {
                      if (typeof t === "number") {
                        clearInterval(intervalId);
                        return /* () */0;
                      } else {
                        return /* () */0;
                      }
                    })]));
  }
}

function fromPromise(source, _type) {
  if (typeof _type === "number") {
    return /* () */0;
  } else if (_type.tag) {
    return /* () */0;
  } else {
    var sink = _type[0];
    source.then((function (a) {
            Curry._1(sink, /* Data */Block.__(1, [a]));
            return Promise.resolve(/* () */0);
          }));
    return /* () */0;
  }
}

exports.interval    = interval;
exports.fromPromise = fromPromise;
/* No side effect */