"use strict";

exports.parseImpl =
function(left) {
  return function(right) {
    return function(s) {
      try {
        return right( require('webidl2').parse(s) );
      }
      catch(err) {
        return left( err.toString() );
      }
    };
  }
}

exports.parseStringify = function(s) {
  try {
    return JSON.stringify( require('webidl2').parse(s) );
  }
  catch(err) {
    return ""
  }
};
