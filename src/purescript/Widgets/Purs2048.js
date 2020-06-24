"use strict";
//
//  Attribution: the content of this file was taken on June 24, 2020 from the Concur React repository:
//  - https://github.com/purescript-concur/purescript-concur-react/blob/master/examples/src/Test/Keyboard.js
//

var observer = null;

function innerHandler(event) {
    event.preventDefault();
    if(observer) observer(event);
}

// Start listening for keys
// :: Effect Unit
exports.startListening = function() {
    document.addEventListener('keydown', innerHandler);
};

// Stop listening for keys
// :: Effect Unit
exports.stopListening = function() {
    document.removeEventListener('keydown', innerHandler);
};

// Await a key
// :: EffectFnAff KeyEvent
exports._awaitKey = function (onError, onSuccess) {
    observer = onSuccess;
    return function (cancelError, onCancelerError, onCancelerSuccess) {
        onCancelerSuccess();
    };
};