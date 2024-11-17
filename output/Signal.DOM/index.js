// Generated by purs version 0.15.15
import * as $foreign from "./foreign.js";
import * as Signal from "../Signal/index.js";
import * as Signal_Time from "../Signal.Time/index.js";
var flippedMap = /* #__PURE__ */ Signal.flippedMap(Signal.functorSignal);
var MouseLeftButton = /* #__PURE__ */ (function () {
    function MouseLeftButton() {

    };
    MouseLeftButton.value = new MouseLeftButton();
    return MouseLeftButton;
})();
var MouseMiddleButton = /* #__PURE__ */ (function () {
    function MouseMiddleButton() {

    };
    MouseMiddleButton.value = new MouseMiddleButton();
    return MouseMiddleButton;
})();
var MouseIE8MiddleButton = /* #__PURE__ */ (function () {
    function MouseIE8MiddleButton() {

    };
    MouseIE8MiddleButton.value = new MouseIE8MiddleButton();
    return MouseIE8MiddleButton;
})();
var MouseRightButton = /* #__PURE__ */ (function () {
    function MouseRightButton() {

    };
    MouseRightButton.value = new MouseRightButton();
    return MouseRightButton;
})();
var windowDimensions = /* #__PURE__ */ $foreign.windowDimensionsP(Signal.constant);
var touch = /* #__PURE__ */ $foreign.touchP(Signal.constant);
var tap = function __do() {
    var touches = touch();
    return flippedMap(touches)(function (t) {
        if (t.length === 0) {
            return false;
        };
        return true;
    });
};
var mousePos = /* #__PURE__ */ $foreign.mousePosP(Signal.constant);
var mouseButton = /* #__PURE__ */ $foreign.mouseButtonP(Signal.constant);
var mouseButtonPressed = function (btn) {
    var buttonNumber = (function () {
        if (btn instanceof MouseLeftButton) {
            return 0;
        };
        if (btn instanceof MouseRightButton) {
            return 2;
        };
        if (btn instanceof MouseMiddleButton) {
            return 1;
        };
        if (btn instanceof MouseIE8MiddleButton) {
            return 4;
        };
        throw new Error("Failed pattern match at Signal.DOM (line 48, column 20 - line 52, column 32): " + [ btn.constructor.name ]);
    })();
    return mouseButton(buttonNumber);
};
var keyPressed = /* #__PURE__ */ $foreign.keyPressedP(Signal.constant);
var animationFrame = /* #__PURE__ */ $foreign.animationFrameP(Signal.constant)(Signal_Time.now);
export {
    animationFrame,
    keyPressed,
    mouseButton,
    mouseButtonPressed,
    touch,
    tap,
    mousePos,
    windowDimensions,
    MouseLeftButton,
    MouseMiddleButton,
    MouseIE8MiddleButton,
    MouseRightButton
};
//# sourceMappingURL=index.js.map