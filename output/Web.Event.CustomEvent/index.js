// Generated by purs version 0.15.15
import * as $foreign from "./foreign.js";
import * as Data_Nullable from "../Data.Nullable/index.js";
import * as Unsafe_Coerce from "../Unsafe.Coerce/index.js";
import * as Web_Internal_FFI from "../Web.Internal.FFI/index.js";
var toEvent = Unsafe_Coerce.unsafeCoerce;
var newWithOptions = function (ty) {
    return function (v) {
        return $foreign.newOptionsImpl(ty)({
            detail: Data_Nullable.toNullable(v.detail),
            bubbles: v.bubbles,
            cancelable: v.cancelable,
            composed: v.composed
        });
    };
};
var new$prime = function (ty) {
    return function (det) {
        return newWithOptions(ty)({
            detail: det,
            bubbles: false,
            cancelable: false,
            composed: false
        });
    };
};
var fromEvent = /* #__PURE__ */ Web_Internal_FFI.unsafeReadProtoTagged("CustomEvent");
export {
    new,
    newOptionsImpl,
    detail
} from "./foreign.js";
export {
    fromEvent,
    toEvent,
    new$prime,
    newWithOptions
};
//# sourceMappingURL=index.js.map
