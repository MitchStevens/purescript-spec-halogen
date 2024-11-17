// Generated by purs version 0.15.15
import * as $foreign from "./foreign.js";
import * as Type_Proxy from "../Type.Proxy/index.js";
var reifiableString = {};
var reifiableOrdering = {};
var reifiableInt = {};
var reifiableBoolean = {};
var reifyType = function () {
    return function (s) {
        return function (f) {
            return $foreign.unsafeCoerce(function (dictReflectable) {
                return f(dictReflectable);
            })({
                reflectType: function (v) {
                    return s;
                }
            })(Type_Proxy["Proxy"].value);
        };
    };
};
var reflectType = function (dict) {
    return dict.reflectType;
};
export {
    reflectType,
    reifyType,
    reifiableBoolean,
    reifiableInt,
    reifiableOrdering,
    reifiableString
};
//# sourceMappingURL=index.js.map
