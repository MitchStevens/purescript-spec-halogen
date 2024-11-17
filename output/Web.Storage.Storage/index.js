// Generated by purs version 0.15.15
import * as $foreign from "./foreign.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Nullable from "../Data.Nullable/index.js";
import * as Effect from "../Effect/index.js";
var map = /* #__PURE__ */ Data_Functor.map(Effect.functorEffect);
var key = function (i) {
    var $2 = map(Data_Nullable.toMaybe);
    var $3 = $foreign["_key"](i);
    return function ($4) {
        return $2($3($4));
    };
};
var getItem = function (s) {
    var $5 = map(Data_Nullable.toMaybe);
    var $6 = $foreign["_getItem"](s);
    return function ($7) {
        return $5($6($7));
    };
};
export {
    length,
    setItem,
    removeItem,
    clear
} from "./foreign.js";
export {
    key,
    getItem
};
//# sourceMappingURL=index.js.map