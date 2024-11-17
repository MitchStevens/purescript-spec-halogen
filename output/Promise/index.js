// Generated by purs version 0.15.15
import * as Effect_Uncurried from "../Effect.Uncurried/index.js";
import * as Promise_Internal from "../Promise.Internal/index.js";
import * as Promise_Rejection from "../Promise.Rejection/index.js";
var then_ = function () {
    return function (k) {
        return function (p) {
            return function () {
                return Promise_Internal.then_(Effect_Uncurried.mkEffectFn1(k), p);
            };
        };
    };
};
var thenOrCatch = function () {
    return function (k) {
        return function (c) {
            return function (p) {
                return function () {
                    return Promise_Internal.thenOrCatch(Effect_Uncurried.mkEffectFn1(k), Effect_Uncurried.mkEffectFn1(c), p);
                };
            };
        };
    };
};
var resolve = function () {
    return Promise_Internal.resolve;
};
var race = /* #__PURE__ */ Effect_Uncurried.runEffectFn1(Promise_Internal.race);
var $$new = function () {
    return function (k) {
        return function () {
            return Promise_Internal["new"](function (onResolve, onReject) {
                return k(Effect_Uncurried.runEffectFn1(onResolve))(Effect_Uncurried.runEffectFn1(onReject))();
            });
        };
    };
};
var flattenPromise = function () {
    return {};
};
var flattenDone = {};
var $$finally = /* #__PURE__ */ Effect_Uncurried.runEffectFn2(Promise_Internal["finally"]);
var $$catch = function (k) {
    return function (p) {
        return function () {
            return Promise_Internal["catch"](Effect_Uncurried.mkEffectFn1(k), p);
        };
    };
};
var all = /* #__PURE__ */ Effect_Uncurried.runEffectFn1(Promise_Internal.all);
export {
    all,
    $$catch as catch,
    $$finally as finally,
    $$new as new,
    race,
    resolve,
    thenOrCatch,
    then_,
    flattenPromise,
    flattenDone
};
export {
    reject
} from "../Promise.Internal/index.js";
//# sourceMappingURL=index.js.map
