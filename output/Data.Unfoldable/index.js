// Generated by purs version 0.15.15
import * as $foreign from "./foreign.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Traversable from "../Data.Traversable/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unfoldable1 from "../Data.Unfoldable1/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
var map = /* #__PURE__ */ Data_Functor.map(Data_Maybe.functorMaybe);
var fromJust = /* #__PURE__ */ Data_Maybe.fromJust();
var unfoldr = function (dict) {
    return dict.unfoldr;
};
var unfoldableMaybe = {
    unfoldr: function (f) {
        return function (b) {
            return map(Data_Tuple.fst)(f(b));
        };
    },
    Unfoldable10: function () {
        return Data_Unfoldable1.unfoldable1Maybe;
    }
};
var unfoldableArray = {
    unfoldr: /* #__PURE__ */ $foreign.unfoldrArrayImpl(Data_Maybe.isNothing)(fromJust)(Data_Tuple.fst)(Data_Tuple.snd),
    Unfoldable10: function () {
        return Data_Unfoldable1.unfoldable1Array;
    }
};
var replicate = function (dictUnfoldable) {
    var unfoldr1 = unfoldr(dictUnfoldable);
    return function (n) {
        return function (v) {
            var step = function (i) {
                var $17 = i <= 0;
                if ($17) {
                    return Data_Maybe.Nothing.value;
                };
                return new Data_Maybe.Just(new Data_Tuple.Tuple(v, i - 1 | 0));
            };
            return unfoldr1(step)(n);
        };
    };
};
var replicateA = function (dictApplicative) {
    return function (dictUnfoldable) {
        var replicate1 = replicate(dictUnfoldable);
        return function (dictTraversable) {
            var sequence = Data_Traversable.sequence(dictTraversable)(dictApplicative);
            return function (n) {
                return function (m) {
                    return sequence(replicate1(n)(m));
                };
            };
        };
    };
};
var none = function (dictUnfoldable) {
    return unfoldr(dictUnfoldable)(Data_Function["const"](Data_Maybe.Nothing.value))(Data_Unit.unit);
};
var fromMaybe = function (dictUnfoldable) {
    return unfoldr(dictUnfoldable)(function (b) {
        return map(Data_Function.flip(Data_Tuple.Tuple.create)(Data_Maybe.Nothing.value))(b);
    });
};
export {
    unfoldr,
    replicate,
    replicateA,
    none,
    fromMaybe,
    unfoldableArray,
    unfoldableMaybe
};
export {
    iterateN,
    range,
    replicate1,
    replicate1A,
    singleton,
    unfoldr1
} from "../Data.Unfoldable1/index.js";
//# sourceMappingURL=index.js.map
