// Generated by purs version 0.15.15
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Comparison from "../Data.Comparison/index.js";
import * as Data_Equivalence from "../Data.Equivalence/index.js";
import * as Data_Op from "../Data.Op/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Predicate from "../Data.Predicate/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
var append = /* #__PURE__ */ Data_Semigroup.append(Data_Ordering.semigroupOrdering);
var identity = /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn);
var dividePredicate = {
    divide: function (f) {
        return function (v) {
            return function (v1) {
                return function (a) {
                    var v2 = f(a);
                    return v(v2.value0) && v1(v2.value1);
                };
            };
        };
    },
    Contravariant0: function () {
        return Data_Predicate.contravariantPredicate;
    }
};
var divideOp = function (dictSemigroup) {
    var append1 = Data_Semigroup.append(dictSemigroup);
    return {
        divide: function (f) {
            return function (v) {
                return function (v1) {
                    return function (a) {
                        var v2 = f(a);
                        return append1(v(v2.value0))(v1(v2.value1));
                    };
                };
            };
        },
        Contravariant0: function () {
            return Data_Op.contravariantOp;
        }
    };
};
var divideEquivalence = {
    divide: function (f) {
        return function (v) {
            return function (v1) {
                return function (a) {
                    return function (b) {
                        var v2 = f(a);
                        var v3 = f(b);
                        return v(v2.value0)(v3.value0) && v1(v2.value1)(v3.value1);
                    };
                };
            };
        };
    },
    Contravariant0: function () {
        return Data_Equivalence.contravariantEquivalence;
    }
};
var divideComparison = {
    divide: function (f) {
        return function (v) {
            return function (v1) {
                return function (a) {
                    return function (b) {
                        var v2 = f(a);
                        var v3 = f(b);
                        return append(v(v2.value0)(v3.value0))(v1(v2.value1)(v3.value1));
                    };
                };
            };
        };
    },
    Contravariant0: function () {
        return Data_Comparison.contravariantComparison;
    }
};
var divide = function (dict) {
    return dict.divide;
};
var divided = function (dictDivide) {
    return divide(dictDivide)(identity);
};
export {
    divide,
    divided,
    divideComparison,
    divideEquivalence,
    dividePredicate,
    divideOp
};
//# sourceMappingURL=index.js.map
