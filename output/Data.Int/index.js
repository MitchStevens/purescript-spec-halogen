// Generated by purs version 0.15.15
import * as $foreign from "./foreign.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Bounded from "../Data.Bounded/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Number from "../Data.Number/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Semiring from "../Data.Semiring/index.js";
var top = /* #__PURE__ */ Data_Bounded.top(Data_Bounded.boundedInt);
var bottom = /* #__PURE__ */ Data_Bounded.bottom(Data_Bounded.boundedInt);
var Radix = function (x) {
    return x;
};
var Even = /* #__PURE__ */ (function () {
    function Even() {

    };
    Even.value = new Even();
    return Even;
})();
var Odd = /* #__PURE__ */ (function () {
    function Odd() {

    };
    Odd.value = new Odd();
    return Odd;
})();
var showParity = {
    show: function (v) {
        if (v instanceof Even) {
            return "Even";
        };
        if (v instanceof Odd) {
            return "Odd";
        };
        throw new Error("Failed pattern match at Data.Int (line 117, column 1 - line 119, column 19): " + [ v.constructor.name ]);
    }
};
var radix = function (n) {
    if (n >= 2 && n <= 36) {
        return new Data_Maybe.Just(n);
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Data.Int (line 198, column 1 - line 198, column 28): " + [ n.constructor.name ]);
};
var odd = function (x) {
    return (x & 1) !== 0;
};
var octal = 8;
var hexadecimal = 16;
var fromStringAs = /* #__PURE__ */ (function () {
    return $foreign.fromStringAsImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
})();
var fromString = /* #__PURE__ */ fromStringAs(10);
var fromNumber = /* #__PURE__ */ (function () {
    return $foreign.fromNumberImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
})();
var unsafeClamp = function (x) {
    if (!Data_Number["isFinite"](x)) {
        return 0;
    };
    if (x >= $foreign.toNumber(top)) {
        return top;
    };
    if (x <= $foreign.toNumber(bottom)) {
        return bottom;
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.fromMaybe(0)(fromNumber(x));
    };
    throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): " + [ x.constructor.name ]);
};
var round = function ($37) {
    return unsafeClamp(Data_Number.round($37));
};
var trunc = function ($38) {
    return unsafeClamp(Data_Number.trunc($38));
};
var floor = function ($39) {
    return unsafeClamp(Data_Number.floor($39));
};
var even = function (x) {
    return (x & 1) === 0;
};
var parity = function (n) {
    var $28 = even(n);
    if ($28) {
        return Even.value;
    };
    return Odd.value;
};
var eqParity = {
    eq: function (x) {
        return function (y) {
            if (x instanceof Even && y instanceof Even) {
                return true;
            };
            if (x instanceof Odd && y instanceof Odd) {
                return true;
            };
            return false;
        };
    }
};
var eq1 = /* #__PURE__ */ Data_Eq.eq(eqParity);
var ordParity = {
    compare: function (x) {
        return function (y) {
            if (x instanceof Even && y instanceof Even) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof Even) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof Even) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof Odd && y instanceof Odd) {
                return Data_Ordering.EQ.value;
            };
            throw new Error("Failed pattern match at Data.Int (line 0, column 0 - line 0, column 0): " + [ x.constructor.name, y.constructor.name ]);
        };
    },
    Eq0: function () {
        return eqParity;
    }
};
var semiringParity = /* #__PURE__ */ (function () {
    return {
        zero: Even.value,
        add: function (x) {
            return function (y) {
                var $33 = eq1(x)(y);
                if ($33) {
                    return Even.value;
                };
                return Odd.value;
            };
        },
        one: Odd.value,
        mul: function (v) {
            return function (v1) {
                if (v instanceof Odd && v1 instanceof Odd) {
                    return Odd.value;
                };
                return Even.value;
            };
        }
    };
})();
var ringParity = {
    sub: /* #__PURE__ */ Data_Semiring.add(semiringParity),
    Semiring0: function () {
        return semiringParity;
    }
};
var divisionRingParity = {
    recip: /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn),
    Ring0: function () {
        return ringParity;
    }
};
var decimal = 10;
var commutativeRingParity = {
    Ring0: function () {
        return ringParity;
    }
};
var euclideanRingParity = {
    degree: function (v) {
        if (v instanceof Even) {
            return 0;
        };
        if (v instanceof Odd) {
            return 1;
        };
        throw new Error("Failed pattern match at Data.Int (line 137, column 1 - line 141, column 17): " + [ v.constructor.name ]);
    },
    div: function (x) {
        return function (v) {
            return x;
        };
    },
    mod: function (v) {
        return function (v1) {
            return Even.value;
        };
    },
    CommutativeRing0: function () {
        return commutativeRingParity;
    }
};
var ceil = function ($40) {
    return unsafeClamp(Data_Number.ceil($40));
};
var boundedParity = /* #__PURE__ */ (function () {
    return {
        bottom: Even.value,
        top: Odd.value,
        Ord0: function () {
            return ordParity;
        }
    };
})();
var binary = 2;
var base36 = 36;
export {
    toNumber,
    toStringAs,
    quot,
    rem,
    pow
} from "./foreign.js";
export {
    fromNumber,
    ceil,
    floor,
    trunc,
    round,
    fromString,
    radix,
    binary,
    octal,
    decimal,
    hexadecimal,
    base36,
    fromStringAs,
    Even,
    Odd,
    parity,
    even,
    odd,
    eqParity,
    ordParity,
    showParity,
    boundedParity,
    semiringParity,
    ringParity,
    commutativeRingParity,
    euclideanRingParity,
    divisionRingParity
};
//# sourceMappingURL=index.js.map
