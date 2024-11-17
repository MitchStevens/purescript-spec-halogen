// Generated by purs version 0.15.15
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad_Rec_Class from "../Control.Monad.Rec.Class/index.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Array_NonEmpty from "../Data.Array.NonEmpty/index.js";
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Enum from "../Data.Enum/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_EuclideanRing from "../Data.EuclideanRing/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Function_Uncurried from "../Data.Function.Uncurried/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Int from "../Data.Int/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_String_CodePoints from "../Data.String.CodePoints/index.js";
import * as Data_String_CodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data_String_Common from "../Data.String.Common/index.js";
import * as Data_String_Regex from "../Data.String.Regex/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing_Combinators from "../Parsing.Combinators/index.js";
var fromEnum = /* #__PURE__ */ Data_Enum.fromEnum(Data_String_CodePoints.boundedEnumCodePoint);
var mod = /* #__PURE__ */ Data_EuclideanRing.mod(Data_EuclideanRing.euclideanRingInt);
var fromJust = /* #__PURE__ */ Data_Maybe.fromJust();
var toEnum = /* #__PURE__ */ Data_Enum.toEnum(Data_Enum.boundedEnumChar);
var eq1 = /* #__PURE__ */ Data_Eq.eq(Data_String_CodePoints.eqCodePoint);
var div = /* #__PURE__ */ Data_EuclideanRing.div(Data_EuclideanRing.euclideanRingInt);
var max = /* #__PURE__ */ Data_Ord.max(Data_Ord.ordInt);
var min = /* #__PURE__ */ Data_Ord.min(Data_Ord.ordInt);
var show = /* #__PURE__ */ Data_Show.show(Data_Show.showInt);
var bind = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT);
var pure = /* #__PURE__ */ Control_Applicative.pure(Parsing.applicativeParserT);
var mapFlipped = /* #__PURE__ */ Data_Functor.mapFlipped(Data_Either.functorEither);
var map = /* #__PURE__ */ Data_Functor.map(Data_Maybe.functorMaybe);
var show1 = /* #__PURE__ */ Data_Show.show(Data_Show.showString);
var show2 = /* #__PURE__ */ Data_Show.show(Data_Show.showChar);
var alt = /* #__PURE__ */ Control_Alt.alt(Parsing.altParserT);
var tailRecM = /* #__PURE__ */ Control_Monad_Rec_Class.tailRecM(Parsing.monadRecParserT);
var updatePosSingle = function (v) {
    return function (cp) {
        return function (after) {
            var v1 = fromEnum(cp);
            if (v1 === 10) {
                return {
                    index: v.index + 1 | 0,
                    line: v.line + 1 | 0,
                    column: 1
                };
            };
            if (v1 === 13) {
                var v2 = Data_String_CodePoints.codePointAt(0)(after);
                if (v2 instanceof Data_Maybe.Just && fromEnum(v2.value0) === 10) {
                    return {
                        index: v.index + 1 | 0,
                        line: v.line,
                        column: v.column
                    };
                };
                return {
                    index: v.index + 1 | 0,
                    line: v.line + 1 | 0,
                    column: 1
                };
            };
            if (v1 === 9) {
                return {
                    index: v.index + 1 | 0,
                    line: v.line,
                    column: (v.column + 8 | 0) - mod(v.column - 1 | 0)(8) | 0
                };
            };
            return {
                index: v.index + 1 | 0,
                line: v.line,
                column: v.column + 1 | 0
            };
        };
    };
};
var updatePosString = function ($copy_pos) {
    return function ($copy_before) {
        return function ($copy_after) {
            var $tco_var_pos = $copy_pos;
            var $tco_var_before = $copy_before;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(pos, before, after) {
                var v = Data_String_CodePoints.uncons(before);
                if (v instanceof Data_Maybe.Nothing) {
                    $tco_done = true;
                    return pos;
                };
                if (v instanceof Data_Maybe.Just) {
                    var newPos = (function () {
                        if (Data_String_Common["null"](v.value0.tail)) {
                            return updatePosSingle(pos)(v.value0.head)(after);
                        };
                        if (Data_Boolean.otherwise) {
                            return updatePosSingle(pos)(v.value0.head)(v.value0.tail);
                        };
                        throw new Error("Failed pattern match at Parsing.String (line 165, column 7 - line 167, column 52): " + [  ]);
                    })();
                    $tco_var_pos = newPos;
                    $tco_var_before = v.value0.tail;
                    $copy_after = after;
                    return;
                };
                throw new Error("Failed pattern match at Parsing.String (line 161, column 36 - line 168, column 38): " + [ v.constructor.name ]);
            };
            while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_pos, $tco_var_before, $copy_after);
            };
            return $tco_result;
        };
    };
};
var satisfyCodePoint = function (f) {
    return Data_Function_Uncurried.mkFn5(function (v) {
        return function (v1) {
            return function (v2) {
                return function ($$throw) {
                    return function (done) {
                        var v3 = Data_String_CodePoints.uncons(v.value0);
                        if (v3 instanceof Data_Maybe.Nothing) {
                            return $$throw(v, new Parsing.ParseError("Unexpected EOF", v.value1));
                        };
                        if (v3 instanceof Data_Maybe.Just) {
                            var $76 = f(v3.value0.head);
                            if ($76) {
                                return done(new Parsing.ParseState(v3.value0.tail, updatePosSingle(v.value1)(v3.value0.head)(v3.value0.tail), true), v3.value0.head);
                            };
                            return $$throw(v, new Parsing.ParseError("Predicate unsatisfied", v.value1));
                        };
                        throw new Error("Failed pattern match at Parsing.String (line 136, column 7 - line 143, column 73): " + [ v3.constructor.name ]);
                    };
                };
            };
        };
    });
};
var satisfy = function (f) {
    return Data_Function_Uncurried.mkFn5(function (v) {
        return function (v1) {
            return function (v2) {
                return function ($$throw) {
                    return function (done) {
                        var v3 = Data_String_CodePoints.uncons(v.value0);
                        if (v3 instanceof Data_Maybe.Nothing) {
                            return $$throw(v, new Parsing.ParseError("Unexpected EOF", v.value1));
                        };
                        if (v3 instanceof Data_Maybe.Just) {
                            var cp = fromEnum(v3.value0.head);
                            var $85 = cp < 0 || cp > 65535;
                            if ($85) {
                                return $$throw(v, new Parsing.ParseError("Expected Char", v.value1));
                            };
                            var ch = fromJust(toEnum(cp));
                            var $86 = f(ch);
                            if ($86) {
                                return done(new Parsing.ParseState(v3.value0.tail, updatePosSingle(v.value1)(v3.value0.head)(v3.value0.tail), true), ch);
                            };
                            return $$throw(v, new Parsing.ParseError("Predicate unsatisfied", v.value1));
                        };
                        throw new Error("Failed pattern match at Parsing.String (line 114, column 7 - line 129, column 75): " + [ v3.constructor.name ]);
                    };
                };
            };
        };
    });
};
var parseErrorHuman = function (input) {
    return function (contextSize) {
        return function (v) {
            var v1 = (function () {
                var go = function ($copy_posBegin) {
                    return function ($copy_lineBegin) {
                        return function ($copy_posEnd) {
                            return function ($copy_lineEnd) {
                                var $tco_var_posBegin = $copy_posBegin;
                                var $tco_var_lineBegin = $copy_lineBegin;
                                var $tco_var_posEnd = $copy_posEnd;
                                var $tco_done = false;
                                var $tco_result;
                                function $tco_loop(posBegin, lineBegin, posEnd, lineEnd) {
                                    var v2 = Data_String_CodePoints.uncons(lineEnd);
                                    if (v2 instanceof Data_Maybe.Just && eq1(v2.value0.head)(Data_String_CodePoints.codePointFromChar("\x0a"))) {
                                        var $97 = posEnd === v.value1.index;
                                        if ($97) {
                                            $tco_done = true;
                                            return {
                                                posBegin: posBegin,
                                                posEnd: posEnd + 1 | 0,
                                                lineBegin: lineBegin
                                            };
                                        };
                                        var $98 = posEnd > v.value1.index;
                                        if ($98) {
                                            $tco_done = true;
                                            return {
                                                posBegin: posBegin,
                                                posEnd: posEnd,
                                                lineBegin: lineBegin
                                            };
                                        };
                                        $tco_var_posBegin = posEnd + 1 | 0;
                                        $tco_var_lineBegin = v2.value0.tail;
                                        $tco_var_posEnd = posEnd + 1 | 0;
                                        $copy_lineEnd = v2.value0.tail;
                                        return;
                                    };
                                    if (v2 instanceof Data_Maybe.Just && eq1(v2.value0.head)(Data_String_CodePoints.codePointFromChar("\x0d"))) {
                                        var $102 = posEnd === v.value1.index;
                                        if ($102) {
                                            $tco_done = true;
                                            return {
                                                posBegin: posBegin,
                                                posEnd: posEnd + 1 | 0,
                                                lineBegin: lineBegin
                                            };
                                        };
                                        var $103 = posEnd > v.value1.index;
                                        if ($103) {
                                            $tco_done = true;
                                            return {
                                                posBegin: posBegin,
                                                posEnd: posEnd,
                                                lineBegin: lineBegin
                                            };
                                        };
                                        $tco_var_posBegin = posEnd + 1 | 0;
                                        $tco_var_lineBegin = v2.value0.tail;
                                        $tco_var_posEnd = posEnd + 1 | 0;
                                        $copy_lineEnd = v2.value0.tail;
                                        return;
                                    };
                                    if (v2 instanceof Data_Maybe.Just) {
                                        $tco_var_posBegin = posBegin;
                                        $tco_var_lineBegin = lineBegin;
                                        $tco_var_posEnd = posEnd + 1 | 0;
                                        $copy_lineEnd = v2.value0.tail;
                                        return;
                                    };
                                    $tco_done = true;
                                    return {
                                        posBegin: posBegin,
                                        posEnd: posEnd,
                                        lineBegin: lineBegin
                                    };
                                };
                                while (!$tco_done) {
                                    $tco_result = $tco_loop($tco_var_posBegin, $tco_var_lineBegin, $tco_var_posEnd, $copy_lineEnd);
                                };
                                return $tco_result;
                            };
                        };
                    };
                };
                return go(0)(input)(0)(input);
            })();
            var lineSelect = Data_String_CodePoints.take(v1.posEnd - v1.posBegin | 0)(v1.lineBegin);
            var lineLength = Data_String_CodePoints.length(lineSelect);
            var lineIndex = v.value1.index - v1.posBegin | 0;
            var bestPosBefore = lineIndex - div(contextSize)(2) | 0;
            var bestPosAfter = (lineIndex + div(contextSize)(2) | 0) + (function () {
                var $110 = Data_Int.odd(contextSize);
                if ($110) {
                    return 1;
                };
                return 0;
            })() | 0;
            var v2 = (function () {
                var $111 = bestPosBefore >= 0;
                if ($111) {
                    var $112 = bestPosAfter <= lineLength;
                    if ($112) {
                        return new Data_Tuple.Tuple(bestPosBefore, bestPosAfter);
                    };
                    return new Data_Tuple.Tuple(max(0)(lineLength - contextSize | 0), lineLength);
                };
                return new Data_Tuple.Tuple(0, min(lineLength)(contextSize));
            })();
            var inputContext = Data_String_CodePoints.take(v2.value1 - v2.value0 | 0)(Data_String_CodePoints.drop(v2.value0)(lineSelect));
            return [ v.value0 + (" at position index:" + (show(v.value1.index) + (" (line:" + (show(v.value1.line) + (", column:" + (show(v.value1.column) + ")")))))), Data_String_Common.joinWith("")(Data_Array.replicate(lineIndex - v2.value0 | 0)(" ")) + "\u25bc", inputContext ];
        };
    };
};
var match = function (p) {
    return bind(Parsing.getParserT)(function (v) {
        return bind(p)(function (x) {
            return bind(Parsing.getParserT)(function (v1) {
                return pure(new Data_Tuple.Tuple(Data_String_CodeUnits.take(Data_String_CodeUnits.length(v.value0) - Data_String_CodeUnits.length(v1.value0) | 0)(v.value0), x));
            });
        });
    });
};
var eof = /* #__PURE__ */ Data_Function_Uncurried.mkFn5(function (v) {
    return function (v1) {
        return function (v2) {
            return function ($$throw) {
                return function (done) {
                    var $133 = Data_String_Common["null"](v.value0);
                    if ($133) {
                        return done(new Parsing.ParseState(v.value0, v.value1, true), Data_Unit.unit);
                    };
                    return $$throw(v, new Parsing.ParseError("Expected EOF", v.value1));
                };
            };
        };
    };
});
var consumeWith = function (f) {
    return Data_Function_Uncurried.mkFn5(function (v) {
        return function (v1) {
            return function (v2) {
                return function ($$throw) {
                    return function (done) {
                        var v3 = f(v.value0);
                        if (v3 instanceof Data_Either.Left) {
                            return $$throw(v, new Parsing.ParseError(v3.value0, v.value1));
                        };
                        if (v3 instanceof Data_Either.Right) {
                            return done(new Parsing.ParseState(v3.value0.remainder, updatePosString(v.value1)(v3.value0.consumed)(v3.value0.remainder), !Data_String_Common["null"](v3.value0.consumed)), v3.value0.value);
                        };
                        throw new Error("Failed pattern match at Parsing.String (line 286, column 7 - line 290, column 121): " + [ v3.constructor.name ]);
                    };
                };
            };
        };
    });
};
var regex = function (pattern) {
    return function (flags) {
        return mapFlipped(Data_String_Regex.regex("^(" + (pattern + ")"))(flags))(function (regexobj) {
            return consumeWith(function (input) {
                var v = map(Data_Array_NonEmpty.head)(Data_String_Regex.match(regexobj)(input));
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Data_Maybe.Just) {
                    var remainder = Data_String_CodeUnits.drop(Data_String_CodeUnits.length(v.value0.value0))(input);
                    return new Data_Either.Right({
                        value: v.value0.value0,
                        consumed: v.value0.value0,
                        remainder: remainder
                    });
                };
                return new Data_Either.Left("No Regex pattern match");
            });
        });
    };
};
var rest = /* #__PURE__ */ consumeWith(function (consumed) {
    return new Data_Either.Right({
        value: consumed,
        consumed: consumed,
        remainder: ""
    });
});
var string = function (str) {
    return consumeWith(function (input) {
        var v = Data_String_CodeUnits.stripPrefix(str)(input);
        if (v instanceof Data_Maybe.Just) {
            return new Data_Either.Right({
                value: str,
                consumed: str,
                remainder: v.value0
            });
        };
        return new Data_Either.Left("Expected " + show1(str));
    });
};
var takeN = function (n) {
    return consumeWith(function (input) {
        var v = Data_String_CodePoints.splitAt(n)(input);
        var $153 = Data_String_CodePoints.length(v.before) === n;
        if ($153) {
            return new Data_Either.Right({
                value: v.before,
                consumed: v.before,
                remainder: v.after
            });
        };
        return new Data_Either.Left("Could not take " + (show(n) + " characters"));
    });
};
var $$char = function (c) {
    return Parsing_Combinators.withErrorMessage(satisfy(function (v) {
        return v === c;
    }))(show2(c));
};
var anyCodePoint = /* #__PURE__ */ satisfyCodePoint(/* #__PURE__ */ Data_Function["const"](true));
var anyTill = function (dictMonad) {
    return function (p) {
        var go = function (unit) {
            return alt(bind(Parsing.getParserT)(function (v) {
                return bind(Parsing_Combinators["try"](p))(function (t) {
                    return pure(new Control_Monad_Rec_Class.Done(new Data_Tuple.Tuple(v.value0, t)));
                });
            }))(bind(anyCodePoint)(function () {
                return pure(new Control_Monad_Rec_Class.Loop(unit));
            }));
        };
        return bind(Parsing.getParserT)(function (v) {
            return bind(tailRecM(go)(Data_Unit.unit))(function (v1) {
                return pure(new Data_Tuple.Tuple(Data_String_CodeUnits.take(Data_String_CodeUnits.length(v.value0) - Data_String_CodeUnits.length(v1.value0) | 0)(v.value0), v1.value1));
            });
        });
    };
};
var anyChar = /* #__PURE__ */ satisfy(/* #__PURE__ */ Data_Function["const"](true));
export {
    $$char as char,
    string,
    anyChar,
    anyCodePoint,
    satisfy,
    satisfyCodePoint,
    takeN,
    rest,
    eof,
    match,
    regex,
    anyTill,
    consumeWith,
    parseErrorHuman
};
//# sourceMappingURL=index.js.map
