// Generated by purs version 0.15.15
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Alternative from "../Control.Alternative/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad_Free from "../Control.Monad.Free/index.js";
import * as Control_Monad_State_Class from "../Control.Monad.State.Class/index.js";
import * as Control_Monad_State_Trans from "../Control.Monad.State.Trans/index.js";
import * as Control_Monad_Trans_Class from "../Control.Monad.Trans.Class/index.js";
import * as Control_Plus from "../Control.Plus/index.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Exists from "../Data.Exists/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_HeytingAlgebra from "../Data.HeytingAlgebra/index.js";
import * as Data_List from "../Data.List/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_String_CodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Options_Applicative_Internal from "../Options.Applicative.Internal/index.js";
import * as Options_Applicative_Internal_Utils from "../Options.Applicative.Internal.Utils/index.js";
import * as Options_Applicative_Types from "../Options.Applicative.Types/index.js";
var bind = /* #__PURE__ */ Control_Bind.bind(Control_Bind.bindArray);
var fromFoldable = /* #__PURE__ */ Data_Array.fromFoldable(Data_List_Types.foldableList);
var map = /* #__PURE__ */ Data_Functor.map(Data_Maybe.functorMaybe);
var voidRight = /* #__PURE__ */ Data_Functor.voidRight(Data_Maybe.functorMaybe);
var guard = /* #__PURE__ */ Control_Alternative.guard(Data_Maybe.alternativeMaybe);
var any = /* #__PURE__ */ Data_Foldable.any(Data_Foldable.foldableArray)(Data_HeytingAlgebra.heytingAlgebraBoolean);
var elem = /* #__PURE__ */ Data_Foldable.elem(Data_Foldable.foldableArray)(Options_Applicative_Types.optNameEq);
var discard = /* #__PURE__ */ Control_Bind.discard(Control_Bind.discardUnit);
var discard1 = /* #__PURE__ */ discard(Data_Maybe.bindMaybe);
var un = /* #__PURE__ */ Data_Newtype.un();
var lift = /* #__PURE__ */ Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT);
var apply = /* #__PURE__ */ Control_Apply.apply(Data_Maybe.applyMaybe);
var alt = /* #__PURE__ */ Control_Alt.alt(Data_Maybe.altMaybe);
var bind1 = /* #__PURE__ */ Control_Bind.bind(Data_Maybe.bindMaybe);
var apply1 = /* #__PURE__ */ Control_Apply.apply(Options_Applicative_Types.parserApply);
var oneOf = /* #__PURE__ */ Data_Foldable.oneOf(Data_Foldable.foldableArray);
var bind2 = /* #__PURE__ */ Control_Bind.bind(Control_Monad_Free.freeBind);
var greaterThan = /* #__PURE__ */ Data_Ord.greaterThan(Options_Applicative_Types.optVisibilityOrd);
var lift1 = /* #__PURE__ */ Control_Monad_Trans_Class.lift(Options_Applicative_Internal.nondetTMonadTrans);
var pure = /* #__PURE__ */ Control_Applicative.pure(Options_Applicative_Types.parserApplicative);
var pure1 = /* #__PURE__ */ Control_Applicative.pure(Data_Maybe.applicativeMaybe);
var notEq1 = /* #__PURE__ */ Data_Eq.notEq(Options_Applicative_Types.argPolicyEq);
var OptWord = /* #__PURE__ */ (function () {
    function OptWord(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    OptWord.create = function (value0) {
        return function (value1) {
            return new OptWord(value0, value1);
        };
    };
    return OptWord;
})();
var unexpectedError = function (arg) {
    return function (p) {
        return new Options_Applicative_Types.UnexpectedError(arg, new Options_Applicative_Types.SomeParser(Data_Exists.mkExists(p)));
    };
};
var simplify = function (v) {
    if (v instanceof Options_Applicative_Types.Leaf) {
        return new Options_Applicative_Types.Leaf(v.value0);
    };
    if (v instanceof Options_Applicative_Types.MultNode) {
        var remove_mult = function (v1) {
            if (v1 instanceof Options_Applicative_Types.MultNode) {
                return v1.value0;
            };
            return [ v1 ];
        };
        var v1 = bind(v.value0)(function ($340) {
            return remove_mult(simplify($340));
        });
        if (v1.length === 1) {
            return v1[0];
        };
        return new Options_Applicative_Types.MultNode(v1);
    };
    if (v instanceof Options_Applicative_Types.AltNode) {
        var remove_alt = function (v1) {
            if (v1 instanceof Options_Applicative_Types.AltNode) {
                return v1.value0;
            };
            if (v1 instanceof Options_Applicative_Types.MultNode && v1.value0.length === 0) {
                return [  ];
            };
            return [ v1 ];
        };
        var v1 = bind(v.value0)(function ($341) {
            return remove_alt(simplify($341));
        });
        if (v1.length === 0) {
            return new Options_Applicative_Types.MultNode([  ]);
        };
        if (v1.length === 1) {
            return v1[0];
        };
        return new Options_Applicative_Types.AltNode(v1);
    };
    throw new Error("Failed pattern match at Options.Applicative.Common (line 280, column 1 - line 280, column 45): " + [ v.constructor.name ]);
};
var showOption = function (v) {
    if (v instanceof Options_Applicative_Types.OptLong) {
        return "--" + v.value0;
    };
    if (v instanceof Options_Applicative_Types.OptShort) {
        return Data_String_CodeUnits.fromCharArray([ "-", v.value0 ]);
    };
    throw new Error("Failed pattern match at Options.Applicative.Common (line 43, column 1 - line 43, column 32): " + [ v.constructor.name ]);
};
var parseWord = /* #__PURE__ */ (function () {
    var go = function (v) {
        if (v instanceof Data_List_Types.Cons && (v.value0 === "-" && (v.value1 instanceof Data_List_Types.Cons && v.value1.value0 === "-"))) {
            return new Data_Maybe.Just((function () {
                var v1 = (function () {
                    var v2 = Data_List.span(function (v3) {
                        return v3 !== "=";
                    })(v.value1.value1);
                    if (v2.rest instanceof Data_List_Types.Nil) {
                        return new Data_Tuple.Tuple(v.value1.value1, Data_Maybe.Nothing.value);
                    };
                    if (v2.rest instanceof Data_List_Types.Cons) {
                        return new Data_Tuple.Tuple(v2.init, new Data_Maybe.Just(v2.rest.value1));
                    };
                    throw new Error("Failed pattern match at Options.Applicative.Common (line 107, column 23 - line 109, column 70): " + [ v2.constructor.name ]);
                })();
                return new OptWord(new Options_Applicative_Types.OptLong(Data_String_CodeUnits.fromCharArray(fromFoldable(v1.value0))), map(function ($342) {
                    return Data_String_CodeUnits.fromCharArray(fromFoldable($342));
                })(v1.value1));
            })());
        };
        if (v instanceof Data_List_Types.Cons && v.value0 === "-") {
            if (v.value1 instanceof Data_List_Types.Nil) {
                return Data_Maybe.Nothing.value;
            };
            if (v.value1 instanceof Data_List_Types.Cons) {
                return new Data_Maybe.Just((function () {
                    var arg = voidRight(v.value1.value1)(guard(!Data_List["null"](v.value1.value1)));
                    return new OptWord(new Options_Applicative_Types.OptShort(v.value1.value0), map(function ($343) {
                        return Data_String_CodeUnits.fromCharArray(fromFoldable($343));
                    })(arg));
                })());
            };
            throw new Error("Failed pattern match at Options.Applicative.Common (line 111, column 25 - line 115, column 79): " + [ v.value1.constructor.name ]);
        };
        return Data_Maybe.Nothing.value;
    };
    var $344 = Data_List.fromFoldable(Data_Foldable.foldableArray);
    return function ($345) {
        return go($344(Data_String_CodeUnits.toCharArray($345)));
    };
})();
var optionNames = function (v) {
    if (v instanceof Options_Applicative_Types.OptReader) {
        return v.value0;
    };
    if (v instanceof Options_Applicative_Types.FlagReader) {
        return v.value0;
    };
    return [  ];
};
var liftOpt = /* #__PURE__ */ (function () {
    return Options_Applicative_Types.OptP.create;
})();
var isOptionPrefix = function (v) {
    return function (v1) {
        if (v instanceof Options_Applicative_Types.OptShort && v1 instanceof Options_Applicative_Types.OptShort) {
            return v.value0 === v1.value0;
        };
        if (v instanceof Options_Applicative_Types.OptLong && v1 instanceof Options_Applicative_Types.OptLong) {
            return Options_Applicative_Internal_Utils.startsWith(v.value0)(v1.value0);
        };
        return false;
    };
};
var optMatches = function (dictMonadP) {
    var Monad0 = dictMonadP.Monad0();
    var bindStateT = Control_Monad_State_Trans.bindStateT(Monad0);
    var bind3 = Control_Bind.bind(bindStateT);
    var monadStateStateT = Control_Monad_State_Trans.monadStateStateT(Monad0);
    var get = Control_Monad_State_Class.get(monadStateStateT);
    var missingArgP = Options_Applicative_Internal.missingArgP(dictMonadP);
    var lift2 = lift(Monad0);
    var pure2 = Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(Monad0));
    var discard2 = discard(bindStateT);
    var put = Control_Monad_State_Class.put(monadStateStateT);
    var runReadM = Options_Applicative_Internal.runReadM(dictMonadP);
    return function (disambiguate) {
        return function (opt) {
            return function (v) {
                var is_short = function (v1) {
                    if (v1 instanceof Options_Applicative_Types.OptShort) {
                        return true;
                    };
                    if (v1 instanceof Options_Applicative_Types.OptLong) {
                        return false;
                    };
                    throw new Error("Failed pattern match at Options.Applicative.Common (line 90, column 5 - line 90, column 33): " + [ v1.constructor.name ]);
                };
                var has_name = function (a) {
                    if (disambiguate) {
                        return any(isOptionPrefix(a));
                    };
                    if (Data_Boolean.otherwise) {
                        return elem(a);
                    };
                    throw new Error("Failed pattern match at Options.Applicative.Common (line 93, column 5 - line 95, column 27): " + [ a.constructor.name ]);
                };
                var errorFor = function (name) {
                    return function (msg) {
                        return "option " + (showOption(name) + (": " + msg));
                    };
                };
                if (opt instanceof Options_Applicative_Types.OptReader) {
                    return discard1(guard(has_name(v.value0)(opt.value0)))(function () {
                        return new Data_Maybe.Just(bind3(get)(function (args) {
                            var missing_arg = missingArgP(opt.value2(showOption(v.value0)))((un(Options_Applicative_Types.CReader)(opt.value1)).crCompleter);
                            return bind3((function () {
                                var v1 = Data_Maybe.maybe(args)(function (v2) {
                                    return new Data_List_Types.Cons(v2, args);
                                })(v.value1);
                                if (v1 instanceof Data_List_Types.Nil) {
                                    return lift2(missing_arg);
                                };
                                if (v1 instanceof Data_List_Types.Cons) {
                                    return pure2(new Data_Tuple.Tuple(v1.value0, v1.value1));
                                };
                                throw new Error("Failed pattern match at Options.Applicative.Common (line 68, column 27 - line 70, column 56): " + [ v1.constructor.name ]);
                            })())(function (v1) {
                                return discard2(put(v1.value1))(function () {
                                    return lift2(runReadM(Options_Applicative_Internal.withReadM(errorFor(v.value0))((un(Options_Applicative_Types.CReader)(opt.value1)).crReader))(v1.value0));
                                });
                            });
                        }));
                    });
                };
                if (opt instanceof Options_Applicative_Types.FlagReader) {
                    return discard1(guard(has_name(v.value0)(opt.value0)))(function () {
                        return discard1(guard(is_short(v.value0) || Data_Maybe.isNothing(v.value1)))(function () {
                            return new Data_Maybe.Just(bind3(get)(function (args) {
                                var val$prime = map(function ($346) {
                                    return (function (s) {
                                        return Data_Array.cons("-")(s);
                                    })(Data_String_CodeUnits.toCharArray($346));
                                })(v.value1);
                                return discard2(put(Data_Maybe.maybe(args)((function () {
                                    var $347 = Data_Function.flip(Data_List_Types.Cons.create)(args);
                                    return function ($348) {
                                        return $347(Data_String_CodeUnits.fromCharArray($348));
                                    };
                                })())(val$prime)))(function () {
                                    return pure2(opt.value1);
                                });
                            }));
                        });
                    });
                };
                return Data_Maybe.Nothing.value;
            };
        };
    };
};
var isArg = function (v) {
    if (v instanceof Options_Applicative_Types.ArgReader) {
        return true;
    };
    return false;
};
var evalParser = function (v) {
    if (v instanceof Options_Applicative_Types.NilP) {
        return new Data_Maybe.Just(v.value0);
    };
    if (v instanceof Options_Applicative_Types.OptP) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Options_Applicative_Types.MultP) {
        return Data_Exists.runExists(function (v1) {
            return apply(evalParser(v1.value0))(evalParser(v1.value1));
        })(v.value0);
    };
    if (v instanceof Options_Applicative_Types.AltP) {
        return alt(evalParser(v.value0))(evalParser(v.value1));
    };
    if (v instanceof Options_Applicative_Types.BindP) {
        return Control_Monad_Free["resume$prime"](function (p) {
            return function (k) {
                return bind1(evalParser(p))(function ($349) {
                    return evalParser(Options_Applicative_Types.BindP.create(k($349)));
                });
            };
        })(Data_Maybe.Just.create)(v.value0);
    };
    throw new Error("Failed pattern match at Options.Applicative.Common (line 220, column 1 - line 220, column 44): " + [ v.constructor.name ]);
};
var searchParser = function (dictMonad) {
    var nondetTPlus = Options_Applicative_Internal.nondetTPlus(dictMonad);
    var empty = Control_Plus.empty(nondetTPlus);
    var mapFlipped = Data_Functor.mapFlipped(Options_Applicative_Internal.nondetTFunctor(dictMonad));
    var nondetTAltOp = Options_Applicative_Internal.nondetTAltOp(dictMonad);
    var oneOf1 = oneOf(nondetTPlus);
    return function (v) {
        return function (v1) {
            if (v1 instanceof Options_Applicative_Types.NilP) {
                return empty;
            };
            if (v1 instanceof Options_Applicative_Types.OptP) {
                return v(v1.value0);
            };
            if (v1 instanceof Options_Applicative_Types.MultP) {
                return Data_Exists.runExists(function (v2) {
                    var b = mapFlipped(searchParser(dictMonad)(v)(v2.value1))(function (p2$prime) {
                        return apply1(v2.value0)(p2$prime);
                    });
                    var a = mapFlipped(searchParser(dictMonad)(v)(v2.value0))(function (p1$prime) {
                        return apply1(p1$prime)(v2.value1);
                    });
                    return nondetTAltOp(a)(b);
                })(v1.value0);
            };
            if (v1 instanceof Options_Applicative_Types.AltP) {
                return oneOf1([ searchParser(dictMonad)(v)(v1.value0), searchParser(dictMonad)(v)(v1.value1) ]);
            };
            if (v1 instanceof Options_Applicative_Types.BindP) {
                return Control_Monad_Free["resume$prime"](function (p) {
                    return function (k) {
                        return oneOf1([ mapFlipped(searchParser(dictMonad)(v)(p))(function (p$prime) {
                            return new Options_Applicative_Types.BindP(bind2(Control_Monad_Free.liftF(p$prime))(k));
                        }), (function () {
                            var v2 = evalParser(p);
                            if (v2 instanceof Data_Maybe.Nothing) {
                                return empty;
                            };
                            if (v2 instanceof Data_Maybe.Just) {
                                return searchParser(dictMonad)(v)(new Options_Applicative_Types.BindP(k(v2.value0)));
                            };
                            throw new Error("Failed pattern match at Options.Applicative.Common (line 135, column 7 - line 137, column 49): " + [ v2.constructor.name ]);
                        })() ]);
                    };
                })(Data_Function["const"](empty))(v1.value0);
            };
            throw new Error("Failed pattern match at Options.Applicative.Common (line 118, column 1 - line 120, column 49): " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
var searchOpt = function (dictMonadP) {
    var monadStateT = Control_Monad_State_Trans.monadStateT(dictMonadP.Monad0());
    var searchParser1 = searchParser(monadStateT);
    var optMatches1 = optMatches(dictMonadP);
    var lift2 = lift1(monadStateT);
    var map1 = Data_Functor.map(Control_Monad_State_Trans.functorStateT((dictMonadP.Alt1()).Functor0()));
    var empty = Control_Plus.empty(Options_Applicative_Internal.nondetTPlus(monadStateT));
    return function (pprefs) {
        return function (w) {
            return searchParser1(function (opt) {
                var disambiguate = (un(Options_Applicative_Types.ParserPrefs)(pprefs)).prefDisambiguate && greaterThan(Options_Applicative_Types.optVisibility(opt))(Options_Applicative_Types.Internal.value);
                var v = optMatches1(disambiguate)((un(Options_Applicative_Types.Option)(opt)).optMain)(w);
                if (v instanceof Data_Maybe.Just) {
                    return lift2(map1(pure)(v.value0));
                };
                if (v instanceof Data_Maybe.Nothing) {
                    return empty;
                };
                throw new Error("Failed pattern match at Options.Applicative.Common (line 144, column 3 - line 146, column 21): " + [ v.constructor.name ]);
            });
        };
    };
};
var stepParser = function (dictMonadP) {
    var alt1 = Control_Alt.alt(Options_Applicative_Internal.nondetTAlt(Control_Monad_State_Trans.monadStateT(dictMonadP.Monad0())));
    var searchOpt1 = searchOpt(dictMonadP);
    return function (v) {
        return function (v1) {
            return function (v2) {
                return function (v3) {
                    if (v1 instanceof Options_Applicative_Types.AllPositionals) {
                        return searchArg(dictMonadP)(v)(v2)(v3);
                    };
                    if (v1 instanceof Options_Applicative_Types.ForwardOptions) {
                        var v4 = parseWord(v2);
                        if (v4 instanceof Data_Maybe.Just) {
                            return alt1(searchOpt1(v)(v4.value0)(v3))(searchArg(dictMonadP)(v)(v2)(v3));
                        };
                        if (v4 instanceof Data_Maybe.Nothing) {
                            return searchArg(dictMonadP)(v)(v2)(v3);
                        };
                        throw new Error("Failed pattern match at Options.Applicative.Common (line 174, column 42 - line 176, column 36): " + [ v4.constructor.name ]);
                    };
                    var v4 = parseWord(v2);
                    if (v4 instanceof Data_Maybe.Just) {
                        return searchOpt1(v)(v4.value0)(v3);
                    };
                    if (v4 instanceof Data_Maybe.Nothing) {
                        return searchArg(dictMonadP)(v)(v2)(v3);
                    };
                    throw new Error("Failed pattern match at Options.Applicative.Common (line 177, column 29 - line 179, column 36): " + [ v4.constructor.name ]);
                };
            };
        };
    };
};
var searchArg = function (dictMonadP) {
    var Monad0 = dictMonadP.Monad0();
    var monadStateT = Control_Monad_State_Trans.monadStateT(Monad0);
    var searchParser1 = searchParser(monadStateT);
    var discard2 = discard(Options_Applicative_Internal.nondetTBind(monadStateT));
    var when = Control_Applicative.when(Options_Applicative_Internal.nondetTApplicative(monadStateT));
    var cut = Options_Applicative_Internal.cut(monadStateT);
    var lift2 = lift1(monadStateT);
    var bindStateT = Control_Monad_State_Trans.bindStateT(Monad0);
    var bind3 = Control_Bind.bind(bindStateT);
    var applyFirst = Control_Apply.applyFirst(Control_Monad_State_Trans.applyStateT(Monad0));
    var monadStateStateT = Control_Monad_State_Trans.monadStateStateT(Monad0);
    var get = Control_Monad_State_Class.get(monadStateStateT);
    var put = Control_Monad_State_Class.put(monadStateStateT);
    var map1 = Data_Functor.map(Control_Monad_State_Trans.functorStateT((dictMonadP.Alt1()).Functor0()));
    var lift3 = lift(Monad0);
    var Apply0 = (Monad0.Bind1()).Apply0();
    var applyFirst1 = Control_Apply.applyFirst(Apply0);
    var applySecond = Control_Apply.applySecond(Apply0);
    var enterContext = Options_Applicative_Internal.enterContext(dictMonadP);
    var exitContext = Options_Applicative_Internal.exitContext(dictMonadP);
    var map2 = Data_Functor.map(Options_Applicative_Internal.nondetTFunctor(monadStateT));
    var discard3 = discard(bindStateT);
    var pure2 = Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(Monad0));
    var empty = Control_Plus.empty(Options_Applicative_Internal.nondetTPlus(monadStateT));
    var runReadM = Options_Applicative_Internal.runReadM(dictMonadP);
    return function (prefs) {
        return function (arg) {
            return searchParser1(function (opt) {
                return discard2(when(isArg((un(Options_Applicative_Types.Option)(opt)).optMain))(cut))(function () {
                    var v = (un(Options_Applicative_Types.Option)(opt)).optMain;
                    if (v instanceof Options_Applicative_Types.CmdReader) {
                        var v1 = new Data_Tuple.Tuple(v.value2(arg), (un(Options_Applicative_Types.ParserPrefs)(prefs)).prefBacktrack);
                        if (v1.value0 instanceof Data_Maybe.Just && v1.value1 instanceof Options_Applicative_Types.NoBacktrack) {
                            return lift2(bind3(applyFirst(get)(put(Data_List_Types.Nil.value)))(function (args) {
                                return map1(pure)(lift3(applyFirst1(applySecond(enterContext(arg)(v1.value0.value0))(runParserInfo(dictMonadP)(v1.value0.value0)(args)))(exitContext)));
                            }));
                        };
                        if (v1.value0 instanceof Data_Maybe.Just && v1.value1 instanceof Options_Applicative_Types.Backtrack) {
                            return map2(pure)(lift2(Control_Monad_State_Trans.StateT(function (args) {
                                return applyFirst1(applySecond(enterContext(arg)(v1.value0.value0))(runParser(dictMonadP)((un(Options_Applicative_Types.ParserInfo)(v1.value0.value0)).infoPolicy)(Options_Applicative_Types.CmdStart.value)((un(Options_Applicative_Types.ParserInfo)(v1.value0.value0)).infoParser)(args)))(exitContext);
                            })));
                        };
                        if (v1.value0 instanceof Data_Maybe.Just && v1.value1 instanceof Options_Applicative_Types.SubparserInline) {
                            return lift2(discard3(lift3(enterContext(arg)(v1.value0.value0)))(function () {
                                return pure2((un(Options_Applicative_Types.ParserInfo)(v1.value0.value0)).infoParser);
                            }));
                        };
                        if (v1.value0 instanceof Data_Maybe.Nothing) {
                            return empty;
                        };
                        throw new Error("Failed pattern match at Options.Applicative.Common (line 154, column 7 - line 166, column 38): " + [ v1.constructor.name ]);
                    };
                    if (v instanceof Options_Applicative_Types.ArgReader) {
                        return map2(pure)(lift2(lift3(runReadM((un(Options_Applicative_Types.CReader)(v.value0)).crReader)(arg))));
                    };
                    return empty;
                });
            });
        };
    };
};
var runParserInfo = function (dictMonadP) {
    return function (i) {
        return runParserFully(dictMonadP)((un(Options_Applicative_Types.ParserInfo)(i)).infoPolicy)((un(Options_Applicative_Types.ParserInfo)(i)).infoParser);
    };
};
var runParserFully = function (dictMonadP) {
    var Monad0 = dictMonadP.Monad0();
    var bind3 = Control_Bind.bind(Monad0.Bind1());
    var pure2 = Control_Applicative.pure(Monad0.Applicative0());
    var errorP = Options_Applicative_Internal.errorP(dictMonadP);
    return function (policy) {
        return function (p) {
            return function (args) {
                return bind3(runParser(dictMonadP)(policy)(Options_Applicative_Types.CmdStart.value)(p)(args))(function (v) {
                    if (v.value1 instanceof Data_List_Types.Nil) {
                        return pure2(v.value0);
                    };
                    if (v.value1 instanceof Data_List_Types.Cons) {
                        return errorP(unexpectedError(v.value1.value0)(pure(Data_Unit.unit)));
                    };
                    throw new Error("Failed pattern match at Options.Applicative.Common (line 214, column 3 - line 216, column 66): " + [ v.value1.constructor.name ]);
                });
            };
        };
    };
};
var runParser = function (dictMonadP) {
    var Monad0 = dictMonadP.Monad0();
    var disamb = Options_Applicative_Internal.disamb(Control_Monad_State_Trans.monadStateT(Monad0));
    var exitP = Options_Applicative_Internal.exitP(dictMonadP);
    var bind3 = Control_Bind.bind(Monad0.Bind1());
    var getPrefs = Options_Applicative_Internal.getPrefs(dictMonadP);
    var hoistMaybe = Options_Applicative_Internal.hoistMaybe(dictMonadP);
    return function (policy) {
        return function (isCmdStart) {
            return function (p) {
                return function (args) {
                    var result = apply(map(Data_Tuple.Tuple.create)(evalParser(p)))(pure1(args));
                    var newPolicy = function (a) {
                        if (policy instanceof Options_Applicative_Types.NoIntersperse) {
                            var $299 = Data_Maybe.isJust(parseWord(a));
                            if ($299) {
                                return Options_Applicative_Types.NoIntersperse.value;
                            };
                            return Options_Applicative_Types.AllPositionals.value;
                        };
                        return policy;
                    };
                    var do_step = function (prefs) {
                        return function (arg) {
                            return function (argt) {
                                return (function (v) {
                                    return Control_Monad_State_Trans.runStateT(v)(argt);
                                })(disamb(!(un(Options_Applicative_Types.ParserPrefs)(prefs)).prefDisambiguate)(stepParser(dictMonadP)(prefs)(policy)(arg)(p)));
                            };
                        };
                    };
                    if (args instanceof Data_List_Types.Nil) {
                        return exitP(isCmdStart)(policy)(p)(result);
                    };
                    if (args instanceof Data_List_Types.Cons && (args.value0 === "--" && notEq1(policy)(Options_Applicative_Types.AllPositionals.value))) {
                        return runParser(dictMonadP)(Options_Applicative_Types.AllPositionals.value)(Options_Applicative_Types.CmdCont.value)(p)(args.value1);
                    };
                    if (args instanceof Data_List_Types.Cons) {
                        return bind3(getPrefs)(function (prefs) {
                            return bind3(do_step(prefs)(args.value0)(args.value1))(function (v) {
                                if (v.value0 instanceof Data_Maybe.Nothing) {
                                    return hoistMaybe(unexpectedError(args.value0)(p))(result);
                                };
                                if (v.value0 instanceof Data_Maybe.Just) {
                                    return runParser(dictMonadP)(newPolicy(args.value0))(Options_Applicative_Types.CmdCont.value)(v.value0.value0)(v.value1);
                                };
                                throw new Error("Failed pattern match at Options.Applicative.Common (line 192, column 5 - line 194, column 60): " + [ v.value0.constructor.name ]);
                            });
                        });
                    };
                    throw new Error("Failed pattern match at Options.Applicative.Common (line 186, column 38 - line 194, column 60): " + [ args.constructor.name ]);
                };
            };
        };
    };
};
var treeMapParser = function (g) {
    var has_default = function (p) {
        return Data_Maybe.isJust(evalParser(p));
    };
    var hasArg = function (v) {
        if (v instanceof Options_Applicative_Types.NilP) {
            return false;
        };
        if (v instanceof Options_Applicative_Types.OptP) {
            return isArg((un(Options_Applicative_Types.Option)(v.value0)).optMain);
        };
        if (v instanceof Options_Applicative_Types.MultP) {
            return Data_Exists.runExists(function (v1) {
                return hasArg(v1.value0) || hasArg(v1.value1);
            })(v.value0);
        };
        if (v instanceof Options_Applicative_Types.AltP) {
            return hasArg(v.value0) || hasArg(v.value1);
        };
        if (v instanceof Options_Applicative_Types.BindP) {
            return Control_Monad_Free["resume$prime"](function (p) {
                return function (v1) {
                    return hasArg(p);
                };
            })(Data_Function["const"](false))(v.value0);
        };
        throw new Error("Failed pattern match at Options.Applicative.Common (line 272, column 5 - line 272, column 44): " + [ v.constructor.name ]);
    };
    var go = function (v) {
        return function (v1) {
            return function (v2) {
                return function (v3) {
                    return function (v4) {
                        if (v4 instanceof Options_Applicative_Types.NilP) {
                            return new Options_Applicative_Types.MultNode([  ]);
                        };
                        if (v4 instanceof Options_Applicative_Types.OptP) {
                            if (greaterThan(Options_Applicative_Types.optVisibility(v4.value0))(Options_Applicative_Types.Internal.value)) {
                                return new Options_Applicative_Types.Leaf(v3({
                                    hinfoMulti: v,
                                    hinfoDefault: v1,
                                    hinfoUnreachableArgs: v2
                                })(v4.value0));
                            };
                            if (Data_Boolean.otherwise) {
                                return new Options_Applicative_Types.MultNode([  ]);
                            };
                        };
                        if (v4 instanceof Options_Applicative_Types.MultP) {
                            return Data_Exists.runExists(function (v5) {
                                var r$prime = v2 || hasArg(v5.value0);
                                return new Options_Applicative_Types.MultNode([ go(v)(v1)(v2)(v3)(v5.value0), go(v)(v1)(r$prime)(v3)(v5.value1) ]);
                            })(v4.value0);
                        };
                        if (v4 instanceof Options_Applicative_Types.AltP) {
                            var d$prime = v1 || (has_default(v4.value0) || has_default(v4.value1));
                            return new Options_Applicative_Types.AltNode([ go(v)(d$prime)(v2)(v3)(v4.value0), go(v)(d$prime)(v2)(v3)(v4.value1) ]);
                        };
                        if (v4 instanceof Options_Applicative_Types.BindP) {
                            return Control_Monad_Free["resume$prime"](function (p) {
                                return function (k) {
                                    var go$prime = go(true)(v1)(v2)(v3)(p);
                                    var v5 = evalParser(p);
                                    if (v5 instanceof Data_Maybe.Nothing) {
                                        return go$prime;
                                    };
                                    if (v5 instanceof Data_Maybe.Just) {
                                        return new Options_Applicative_Types.MultNode([ go$prime, go(true)(v1)(v2)(v3)(new Options_Applicative_Types.BindP(k(v5.value0))) ]);
                                    };
                                    throw new Error("Failed pattern match at Options.Applicative.Common (line 267, column 12 - line 269, column 68): " + [ v5.constructor.name ]);
                                };
                            })(Data_Function["const"](new Options_Applicative_Types.MultNode([  ])))(v4.value0);
                        };
                        throw new Error("Failed pattern match at Options.Applicative.Common (line 248, column 5 - line 251, column 21): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name, v3.constructor.name, v4.constructor.name ]);
                    };
                };
            };
        };
    };
    var $350 = go(false)(false)(false)(g);
    return function ($351) {
        return simplify($350($351));
    };
};
var mapParser = function (f) {
    var flatten = function (v) {
        if (v instanceof Options_Applicative_Types.Leaf) {
            return [ v.value0 ];
        };
        if (v instanceof Options_Applicative_Types.MultNode) {
            return bind(v.value0)(flatten);
        };
        if (v instanceof Options_Applicative_Types.AltNode) {
            return bind(v.value0)(flatten);
        };
        throw new Error("Failed pattern match at Options.Applicative.Common (line 235, column 5 - line 235, column 27): " + [ v.constructor.name ]);
    };
    var $352 = treeMapParser(f);
    return function ($353) {
        return flatten($352($353));
    };
};
export {
    liftOpt,
    showOption,
    runParserInfo,
    runParserFully,
    runParser,
    evalParser,
    mapParser,
    treeMapParser,
    optionNames
};
export {
    ParserInfo,
    ParserPrefs
} from "../Options.Applicative.Types/index.js";
//# sourceMappingURL=index.js.map
