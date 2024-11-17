// Generated by purs version 0.15.15
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Lazy from "../Control.Lazy/index.js";
import * as Control_Monad_Error_Class from "../Control.Monad.Error.Class/index.js";
import * as Control_Monad_Rec_Class from "../Control.Monad.Rec.Class/index.js";
import * as Control_Plus from "../Control.Plus/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_List from "../Data.List/index.js";
import * as Data_List_Lazy from "../Data.List.Lazy/index.js";
import * as Data_List_NonEmpty from "../Data.List.NonEmpty/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unfoldable from "../Data.Unfoldable/index.js";
import * as Data_Unfoldable1 from "../Data.Unfoldable1/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Parsing from "../Parsing/index.js";
var alt = /* #__PURE__ */ Control_Alt.alt(Parsing.altParserT);
var defer = /* #__PURE__ */ Control_Lazy.defer(Parsing.lazyParserT);
var voidLeft = /* #__PURE__ */ Data_Functor.voidLeft(Parsing.functorParserT);
var pure = /* #__PURE__ */ Control_Applicative.pure(Parsing.applicativeParserT);
var applySecond = /* #__PURE__ */ Control_Apply.applySecond(Parsing.applyParserT);
var tailRecM = /* #__PURE__ */ Control_Monad_Rec_Class.tailRecM(Parsing.monadRecParserT);
var bind = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT);
var mapFlipped = /* #__PURE__ */ Data_Functor.mapFlipped(Parsing.functorParserT);
var map = /* #__PURE__ */ Data_Functor.map(Parsing.functorParserT);
var manyRec = /* #__PURE__ */ Data_List.manyRec(Parsing.monadRecParserT)(Parsing.alternativeParserT);
var $$void = /* #__PURE__ */ Data_Functor["void"](Parsing.functorParserT);
var catchError = /* #__PURE__ */ Control_Monad_Error_Class.catchError(Parsing.monadErrorParseErrorParse);
var throwError = /* #__PURE__ */ Control_Monad_Error_Class.throwError(Parsing.monadThrowParseErrorParse);
var show = /* #__PURE__ */ Data_Show.show(Data_Show.showInt);
var apply = /* #__PURE__ */ Control_Apply.apply(Parsing.applyParserT);
var applyFirst = /* #__PURE__ */ Control_Apply.applyFirst(Parsing.applyParserT);
var empty = /* #__PURE__ */ Control_Plus.empty(Parsing.plusParserT);
var foldl = /* #__PURE__ */ Data_Foldable.foldl(Data_List_Types.foldableList);
var withLazyErrorMessage = function (p) {
    return function (msg) {
        return alt(p)(defer(function (v) {
            return Parsing.fail("Expected " + msg(Data_Unit.unit));
        }));
    };
};
var withErrorMessage = function (p) {
    return function (msg) {
        return alt(p)(Parsing.fail("Expected " + msg));
    };
};
var tryRethrow = function (v) {
    return function (v1, more, lift, $$throw, done) {
        return v(v1, more, lift, function (v2, v3) {
            return $$throw(new Parsing.ParseState(v2.value0, v2.value1, v1.value2), new Parsing.ParseError(v3.value0, v1.value1));
        }, done);
    };
};
var $$try = function (v) {
    return function (v1, more, lift, $$throw, done) {
        return v(v1, more, lift, function (v2, err) {
            return $$throw(new Parsing.ParseState(v2.value0, v2.value1, v1.value2), err);
        }, done);
    };
};
var skipMany1 = function (p) {
    var go = function (v) {
        return alt(voidLeft(p)(new Control_Monad_Rec_Class.Loop(Data_Unit.unit)))(pure(new Control_Monad_Rec_Class.Done(Data_Unit.unit)));
    };
    return applySecond(p)(tailRecM(go)(Data_Unit.unit));
};
var skipMany = function (p) {
    return alt(skipMany1(p))(pure(Data_Unit.unit));
};
var sepEndBy1 = function (p) {
    return function (sep) {
        var go = function (acc) {
            var done = defer(function (v) {
                return pure(new Control_Monad_Rec_Class.Done(Data_List.reverse(acc)));
            });
            var nextOne = bind(sep)(function () {
                return alt(mapFlipped(p)(function (a) {
                    return new Control_Monad_Rec_Class.Loop(new Data_List_Types.Cons(a, acc));
                }))(done);
            });
            return alt(nextOne)(done);
        };
        return bind(p)(function (a) {
            return alt(map(Data_List_NonEmpty["cons$prime"](a))(tailRecM(go)(Data_List_Types.Nil.value)))(pure(Data_List_NonEmpty.singleton(a)));
        });
    };
};
var sepEndBy = function (p) {
    return function (sep) {
        return alt(map(Data_List_NonEmpty.toList)(sepEndBy1(p)(sep)))(pure(Data_List_Types.Nil.value));
    };
};
var sepBy1 = function (p) {
    return function (sep) {
        return bind(p)(function (a) {
            return bind(manyRec(applySecond(sep)(p)))(function (as) {
                return pure(Data_List_NonEmpty["cons$prime"](a)(as));
            });
        });
    };
};
var sepBy = function (p) {
    return function (sep) {
        return alt(map(Data_List_NonEmpty.toList)(sepBy1(p)(sep)))(pure(Data_List_Types.Nil.value));
    };
};
var optional = function (p) {
    return alt($$void(p))(pure(Data_Unit.unit));
};
var option = function (a) {
    return function (p) {
        return alt(p)(pure(a));
    };
};
var optionMaybe = function (p) {
    return option(Data_Maybe.Nothing.value)(map(Data_Maybe.Just.create)(p));
};
var notFollowedBy = function (p) {
    return $$try(alt(applySecond($$try(p))(Parsing.fail("Negated parser succeeded")))(pure(Data_Unit.unit)));
};
var manyTill_ = function (p) {
    return function (end) {
        var go = function (xs) {
            return alt(bind(end)(function (t) {
                return pure(new Control_Monad_Rec_Class.Done(new Data_Tuple.Tuple(Data_List.reverse(xs), t)));
            }))(bind(p)(function (x) {
                return pure(new Control_Monad_Rec_Class.Loop(new Data_List_Types.Cons(x, xs)));
            }));
        };
        return tailRecM(go)(Data_List_Types.Nil.value);
    };
};
var manyTill = function (p) {
    return function (end) {
        var go = function (acc) {
            return alt(mapFlipped(end)(function (v) {
                return new Control_Monad_Rec_Class.Done(Data_List.reverse(acc));
            }))(mapFlipped(p)(function (x) {
                return new Control_Monad_Rec_Class.Loop(new Data_List_Types.Cons(x, acc));
            }));
        };
        return tailRecM(go)(Data_List_Types.Nil.value);
    };
};
var manyIndex = function (from) {
    return function (to) {
        return function (p) {
            var go = function (v) {
                var $72 = v.value0 >= to;
                if ($72) {
                    return pure(new Control_Monad_Rec_Class.Done(new Data_Tuple.Tuple(v.value0, Data_List.reverse(v.value1))));
                };
                return catchError(bind(p(v.value0))(function (x) {
                    return pure(new Control_Monad_Rec_Class.Loop(new Data_Tuple.Tuple(v.value0 + 1 | 0, new Data_List_Types.Cons(x, v.value1))));
                }))(function (e) {
                    var $73 = v.value0 >= from;
                    if ($73) {
                        return pure(new Control_Monad_Rec_Class.Done(new Data_Tuple.Tuple(v.value0, Data_List.reverse(v.value1))));
                    };
                    return throwError(new Parsing.ParseError(Parsing.parseErrorMessage(e) + (" (at least " + (show(from) + (", but only parsed " + (show(v.value0) + ")")))), Parsing.parseErrorPosition(e)));
                });
            };
            var $76 = from > to || from < 0;
            if ($76) {
                return pure(new Data_Tuple.Tuple(0, Data_List_Types.Nil.value));
            };
            return tailRecM(go)(new Data_Tuple.Tuple(0, Data_List_Types.Nil.value));
        };
    };
};
var many1Till_ = function (p) {
    return function (end) {
        return bind(p)(function (x) {
            return bind(manyTill_(p)(end))(function (v) {
                return pure(new Data_Tuple.Tuple(Data_List_NonEmpty["cons$prime"](x)(v.value0), v.value1));
            });
        });
    };
};
var many1Till = function (p) {
    return function (end) {
        return apply(map(Data_List_NonEmpty["cons$prime"])(p))(manyTill(p)(end));
    };
};
var many1 = function (p) {
    return apply(map(Data_List_NonEmpty["cons$prime"])(p))(manyRec(p));
};
var many = manyRec;
var lookAhead = function (v) {
    return function (state1, more, lift, $$throw, done) {
        return v(state1, more, lift, function (v1, err) {
            return $$throw(state1, err);
        }, function (v1, res) {
            return done(state1, res);
        });
    };
};
var endBy1 = function (p) {
    return function (sep) {
        return many1(applyFirst(p)(sep));
    };
};
var endBy = function (p) {
    return function (sep) {
        return manyRec(applyFirst(p)(sep));
    };
};
var choice = function (dictFoldable) {
    var go = function (p1) {
        return function (v) {
            if (v instanceof Data_Maybe.Nothing) {
                return new Data_Maybe.Just(p1);
            };
            if (v instanceof Data_Maybe.Just) {
                return new Data_Maybe.Just(alt(p1)(v.value0));
            };
            throw new Error("Failed pattern match at Parsing.Combinators (line 358, column 11 - line 360, column 32): " + [ v.constructor.name ]);
        };
    };
    var $95 = Data_Maybe.fromMaybe(empty);
    var $96 = Data_Foldable.foldr(dictFoldable)(go)(Data_Maybe.Nothing.value);
    return function ($97) {
        return $95($96($97));
    };
};
var chainr1 = function (p) {
    return function (f) {
        var apply1 = function (y) {
            return function (v) {
                return v.value1(v.value0)(y);
            };
        };
        var go = function (v) {
            return alt(bind(f)(function (op) {
                return bind(p)(function (a) {
                    return pure(new Control_Monad_Rec_Class.Loop({
                        last: a,
                        init: new Data_List_Types.Cons(new Data_Tuple.Tuple(v.last, op), v.init)
                    }));
                });
            }))(defer(function (v1) {
                return pure(new Control_Monad_Rec_Class.Done(foldl(apply1)(v.last)(v.init)));
            }));
        };
        return bind(p)(function (a) {
            return tailRecM(go)({
                last: a,
                init: Data_List_Types.Nil.value
            });
        });
    };
};
var chainr = function (p) {
    return function (f) {
        return function (a) {
            return alt(chainr1(p)(f))(pure(a));
        };
    };
};
var chainl1 = function (p) {
    return function (f) {
        var go = function (a) {
            return alt(bind(f)(function (op) {
                return bind(p)(function (a$prime) {
                    return pure(new Control_Monad_Rec_Class.Loop(op(a)(a$prime)));
                });
            }))(pure(new Control_Monad_Rec_Class.Done(a)));
        };
        return bind(p)(function (a) {
            return tailRecM(go)(a);
        });
    };
};
var chainl = function (p) {
    return function (f) {
        return function (a) {
            return alt(chainl1(p)(f))(pure(a));
        };
    };
};
var between = function (open) {
    return function (close) {
        return function (p) {
            return applyFirst(applySecond(open)(p))(close);
        };
    };
};
var asErrorMessage = /* #__PURE__ */ Data_Function.flip(withErrorMessage);
var advance = function (p) {
    return bind(Parsing.position)(function (v) {
        return bind(p)(function (x) {
            return bind(Parsing.position)(function (v1) {
                var $92 = v1.index > v.index;
                if ($92) {
                    return pure(x);
                };
                return Parsing.fail("Expected progress");
            });
        });
    });
};
export {
    $$try as try,
    tryRethrow,
    lookAhead,
    choice,
    between,
    notFollowedBy,
    option,
    optionMaybe,
    optional,
    many,
    many1,
    manyTill,
    manyTill_,
    many1Till,
    many1Till_,
    manyIndex,
    skipMany,
    skipMany1,
    sepBy,
    sepBy1,
    sepEndBy,
    sepEndBy1,
    endBy,
    endBy1,
    chainl,
    chainl1,
    chainr,
    chainr1,
    advance,
    withErrorMessage,
    withLazyErrorMessage,
    asErrorMessage
};
export {
    alt,
    empty
} from "../Control.Plus/index.js";
export {
    replicateM
} from "../Data.List.Lazy/index.js";
export {
    replicateA
} from "../Data.Unfoldable/index.js";
export {
    replicate1A
} from "../Data.Unfoldable1/index.js";
//# sourceMappingURL=index.js.map
