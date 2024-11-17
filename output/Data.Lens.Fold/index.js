// Generated by purs version 0.15.15
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_HeytingAlgebra from "../Data.HeytingAlgebra/index.js";
import * as Data_Lens_Internal_Forget from "../Data.Lens.Internal.Forget/index.js";
import * as Data_Lens_Types from "../Data.Lens.Types/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Maybe_First from "../Data.Maybe.First/index.js";
import * as Data_Maybe_Last from "../Data.Maybe.Last/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Monoid_Additive from "../Data.Monoid.Additive/index.js";
import * as Data_Monoid_Conj from "../Data.Monoid.Conj/index.js";
import * as Data_Monoid_Disj from "../Data.Monoid.Disj/index.js";
import * as Data_Monoid_Dual from "../Data.Monoid.Dual/index.js";
import * as Data_Monoid_Endo from "../Data.Monoid.Endo/index.js";
import * as Data_Monoid_Multiplicative from "../Data.Monoid.Multiplicative/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Profunctor from "../Data.Profunctor/index.js";
import * as Data_Profunctor_Choice from "../Data.Profunctor.Choice/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
var $runtime_lazy = function (name, moduleName, init) {
    var state = 0;
    var val;
    return function (lineNumber) {
        if (state === 2) return val;
        if (state === 1) throw new ReferenceError(name + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
        state = 1;
        val = init();
        state = 2;
        return val;
    };
};
var unwrap = /* #__PURE__ */ Data_Newtype.unwrap();
var identity = /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn);
var fromFoldable = /* #__PURE__ */ Data_Array.fromFoldable(Data_List_Types.foldableList);
var unfolded = function (dictMonoid) {
    var mempty = Data_Monoid.mempty(dictMonoid);
    var append = Data_Semigroup.append(dictMonoid.Semigroup0());
    return function (f) {
        return function (p) {
            var $lazy_go = $runtime_lazy("go", "Data.Lens.Fold", function () {
                var $89 = Data_Maybe.maybe(mempty)(function (v) {
                    return append(unwrap(p)(v.value0))($lazy_go(232)(v.value1));
                });
                return function ($90) {
                    return $89(f($90));
                };
            });
            var go = $lazy_go(232);
            return go;
        };
    };
};
var replicated = function (dictMonoid) {
    var mempty = Data_Monoid.mempty(Data_Monoid.monoidFn(dictMonoid));
    var append = Data_Semigroup.append(Data_Semigroup.semigroupFn(dictMonoid.Semigroup0()));
    return function (i) {
        return function (v) {
            var go = function (v1) {
                return function (v2) {
                    if (v1 === 0) {
                        return mempty;
                    };
                    return append(v2)(go(v1 - 1 | 0)(v2));
                };
            };
            return go(i)(v);
        };
    };
};
var ifoldMapOf = function (p) {
    return function (f) {
        return unwrap(p(Data_Tuple.uncurry(f)));
    };
};
var ifoldlOf = function (p) {
    return function (f) {
        return function (r) {
            var $91 = Data_Function.flip(unwrap)(r);
            var $92 = ifoldMapOf(p)(function (i) {
                var $94 = Data_Function.flip(f(i));
                return function ($95) {
                    return Data_Monoid_Dual.Dual(Data_Monoid_Endo.Endo($94($95)));
                };
            });
            return function ($93) {
                return $91(unwrap($92($93)));
            };
        };
    };
};
var ifoldrOf = function (p) {
    return function (f) {
        return function (r) {
            var $96 = Data_Function.flip(unwrap)(r);
            var $97 = ifoldMapOf(p)(function (i) {
                var $99 = f(i);
                return function ($100) {
                    return Data_Monoid_Endo.Endo($99($100));
                };
            });
            return function ($98) {
                return $96($97($98));
            };
        };
    };
};
var itoListOf = function (p) {
    return ifoldrOf(p)(function (i) {
        return function (x) {
            return function (xs) {
                return new Data_List_Types.Cons(new Data_Tuple.Tuple(i, x), xs);
            };
        };
    })(Data_List_Types.Nil.value);
};
var itraverseOf_ = function (dictApplicative) {
    var Apply0 = dictApplicative.Apply0();
    var applySecond = Control_Apply.applySecond(Apply0);
    var $$void = Data_Functor["void"](Apply0.Functor0());
    var pure = Control_Applicative.pure(dictApplicative);
    return function (p) {
        return function (f) {
            return ifoldrOf(p)(function (i) {
                return function (a) {
                    return function (fu) {
                        return applySecond($$void(f(i)(a)))(fu);
                    };
                };
            })(pure(Data_Unit.unit));
        };
    };
};
var iforOf_ = function (dictApplicative) {
    var $101 = itraverseOf_(dictApplicative);
    return function ($102) {
        return Data_Function.flip($101($102));
    };
};
var ifindOf = function (p) {
    return function (f) {
        return ifoldrOf(p)(function (i) {
            return function (a) {
                return Data_Maybe.maybe((function () {
                    var $83 = f(i)(a);
                    if ($83) {
                        return new Data_Maybe.Just(a);
                    };
                    return Data_Maybe.Nothing.value;
                })())(Data_Maybe.Just.create);
            };
        })(Data_Maybe.Nothing.value);
    };
};
var ianyOf = function (dictHeytingAlgebra) {
    return function (p) {
        return function (f) {
            var $103 = ifoldMapOf(p)(function (i) {
                var $105 = f(i);
                return function ($106) {
                    return Data_Monoid_Disj.Disj($105($106));
                };
            });
            return function ($104) {
                return unwrap($103($104));
            };
        };
    };
};
var iallOf = function (dictHeytingAlgebra) {
    return function (p) {
        return function (f) {
            var $107 = ifoldMapOf(p)(function (i) {
                var $109 = f(i);
                return function ($110) {
                    return Data_Monoid_Conj.Conj($109($110));
                };
            });
            return function ($108) {
                return unwrap($107($108));
            };
        };
    };
};
var folded = function (dictMonoid) {
    return function (dictFoldable) {
        var foldMap = Data_Foldable.foldMap(dictFoldable)(dictMonoid);
        return function (v) {
            return foldMap(v);
        };
    };
};
var foldMapOf = /* #__PURE__ */ Data_Newtype.under()()(Data_Lens_Internal_Forget.Forget);
var foldOf = function (p) {
    return foldMapOf(p)(identity);
};
var foldlOf = function (p) {
    return function (f) {
        return function (r) {
            var $111 = Data_Function.flip(unwrap)(r);
            var $112 = foldMapOf(p)((function () {
                var $114 = Data_Function.flip(f);
                return function ($115) {
                    return Data_Monoid_Dual.Dual(Data_Monoid_Endo.Endo($114($115)));
                };
            })());
            return function ($113) {
                return $111(unwrap($112($113)));
            };
        };
    };
};
var foldrOf = function (p) {
    return function (f) {
        return function (r) {
            var $116 = Data_Function.flip(unwrap)(r);
            var $117 = foldMapOf(p)(function ($119) {
                return Data_Monoid_Endo.Endo(f($119));
            });
            return function ($118) {
                return $116($117($118));
            };
        };
    };
};
var maximumOf = function (dictOrd) {
    var greaterThan = Data_Ord.greaterThan(dictOrd);
    return function (p) {
        var max = function (a) {
            return function (b) {
                var $85 = greaterThan(a)(b);
                if ($85) {
                    return a;
                };
                return b;
            };
        };
        return foldrOf(p)(function (a) {
            var $120 = Data_Maybe.maybe(a)(max(a));
            return function ($121) {
                return Data_Maybe.Just.create($120($121));
            };
        })(Data_Maybe.Nothing.value);
    };
};
var minimumOf = function (dictOrd) {
    var lessThan = Data_Ord.lessThan(dictOrd);
    return function (p) {
        var min = function (a) {
            return function (b) {
                var $86 = lessThan(a)(b);
                if ($86) {
                    return a;
                };
                return b;
            };
        };
        return foldrOf(p)(function (a) {
            var $122 = Data_Maybe.maybe(a)(min(a));
            return function ($123) {
                return Data_Maybe.Just.create($122($123));
            };
        })(Data_Maybe.Nothing.value);
    };
};
var toListOf = function (p) {
    return foldrOf(p)(Data_List_Types.Cons.create)(Data_List_Types.Nil.value);
};
var toArrayOf = function (p) {
    var $124 = toListOf(p);
    return function ($125) {
        return fromFoldable($124($125));
    };
};
var toArrayOfOn = function (s) {
    return function (p) {
        return toArrayOf(p)(s);
    };
};
var toListOfOn = function (s) {
    return function (p) {
        return toListOf(p)(s);
    };
};
var traverseOf_ = function (dictApplicative) {
    var Apply0 = dictApplicative.Apply0();
    var applySecond = Control_Apply.applySecond(Apply0);
    var $$void = Data_Functor["void"](Apply0.Functor0());
    var pure = Control_Applicative.pure(dictApplicative);
    return function (p) {
        return function (f) {
            return foldrOf(p)(function (a) {
                return function (fu) {
                    return applySecond($$void(f(a)))(fu);
                };
            })(pure(Data_Unit.unit));
        };
    };
};
var has = function (dictHeytingAlgebra) {
    var tt = Data_HeytingAlgebra.tt(dictHeytingAlgebra);
    return function (p) {
        var $126 = foldMapOf(p)(Data_Function["const"](tt));
        return function ($127) {
            return unwrap($126($127));
        };
    };
};
var hasn$primet = function (dictHeytingAlgebra) {
    var ff = Data_HeytingAlgebra.ff(dictHeytingAlgebra);
    return function (p) {
        var $128 = foldMapOf(p)(Data_Function["const"](ff));
        return function ($129) {
            return unwrap($128($129));
        };
    };
};
var lastOf = function (p) {
    var $130 = foldMapOf(p)(function ($132) {
        return Data_Maybe_Last.Last(Data_Maybe.Just.create($132));
    });
    return function ($131) {
        return unwrap($130($131));
    };
};
var lengthOf = function (p) {
    var $133 = foldMapOf(p)(Data_Function["const"](1));
    return function ($134) {
        return unwrap($133($134));
    };
};
var preview = function (p) {
    var $135 = foldMapOf(p)(function ($137) {
        return Data_Maybe_First.First(Data_Maybe.Just.create($137));
    });
    return function ($136) {
        return unwrap($135($136));
    };
};
var previewOn = function (s) {
    return function (p) {
        return preview(p)(s);
    };
};
var productOf = function (dictSemiring) {
    return function (p) {
        var $138 = foldMapOf(p)(Data_Monoid_Multiplicative.Multiplicative);
        return function ($139) {
            return unwrap($138($139));
        };
    };
};
var sequenceOf_ = function (dictApplicative) {
    var pure = Control_Applicative.pure(dictApplicative);
    var applySecond = Control_Apply.applySecond(dictApplicative.Apply0());
    return function (p) {
        var $140 = Data_Function.flip(unwrap)(pure(Data_Unit.unit));
        var $141 = foldMapOf(p)(function (f) {
            return function (v) {
                return applySecond(f)(v);
            };
        });
        return function ($142) {
            return $140($141($142));
        };
    };
};
var sumOf = function (dictSemiring) {
    return function (p) {
        var $143 = foldMapOf(p)(Data_Monoid_Additive.Additive);
        return function ($144) {
            return unwrap($143($144));
        };
    };
};
var firstOf = function (p) {
    var $145 = foldMapOf(p)(function ($147) {
        return Data_Maybe_First.First(Data_Maybe.Just.create($147));
    });
    return function ($146) {
        return unwrap($145($146));
    };
};
var findOf = function (p) {
    return function (f) {
        return foldrOf(p)(function (a) {
            return Data_Maybe.maybe((function () {
                var $87 = f(a);
                if ($87) {
                    return new Data_Maybe.Just(a);
                };
                return Data_Maybe.Nothing.value;
            })())(Data_Maybe.Just.create);
        })(Data_Maybe.Nothing.value);
    };
};
var filtered = function (dictChoice) {
    var right = Data_Profunctor_Choice.right(dictChoice);
    var dimap = Data_Profunctor.dimap(dictChoice.Profunctor0());
    return function (f) {
        var $148 = dimap(function (x) {
            var $88 = f(x);
            if ($88) {
                return new Data_Either.Right(x);
            };
            return new Data_Either.Left(x);
        })(Data_Either.either(identity)(identity));
        return function ($149) {
            return $148(right($149));
        };
    };
};
var anyOf = function (dictHeytingAlgebra) {
    return function (p) {
        return function (f) {
            var $150 = foldMapOf(p)(function ($152) {
                return Data_Monoid_Disj.Disj(f($152));
            });
            return function ($151) {
                return unwrap($150($151));
            };
        };
    };
};
var anyOf1 = /* #__PURE__ */ anyOf(Data_HeytingAlgebra.heytingAlgebraBoolean);
var elemOf = function (dictEq) {
    var eq = Data_Eq.eq(dictEq);
    return function (p) {
        return function (a) {
            return anyOf1(p)(function (v) {
                return eq(v)(a);
            });
        };
    };
};
var orOf = function (dictHeytingAlgebra) {
    var anyOf2 = anyOf(dictHeytingAlgebra);
    return function (p) {
        return anyOf2(p)(identity);
    };
};
var allOf = function (dictHeytingAlgebra) {
    return function (p) {
        return function (f) {
            var $153 = foldMapOf(p)(function ($155) {
                return Data_Monoid_Conj.Conj(f($155));
            });
            return function ($154) {
                return unwrap($153($154));
            };
        };
    };
};
var allOf1 = /* #__PURE__ */ allOf(Data_HeytingAlgebra.heytingAlgebraBoolean);
var andOf = function (dictHeytingAlgebra) {
    var allOf2 = allOf(dictHeytingAlgebra);
    return function (p) {
        return allOf2(p)(identity);
    };
};
var notElemOf = function (dictEq) {
    var notEq = Data_Eq.notEq(dictEq);
    return function (p) {
        return function (a) {
            return allOf1(p)(function (v) {
                return notEq(v)(a);
            });
        };
    };
};
export {
    previewOn,
    toListOfOn,
    preview,
    foldOf,
    foldMapOf,
    foldrOf,
    foldlOf,
    toListOf,
    firstOf,
    lastOf,
    maximumOf,
    minimumOf,
    allOf,
    anyOf,
    andOf,
    orOf,
    elemOf,
    notElemOf,
    sumOf,
    productOf,
    lengthOf,
    findOf,
    sequenceOf_,
    traverseOf_,
    has,
    hasn$primet,
    replicated,
    filtered,
    folded,
    unfolded,
    toArrayOf,
    toArrayOfOn,
    ifoldMapOf,
    ifoldrOf,
    ifoldlOf,
    iallOf,
    ianyOf,
    ifindOf,
    itoListOf,
    itraverseOf_,
    iforOf_
};
//# sourceMappingURL=index.js.map