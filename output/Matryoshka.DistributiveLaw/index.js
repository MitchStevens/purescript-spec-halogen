// Generated by purs version 0.15.15
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Control_Comonad from "../Control.Comonad/index.js";
import * as Control_Comonad_Cofree from "../Control.Comonad.Cofree/index.js";
import * as Control_Comonad_Env_Trans from "../Control.Comonad.Env.Trans/index.js";
import * as Control_Comonad_Trans_Class from "../Control.Comonad.Trans.Class/index.js";
import * as Control_Monad_Except_Trans from "../Control.Monad.Except.Trans/index.js";
import * as Control_Monad_Free from "../Control.Monad.Free/index.js";
import * as Data_Distributive from "../Data.Distributive/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Traversable from "../Data.Traversable/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Matryoshka_Class_Corecursive from "../Matryoshka.Class.Corecursive/index.js";
import * as Matryoshka_Class_Recursive from "../Matryoshka.Class.Recursive/index.js";
var lower = /* #__PURE__ */ Control_Comonad_Trans_Class.lower(Control_Comonad_Env_Trans.comonadTransEnvT);
var identity = /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn);
var join = /* #__PURE__ */ Control_Bind.join(Control_Monad_Free.freeBind);
var pure = /* #__PURE__ */ Control_Applicative.pure(Control_Monad_Free.freeApplicative);
var wrap = /* #__PURE__ */ Data_Newtype.wrap();
var unwrap = /* #__PURE__ */ Data_Newtype.unwrap();
var distZygoT = function (dictFunctor) {
    var map = Data_Functor.map(dictFunctor);
    return function (dictComonad) {
        var lower1 = lower(dictComonad);
        return function (g) {
            return function (k) {
                return function (fe) {
                    return new Data_Tuple.Tuple(g(map(function ($68) {
                        return Data_Tuple.fst(Control_Comonad_Env_Trans.runEnvT($68));
                    })(fe)), k(map(lower1)(fe)));
                };
            };
        };
    };
};
var distZygo = function (dictFunctor) {
    var map = Data_Functor.map(dictFunctor);
    return function (g) {
        return function (m) {
            return new Data_Tuple.Tuple(g(map(Data_Tuple.fst)(m)), map(Data_Tuple.snd)(m));
        };
    };
};
var distParaT = function (dictCorecursive) {
    var distZygoT1 = distZygoT(dictCorecursive.Functor0());
    var embed = Matryoshka_Class_Corecursive.embed(dictCorecursive);
    return function (dictComonad) {
        return distZygoT1(dictComonad)(embed);
    };
};
var distPara = function (dictCorecursive) {
    return distZygo(dictCorecursive.Functor0())(Matryoshka_Class_Corecursive.embed(dictCorecursive));
};
var distGHisto = function (dictFunctor) {
    var map = Data_Functor.map(dictFunctor);
    return function (dictFunctor1) {
        var buildCofree = Control_Comonad_Cofree.buildCofree(dictFunctor1);
        var extract = Control_Comonad.extract(Control_Comonad_Cofree.comonadCofree(dictFunctor1));
        return function (k) {
            return buildCofree(function (s) {
                return new Data_Tuple.Tuple(map(extract)(s), k(map(Control_Comonad_Cofree.tail)(s)));
            });
        };
    };
};
var distHisto = function (dictFunctor) {
    return distGHisto(dictFunctor)(dictFunctor)(identity);
};
var distGFutu = function (dictFunctor) {
    var map = Data_Functor.map(dictFunctor);
    return function (dictFunctor1) {
        var resume = Control_Monad_Free.resume(dictFunctor1);
        var map1 = Data_Functor.map(dictFunctor1);
        return function (k) {
            return function (f) {
                var v = resume(f);
                if (v instanceof Data_Either.Left) {
                    return map(function ($69) {
                        return join(Control_Monad_Free.liftF($69));
                    })(k(map1(distGFutu(dictFunctor)(dictFunctor1)(k))(v.value0)));
                };
                if (v instanceof Data_Either.Right) {
                    return map(pure)(v.value0);
                };
                throw new Error("Failed pattern match at Matryoshka.DistributiveLaw (line 113, column 17 - line 115, column 24): " + [ v.constructor.name ]);
            };
        };
    };
};
var distGApo = function (dictFunctor) {
    var map = Data_Functor.map(dictFunctor);
    return function (f) {
        return Data_Either.either((function () {
            var $70 = map(Data_Either.Left.create);
            return function ($71) {
                return $70(f($71));
            };
        })())(map(Data_Either.Right.create));
    };
};
var distGApoT = function (dictFunctor) {
    var map = Data_Functor.map(dictFunctor);
    var distGApo1 = distGApo(dictFunctor);
    return function (dictFunctor1) {
        var map1 = Data_Functor.map(dictFunctor1);
        return function (g) {
            return function (k) {
                var $72 = map(Control_Monad_Except_Trans.ExceptT);
                var $73 = map1(distGApo1(g));
                return function ($74) {
                    return $72(k($73(Control_Monad_Except_Trans.runExceptT($74))));
                };
            };
        };
    };
};
var distFutu = function (dictFunctor) {
    return distGFutu(dictFunctor)(dictFunctor)(identity);
};
var distDistributive = function (dictTraversable) {
    var Functor0 = dictTraversable.Functor0();
    return function (dictDistributive) {
        return Data_Distributive.distribute(dictDistributive)(Functor0);
    };
};
var distCata = function (dictFunctor) {
    var $75 = Data_Functor.map(dictFunctor)(unwrap);
    return function ($76) {
        return wrap($75($76));
    };
};
var distApplicative = function (dictTraversable) {
    var sequence = Data_Traversable.sequence(dictTraversable);
    return function (dictApplicative) {
        return sequence(dictApplicative);
    };
};
var distApo = function (dictRecursive) {
    return distGApo(dictRecursive.Functor0())(Matryoshka_Class_Recursive.project(dictRecursive));
};
var distAna = function (dictFunctor) {
    var $77 = Data_Functor.map(dictFunctor)(wrap);
    return function ($78) {
        return $77(unwrap($78));
    };
};
export {
    distApplicative,
    distDistributive,
    distCata,
    distPara,
    distParaT,
    distZygo,
    distZygoT,
    distHisto,
    distGHisto,
    distAna,
    distApo,
    distGApo,
    distGApoT,
    distFutu,
    distGFutu
};
//# sourceMappingURL=index.js.map