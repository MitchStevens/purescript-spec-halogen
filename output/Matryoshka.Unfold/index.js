// Generated by purs version 0.15.15
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Control_Monad_Free from "../Control.Monad.Free/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Identity from "../Data.Identity/index.js";
import * as Data_Traversable from "../Data.Traversable/index.js";
import * as Matryoshka_Class_Corecursive from "../Matryoshka.Class.Corecursive/index.js";
import * as Matryoshka_Class_Recursive from "../Matryoshka.Class.Recursive/index.js";
import * as Matryoshka_DistributiveLaw from "../Matryoshka.DistributiveLaw/index.js";
import * as Matryoshka_Util from "../Matryoshka.Util/index.js";
var identity = /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn);
var transApoT = function (dictRecursive) {
    var mapR = Matryoshka_Util.mapR(dictRecursive);
    return function (dictCorecursive) {
        var mapR1 = mapR(dictCorecursive);
        var map = Data_Functor.map(dictCorecursive.Functor0());
        return function (f) {
            var go = function (t) {
                return Data_Either.either(identity)(mapR1(map(go)))(f(t));
            };
            return go;
        };
    };
};
var transApo = function (dictRecursive) {
    var mapR = Matryoshka_Util.mapR(dictRecursive);
    return function (dictCorecursive) {
        var mapR1 = mapR(dictCorecursive);
        var map = Data_Functor.map(dictCorecursive.Functor0());
        return function (f) {
            var go = function (t) {
                return mapR1((function () {
                    var $198 = map(Data_Either.either(identity)(go));
                    return function ($199) {
                        return $198(f($199));
                    };
                })())(t);
            };
            return go;
        };
    };
};
var transAnaTM = function (dictRecursive) {
    var traverseR = Matryoshka_Util.traverseR(dictRecursive);
    return function (dictCorecursive) {
        var traverseR1 = traverseR(dictCorecursive);
        return function (dictMonad) {
            var Bind1 = dictMonad.Bind1();
            var bindFlipped = Control_Bind.bindFlipped(Bind1);
            var traverseR2 = traverseR1((Bind1.Apply0()).Functor0());
            var Applicative0 = dictMonad.Applicative0();
            return function (dictTraversable) {
                var traverse = Data_Traversable.traverse(dictTraversable)(Applicative0);
                return function (f) {
                    var go = function (t) {
                        return bindFlipped(traverseR2(traverse(go)))(f(t));
                    };
                    return go;
                };
            };
        };
    };
};
var transAnaT = function (dictRecursive) {
    var mapR = Matryoshka_Util.mapR(dictRecursive);
    return function (dictCorecursive) {
        var mapR1 = mapR(dictCorecursive);
        var map = Data_Functor.map(dictCorecursive.Functor0());
        return function (f) {
            var go = function (t) {
                return mapR1(map(go))(f(t));
            };
            return go;
        };
    };
};
var transAnaM = function (dictRecursive) {
    var traverseR = Matryoshka_Util.traverseR(dictRecursive);
    return function (dictCorecursive) {
        var traverseR1 = traverseR(dictCorecursive);
        return function (dictMonad) {
            var Bind1 = dictMonad.Bind1();
            var traverseR2 = traverseR1((Bind1.Apply0()).Functor0());
            var composeKleisliFlipped = Control_Bind.composeKleisliFlipped(Bind1);
            var Applicative0 = dictMonad.Applicative0();
            return function (dictTraversable) {
                var traverse = Data_Traversable.traverse(dictTraversable)(Applicative0);
                return function (f) {
                    var go = function (t) {
                        return traverseR2(composeKleisliFlipped(traverse(go))(f))(t);
                    };
                    return go;
                };
            };
        };
    };
};
var transAna = function (dictRecursive) {
    var mapR = Matryoshka_Util.mapR(dictRecursive);
    return function (dictCorecursive) {
        var mapR1 = mapR(dictCorecursive);
        var map = Data_Functor.map(dictCorecursive.Functor0());
        return function (f) {
            var go = function (t) {
                return mapR1((function () {
                    var $200 = map(go);
                    return function ($201) {
                        return $200(f($201));
                    };
                })())(t);
            };
            return go;
        };
    };
};
var transPostpro = function (dictRecursive) {
    var mapR = Matryoshka_Util.mapR(dictRecursive);
    return function (dictRecursive1) {
        var transAna1 = transAna(dictRecursive1);
        return function (dictCorecursive) {
            var mapR1 = mapR(dictCorecursive);
            var map = Data_Functor.map(dictCorecursive.Functor0());
            var transAna2 = transAna1(dictCorecursive);
            return function (f) {
                return function (g) {
                    var go = function (t) {
                        return mapR1((function () {
                            var $202 = map((function () {
                                var $204 = transAna2(f);
                                return function ($205) {
                                    return $204(go($205));
                                };
                            })());
                            return function ($203) {
                                return $202(g($203));
                            };
                        })())(t);
                    };
                    return go;
                };
            };
        };
    };
};
var ganaM = function (dictCorecursive) {
    var embed = Matryoshka_Class_Corecursive.embed(dictCorecursive);
    return function (dictMonad) {
        var Bind1 = dictMonad.Bind1();
        var map = Data_Functor.map((Bind1.Apply0()).Functor0());
        var Applicative0 = dictMonad.Applicative0();
        var composeKleisliFlipped = Control_Bind.composeKleisliFlipped(Bind1);
        return function (dictMonad1) {
            var join = Control_Bind.join(dictMonad1.Bind1());
            var pure = Control_Applicative.pure(dictMonad1.Applicative0());
            return function (dictTraversable) {
                var traverse = Data_Traversable.traverse(dictTraversable)(Applicative0);
                return function (dictTraversable1) {
                    var traverse1 = Data_Traversable.traverse(dictTraversable1)(Applicative0);
                    return function (k) {
                        return function (f) {
                            var go = function (a) {
                                return map(embed)(traverse(composeKleisliFlipped(go)((function () {
                                    var $206 = traverse1(f);
                                    return function ($207) {
                                        return $206(join($207));
                                    };
                                })()))(k(a)));
                            };
                            return composeKleisliFlipped(go)((function () {
                                var $208 = map(pure);
                                return function ($209) {
                                    return $208(f($209));
                                };
                            })());
                        };
                    };
                };
            };
        };
    };
};
var gana = function (dictCorecursive) {
    var embed = Matryoshka_Class_Corecursive.embed(dictCorecursive);
    var map = Data_Functor.map(dictCorecursive.Functor0());
    return function (dictMonad) {
        var Bind1 = dictMonad.Bind1();
        var map1 = Data_Functor.map((Bind1.Apply0()).Functor0());
        var join = Control_Bind.join(Bind1);
        var pure = Control_Applicative.pure(dictMonad.Applicative0());
        return function (k) {
            return function (f) {
                var go = function (a) {
                    return embed(map((function () {
                        var $210 = map1(f);
                        return function ($211) {
                            return go($210(join($211)));
                        };
                    })())(k(a)));
                };
                return function ($212) {
                    return go(pure(f($212)));
                };
            };
        };
    };
};
var futuM = function (dictCorecursive) {
    var embed = Matryoshka_Class_Corecursive.embed(dictCorecursive);
    var resume = Control_Monad_Free.resume(dictCorecursive.Functor0());
    return function (dictMonad) {
        var Bind1 = dictMonad.Bind1();
        var map = Data_Functor.map((Bind1.Apply0()).Functor0());
        var Applicative0 = dictMonad.Applicative0();
        var bindFlipped = Control_Bind.bindFlipped(Bind1);
        return function (dictTraversable) {
            var traverse = Data_Traversable.traverse(dictTraversable)(Applicative0);
            return function (f) {
                var loop = function (x) {
                    return Data_Either.either((function () {
                        var $213 = map(embed);
                        var $214 = traverse(loop);
                        return function ($215) {
                            return $213($214($215));
                        };
                    })())(go)(resume(x));
                };
                var go = function (a) {
                    return bindFlipped((function () {
                        var $216 = map(embed);
                        var $217 = traverse(loop);
                        return function ($218) {
                            return $216($217($218));
                        };
                    })())(f(a));
                };
                return go;
            };
        };
    };
};
var futu = function (dictCorecursive) {
    return gana(dictCorecursive)(Control_Monad_Free.freeMonad)(Matryoshka_DistributiveLaw.distFutu(dictCorecursive.Functor0()));
};
var elgotApo = function (dictCorecursive) {
    var embed = Matryoshka_Class_Corecursive.embed(dictCorecursive);
    var map = Data_Functor.map(dictCorecursive.Functor0());
    return function (f) {
        var go = function (a) {
            return Data_Either.either(identity)((function () {
                var $219 = map(go);
                return function ($220) {
                    return embed($219($220));
                };
            })())(f(a));
        };
        return go;
    };
};
var elgotAna = function (dictCorecursive) {
    var embed = Matryoshka_Class_Corecursive.embed(dictCorecursive);
    var map = Data_Functor.map(dictCorecursive.Functor0());
    return function (dictMonad) {
        var bindFlipped = Control_Bind.bindFlipped(dictMonad.Bind1());
        return function (k) {
            return function (f) {
                var go = function (a) {
                    return embed(map(function ($221) {
                        return go((function (v) {
                            return bindFlipped(f)(v);
                        })($221));
                    })(k(a)));
                };
                return function ($222) {
                    return go(f($222));
                };
            };
        };
    };
};
var elgotFutu = function (dictCorecursive) {
    return elgotAna(dictCorecursive)(Control_Monad_Free.freeMonad)(Matryoshka_DistributiveLaw.distFutu(dictCorecursive.Functor0()));
};
var apoM = function (dictCorecursive) {
    var embed = Matryoshka_Class_Corecursive.embed(dictCorecursive);
    return function (dictMonad) {
        var Bind1 = dictMonad.Bind1();
        var map = Data_Functor.map((Bind1.Apply0()).Functor0());
        var bindFlipped = Control_Bind.bindFlipped(Bind1);
        var Applicative0 = dictMonad.Applicative0();
        var pure = Control_Applicative.pure(Applicative0);
        return function (dictTraversable) {
            var traverse = Data_Traversable.traverse(dictTraversable)(Applicative0);
            return function (f) {
                var go = function (a) {
                    return map(embed)(bindFlipped(traverse(Data_Either.either(pure)(go)))(f(a)));
                };
                return go;
            };
        };
    };
};
var apo = function (dictCorecursive) {
    var embed = Matryoshka_Class_Corecursive.embed(dictCorecursive);
    var map = Data_Functor.map(dictCorecursive.Functor0());
    return function (f) {
        var go = function (a) {
            return embed(map(Data_Either.either(identity)(go))(f(a)));
        };
        return go;
    };
};
var anaM = function (dictCorecursive) {
    var embed = Matryoshka_Class_Corecursive.embed(dictCorecursive);
    return function (dictMonad) {
        var Bind1 = dictMonad.Bind1();
        var bind = Control_Bind.bind(Bind1);
        var map = Data_Functor.map((Bind1.Apply0()).Functor0());
        var Applicative0 = dictMonad.Applicative0();
        return function (dictTraversable) {
            var traverse = Data_Traversable.traverse(dictTraversable)(Applicative0);
            return function (f) {
                var go = function (a) {
                    return bind(f(a))((function () {
                        var $223 = map(embed);
                        var $224 = traverse(go);
                        return function ($225) {
                            return $223($224($225));
                        };
                    })());
                };
                return go;
            };
        };
    };
};
var ana = function (dictCorecursive) {
    var embed = Matryoshka_Class_Corecursive.embed(dictCorecursive);
    var map = Data_Functor.map(dictCorecursive.Functor0());
    return function (f) {
        var go = function (a) {
            return embed(map(go)(f(a)));
        };
        return go;
    };
};
var colambek = function (dictRecursive) {
    var project = Matryoshka_Class_Recursive.project(dictRecursive);
    return function (dictCorecursive) {
        return ana(dictCorecursive)(Data_Functor.map(dictCorecursive.Functor0())(project));
    };
};
var gapo = function (dictCorecursive) {
    var ana1 = ana(dictCorecursive);
    var embed = Matryoshka_Class_Corecursive.embed(dictCorecursive);
    var map = Data_Functor.map(dictCorecursive.Functor0());
    return function (f) {
        return function (g) {
            var anaf = ana1(f);
            var go = function (a) {
                return embed(map(Data_Either.either(anaf)(go))(g(a)));
            };
            return go;
        };
    };
};
var gpostpro = function (dictRecursive) {
    var project = Matryoshka_Class_Recursive.project(dictRecursive);
    return function (dictCorecursive) {
        var embed = Matryoshka_Class_Corecursive.embed(dictCorecursive);
        var map = Data_Functor.map(dictCorecursive.Functor0());
        var ana1 = ana(dictCorecursive);
        return function (dictMonad) {
            var Bind1 = dictMonad.Bind1();
            var join = Control_Bind.join(Bind1);
            var map1 = Data_Functor.map((Bind1.Apply0()).Functor0());
            var pure = Control_Applicative.pure(dictMonad.Applicative0());
            return function (f) {
                return function (g) {
                    return function (h) {
                        var go = function (na) {
                            return embed(map((function () {
                                var $226 = ana1(function ($228) {
                                    return g(project($228));
                                });
                                return function ($227) {
                                    return $226(go(join($227)));
                                };
                            })())(f(map1(h)(na))));
                        };
                        return function ($229) {
                            return go(pure($229));
                        };
                    };
                };
            };
        };
    };
};
var postpro = function (dictRecursive) {
    var gpostpro1 = gpostpro(dictRecursive);
    return function (dictCorecursive) {
        var gpostpro2 = gpostpro1(dictCorecursive)(Data_Identity.monadIdentity);
        var Functor0 = dictCorecursive.Functor0();
        var distAna = Matryoshka_DistributiveLaw.distAna(Functor0);
        var map = Data_Functor.map(Functor0);
        return function (f) {
            return function (g) {
                return gpostpro2(distAna)(f)((function () {
                    var $230 = map(Data_Identity.Identity);
                    return function ($231) {
                        return $230(g($231));
                    };
                })());
            };
        };
    };
};
export {
    ana,
    anaM,
    gana,
    ganaM,
    elgotAna,
    transAna,
    transAnaT,
    transAnaM,
    transAnaTM,
    postpro,
    gpostpro,
    transPostpro,
    apo,
    gapo,
    apoM,
    elgotApo,
    transApo,
    transApoT,
    futu,
    elgotFutu,
    futuM,
    colambek
};
//# sourceMappingURL=index.js.map