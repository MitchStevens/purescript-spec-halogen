// Generated by purs version 0.15.15
import * as Control_Comonad from "../Control.Comonad/index.js";
import * as Control_Comonad_Env_Trans from "../Control.Comonad.Env.Trans/index.js";
import * as Control_Comonad_Store_Trans from "../Control.Comonad.Store.Trans/index.js";
import * as Control_Comonad_Traced_Trans from "../Control.Comonad.Traced.Trans/index.js";
import * as Control_Comonad_Trans_Class from "../Control.Comonad.Trans.Class/index.js";
import * as Control_Monad_Identity_Trans from "../Control.Monad.Identity.Trans/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
var track = function (dict) {
    return dict.track;
};
var tracks = function (dictComonadTraced) {
    var track1 = track(dictComonadTraced);
    var extract = Control_Comonad.extract(dictComonadTraced.Comonad0());
    return function (f) {
        return function (w) {
            return track1(f(extract(w)))(w);
        };
    };
};
var lowerTrack = function (dictComonadTrans) {
    var lower = Control_Comonad_Trans_Class.lower(dictComonadTrans);
    return function (dictComonadTraced) {
        var track1 = track(dictComonadTraced);
        var lower1 = lower(dictComonadTraced.Comonad0());
        return function (m) {
            var $51 = track1(m);
            return function ($52) {
                return $51(lower1($52));
            };
        };
    };
};
var lowerTrack1 = /* #__PURE__ */ lowerTrack(Control_Comonad_Store_Trans.comonadTransStoreT);
var lowerTrack2 = /* #__PURE__ */ lowerTrack(Control_Comonad_Trans_Class.comonadTransIdentityT);
var lowerTrack3 = /* #__PURE__ */ lowerTrack(Control_Comonad_Env_Trans.comonadTransEnvT);
var listens = function (dictFunctor) {
    var map = Data_Functor.map(dictFunctor);
    return function (f) {
        return function (v) {
            return map(function (g) {
                return function (t) {
                    return new Data_Tuple.Tuple(g(t), f(t));
                };
            })(v);
        };
    };
};
var listen = function (dictFunctor) {
    var map = Data_Functor.map(dictFunctor);
    return function (v) {
        return map(function (f) {
            return function (t) {
                return new Data_Tuple.Tuple(f(t), t);
            };
        })(v);
    };
};
var comonadTracedTracedT = function (dictComonad) {
    var extract = Control_Comonad.extract(dictComonad);
    var comonadTracedT = Control_Comonad_Traced_Trans.comonadTracedT(dictComonad);
    return function (dictMonoid) {
        var comonadTracedT1 = comonadTracedT(dictMonoid);
        return {
            track: function (t) {
                return function (v) {
                    return extract(v)(t);
                };
            },
            Comonad0: function () {
                return comonadTracedT1;
            }
        };
    };
};
var comonadTracedStoreT = function (dictComonadTraced) {
    var comonadStoreT = Control_Comonad_Store_Trans.comonadStoreT(dictComonadTraced.Comonad0());
    return {
        track: lowerTrack1(dictComonadTraced),
        Comonad0: function () {
            return comonadStoreT;
        }
    };
};
var comonadTracedIdentityT = function (dictComonadTraced) {
    var comonadIdentityT = Control_Monad_Identity_Trans.comonadIdentityT(dictComonadTraced.Comonad0());
    return {
        track: lowerTrack2(dictComonadTraced),
        Comonad0: function () {
            return comonadIdentityT;
        }
    };
};
var comonadTracedEnvT = function (dictComonadTraced) {
    var comonadEnvT = Control_Comonad_Env_Trans.comonadEnvT(dictComonadTraced.Comonad0());
    return {
        track: lowerTrack3(dictComonadTraced),
        Comonad0: function () {
            return comonadEnvT;
        }
    };
};
var censor = function (dictFunctor) {
    var map = Data_Functor.map(dictFunctor);
    return function (f) {
        return function (v) {
            return map(function (v1) {
                return function ($53) {
                    return v1(f($53));
                };
            })(v);
        };
    };
};
export {
    track,
    tracks,
    listen,
    listens,
    censor,
    comonadTracedTracedT,
    comonadTracedIdentityT,
    comonadTracedEnvT,
    comonadTracedStoreT
};
//# sourceMappingURL=index.js.map