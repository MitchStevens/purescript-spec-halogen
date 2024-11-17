// Generated by purs version 0.15.15
import * as $foreign from "./foreign.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect_Aff from "../Effect.Aff/index.js";
import * as Signal_Channel from "../Signal.Channel/index.js";
var pure = /* #__PURE__ */ Control_Applicative.pure(Effect.applicativeEffect);
var sendEither = function (chan) {
    return Data_Either.either(Data_Function["const"](pure(Data_Unit.unit)))((function () {
        var $2 = Signal_Channel.send(chan);
        return function ($3) {
            return $2(Data_Maybe.Just.create($3));
        };
    })());
};
var mkChannel = /* #__PURE__ */ (function () {
    return Signal_Channel.channel(Data_Maybe.Nothing.value);
})();
var mapAff = function (action) {
    return $foreign.mapAffP(Effect_Aff.runAff_)(mkChannel)(sendEither)(action);
};
export {
    mapAff
};
//# sourceMappingURL=index.js.map
