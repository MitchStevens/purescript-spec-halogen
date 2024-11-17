// Generated by purs version 0.15.15
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad_State_Class from "../Control.Monad.State.Class/index.js";
import * as Control_Monad_State_Trans from "../Control.Monad.State.Trans/index.js";
import * as Control_Monad_Writer_Class from "../Control.Monad.Writer.Class/index.js";
import * as Control_Monad_Writer_Trans from "../Control.Monad.Writer.Trans/index.js";
import * as Data_EuclideanRing from "../Data.EuclideanRing/index.js";
import * as Data_Identity from "../Data.Identity/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Test_Spec_Console from "../Test.Spec.Console/index.js";
import * as Test_Spec_Reporter_Base from "../Test.Spec.Reporter.Base/index.js";
import * as Test_Spec_Result from "../Test.Spec.Result/index.js";
import * as Test_Spec_Runner_Event from "../Test.Spec.Runner.Event/index.js";
import * as Test_Spec_Speed from "../Test.Spec.Speed/index.js";
import * as Test_Spec_Style from "../Test.Spec.Style/index.js";
var monadWriterT = /* #__PURE__ */ Control_Monad_Writer_Trans.monadWriterT(Data_Monoid.monoidString)(Data_Identity.monadIdentity);
var bindStateT = /* #__PURE__ */ Control_Monad_State_Trans.bindStateT(monadWriterT);
var bind = /* #__PURE__ */ Control_Bind.bind(bindStateT);
var modify = /* #__PURE__ */ Control_Monad_State_Class.modify(/* #__PURE__ */ Control_Monad_State_Trans.monadStateStateT(monadWriterT));
var discard = /* #__PURE__ */ Control_Bind.discard(Control_Bind.discardUnit)(bindStateT);
var applicativeStateT = /* #__PURE__ */ Control_Monad_State_Trans.applicativeStateT(monadWriterT);
var when = /* #__PURE__ */ Control_Applicative.when(applicativeStateT);
var mod = /* #__PURE__ */ Data_EuclideanRing.mod(Data_EuclideanRing.euclideanRingInt);
var tellLn = /* #__PURE__ */ Test_Spec_Console.tellLn(/* #__PURE__ */ Control_Monad_State_Trans.monadWriterStateT(/* #__PURE__ */ Control_Monad_Writer_Trans.monadWriterWriterT(Data_Monoid.monoidString)(Data_Identity.monadIdentity)));
var tell = /* #__PURE__ */ Control_Monad_Writer_Class.tell(/* #__PURE__ */ Control_Monad_State_Trans.monadTellStateT(/* #__PURE__ */ Control_Monad_Writer_Trans.monadTellWriterT(Data_Monoid.monoidString)(Data_Identity.monadIdentity)));
var pure = /* #__PURE__ */ Control_Applicative.pure(applicativeStateT);
var dotReporter = function (v) {
    var wrap = function (action) {
        return bind(modify(function (v1) {
            return v1 + 1 | 0;
        }))(function (n) {
            return discard(when(mod(n)(v.width) === 0)(tellLn("")))(function () {
                return tell(action);
            });
        });
    };
    return Test_Spec_Reporter_Base.defaultReporter(-1 | 0)(function (v1) {
        if (v1 instanceof Test_Spec_Runner_Event.TestEnd && v1.value2 instanceof Test_Spec_Result.Success) {
            return wrap(Test_Spec_Style.styled(Test_Spec_Speed.toStyle(v1.value2.value0))("."));
        };
        if (v1 instanceof Test_Spec_Runner_Event.TestEnd && v1.value2 instanceof Test_Spec_Result.Failure) {
            return wrap(Test_Spec_Style.styled(Test_Spec_Style.red)("!"));
        };
        if (v1 instanceof Test_Spec_Runner_Event.Pending) {
            return wrap(Test_Spec_Style.styled(Test_Spec_Style.dim)(","));
        };
        if (v1 instanceof Test_Spec_Runner_Event.End) {
            return tellLn("");
        };
        return pure(Data_Unit.unit);
    });
};
export {
    dotReporter
};
//# sourceMappingURL=index.js.map
