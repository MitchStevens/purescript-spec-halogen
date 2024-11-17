// Generated by purs version 0.15.15
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Lens_Record from "../Data.Lens.Record/index.js";
import * as Data_Lens_Setter from "../Data.Lens.Setter/index.js";
import * as Data_Map_Internal from "../Data.Map.Internal/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Profunctor_Strong from "../Data.Profunctor.Strong/index.js";
import * as Data_Semiring from "../Data.Semiring/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Show_Generic from "../Data.Show.Generic/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Halogen_Component from "../Halogen.Component/index.js";
import * as Halogen_HTML from "../Halogen.HTML/index.js";
import * as Halogen_HTML_Elements from "../Halogen.HTML.Elements/index.js";
import * as Halogen_Query from "../Halogen.Query/index.js";
import * as Halogen_Query_HalogenM from "../Halogen.Query.HalogenM/index.js";
import * as Test_Component_Counter from "../Test.Component.Counter/index.js";
import * as Type_Proxy from "../Type.Proxy/index.js";
var eq1 = /* #__PURE__ */ Data_Eq.eq(Test_Component_Counter.eqOutput);
var prop = /* #__PURE__ */ Data_Lens_Record.prop({
    reflectSymbol: function () {
        return "total";
    }
})()();
var map = /* #__PURE__ */ Data_Functor.map(Data_Functor.functorArray);
var counterSlotIsSymbol = {
    reflectSymbol: function () {
        return "counterSlot";
    }
};
var slot = /* #__PURE__ */ Halogen_HTML.slot()(counterSlotIsSymbol)(Data_Ord.ordInt);
var pure = /* #__PURE__ */ Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM);
var bind = /* #__PURE__ */ Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM);
var requestAll = /* #__PURE__ */ Halogen_Query.requestAll()(counterSlotIsSymbol)(Data_Ord.ordInt);
var assign = /* #__PURE__ */ Data_Lens_Setter.assign(Halogen_Query_HalogenM.monadStateHalogenM);
var sum = /* #__PURE__ */ Data_Foldable.sum(Data_Map_Internal.foldableMap)(Data_Semiring.semiringInt);
var Total = /* #__PURE__ */ (function () {
    function Total(value0) {
        this.value0 = value0;
    };
    Total.create = function (value0) {
        return new Total(value0);
    };
    return Total;
})();
var CounterOutput = /* #__PURE__ */ (function () {
    function CounterOutput(value0) {
        this.value0 = value0;
    };
    CounterOutput.create = function (value0) {
        return new CounterOutput(value0);
    };
    return CounterOutput;
})();
var genericOutput_ = {
    to: function (x) {
        return new Total(x);
    },
    from: function (x) {
        return x.value0;
    }
};
var showOutput = {
    show: /* #__PURE__ */ Data_Show_Generic.genericShow(genericOutput_)(/* #__PURE__ */ Data_Show_Generic.genericShowConstructor(/* #__PURE__ */ Data_Show_Generic.genericShowArgsArgument(Data_Show.showInt))({
        reflectSymbol: function () {
            return "Total";
        }
    }))
};
var genericAction_ = {
    to: function (x) {
        return new CounterOutput(x);
    },
    from: function (x) {
        return x.value0;
    }
};
var showAction = {
    show: /* #__PURE__ */ Data_Show_Generic.genericShow(genericAction_)(/* #__PURE__ */ Data_Show_Generic.genericShowConstructor(/* #__PURE__ */ Data_Show_Generic.genericShowArgsArgument(Test_Component_Counter.showOutput))({
        reflectSymbol: function () {
            return "CounterOutput";
        }
    }))
};
var eqOutput = {
    eq: function (x) {
        return function (y) {
            return x.value0 === y.value0;
        };
    }
};
var eqAction = {
    eq: function (x) {
        return function (y) {
            return eq1(x.value0)(y.value0);
        };
    }
};
var _total = function (dictStrong) {
    return prop(Type_Proxy["Proxy"].value)(dictStrong);
};
var _total1 = /* #__PURE__ */ _total(Data_Profunctor_Strong.strongFn);
var componentSpec = function (dictMonadAff) {
    var component1 = Test_Component_Counter.component(dictMonadAff);
    var render = function (state) {
        return Halogen_HTML_Elements.div_(Data_Function.flip(map)(Data_Array.range(0)(state.numCounters))(function (i) {
            return slot(Type_Proxy["Proxy"].value)(i)(component1)(Data_Unit.unit)(CounterOutput.create);
        }));
    };
    var initialState = function (v) {
        return {
            numCounters: v.numCounters,
            total: 0
        };
    };
    var handleQuery = function (v) {
        return pure(Data_Maybe.Nothing.value);
    };
    var handleAction = function (v) {
        if (v.value0 instanceof Test_Component_Counter.Equals10) {
            return pure(Data_Unit.unit);
        };
        if (v.value0 instanceof Test_Component_Counter.FinishedPondering) {
            return pure(Data_Unit.unit);
        };
        if (v.value0 instanceof Test_Component_Counter.CountChanged) {
            return bind(requestAll(Type_Proxy["Proxy"].value)(Test_Component_Counter.GetCount.create))(function (counts) {
                return assign(_total1)(sum(counts));
            });
        };
        throw new Error("Failed pattern match at Test.Component.Summation (line 66, column 31 - line 71, column 31): " + [ v.value0.constructor.name ]);
    };
    return {
        initialState: initialState,
        render: render,
        "eval": Halogen_Component.mkEval({
            receive: Halogen_Component.defaultEval.receive,
            initialize: Halogen_Component.defaultEval.initialize,
            finalize: Halogen_Component.defaultEval.finalize,
            handleAction: function (action) {
                return handleAction(action);
            },
            handleQuery: function (query) {
                return handleQuery(query);
            }
        })
    };
};
var component = function (dictMonadAff) {
    return Halogen_Component.mkComponent(componentSpec(dictMonadAff));
};
export {
    CounterOutput,
    Total,
    _total,
    component,
    componentSpec,
    genericAction_,
    eqAction,
    showAction,
    genericOutput_,
    eqOutput,
    showOutput
};
//# sourceMappingURL=index.js.map
