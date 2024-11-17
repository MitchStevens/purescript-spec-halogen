// Generated by purs version 0.15.15
import * as Data_Function from "../Data.Function/index.js";
import * as Data_List_Lazy from "../Data.List.Lazy/index.js";
import * as Data_List_Lazy_Types from "../Data.List.Lazy.Types/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Partial_Unsafe from "../Partial.Unsafe/index.js";
var append1 = /* #__PURE__ */ Data_Semigroup.append(Data_List_Lazy_Types.semigroupList);
var ZipList = function (x) {
    return x;
};
var traversableZipList = Data_List_Lazy_Types.traversableList;
var showZipList = function (dictShow) {
    var show = Data_Show.show(Data_List_Lazy_Types.showList(dictShow));
    return {
        show: function (v) {
            return "(ZipList " + (show(v) + ")");
        }
    };
};
var semigroupZipList = Data_List_Lazy_Types.semigroupList;
var ordZipList = function (dictOrd) {
    return Data_List_Lazy_Types.ordList(dictOrd);
};
var newtypeZipList = {
    Coercible0: function () {
        return undefined;
    }
};
var monoidZipList = Data_List_Lazy_Types.monoidList;
var functorZipList = Data_List_Lazy_Types.functorList;
var foldableZipList = Data_List_Lazy_Types.foldableList;
var eqZipList = function (dictEq) {
    return Data_List_Lazy_Types.eqList(dictEq);
};
var applyZipList = {
    apply: function (v) {
        return function (v1) {
            return Data_List_Lazy.zipWith(Data_Function.apply)(v)(v1);
        };
    },
    Functor0: function () {
        return functorZipList;
    }
};
var zipListIsNotBind = function () {
    return {
        bind: Partial_Unsafe.unsafeCrashWith("bind: unreachable"),
        Apply0: function () {
            return applyZipList;
        }
    };
};
var applicativeZipList = {
    pure: function ($21) {
        return ZipList(Data_List_Lazy.repeat($21));
    },
    Apply0: function () {
        return applyZipList;
    }
};
var altZipList = {
    alt: function (v) {
        return function (v1) {
            return append1(v)(Data_List_Lazy.drop(Data_List_Lazy.length(v))(v1));
        };
    },
    Functor0: function () {
        return functorZipList;
    }
};
var plusZipList = {
    empty: /* #__PURE__ */ Data_Monoid.mempty(monoidZipList),
    Alt0: function () {
        return altZipList;
    }
};
var alternativeZipList = {
    Applicative0: function () {
        return applicativeZipList;
    },
    Plus1: function () {
        return plusZipList;
    }
};
export {
    ZipList,
    showZipList,
    newtypeZipList,
    eqZipList,
    ordZipList,
    semigroupZipList,
    monoidZipList,
    foldableZipList,
    traversableZipList,
    functorZipList,
    applyZipList,
    applicativeZipList,
    altZipList,
    plusZipList,
    alternativeZipList,
    zipListIsNotBind
};
//# sourceMappingURL=index.js.map
