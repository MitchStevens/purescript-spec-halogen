// Generated by purs version 0.15.15
import * as $foreign from "./foreign.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Foreign_Object from "../Foreign.Object/index.js";
var eq = /* #__PURE__ */ Data_Eq.eq(Data_Ordering.eqOrdering);
var verbJsonType = function (def) {
    return function (f) {
        return function (g) {
            return g(def)(f);
        };
    };
};
var toJsonType = /* #__PURE__ */ (function () {
    return verbJsonType(Data_Maybe.Nothing.value)(Data_Maybe.Just.create);
})();
var jsonZero = /* #__PURE__ */ $foreign.fromNumber(0.0);
var jsonTrue = /* #__PURE__ */ $foreign.fromBoolean(true);
var jsonSingletonObject = function (key) {
    return function (val) {
        return $foreign.fromObject(Foreign_Object.singleton(key)(val));
    };
};
var jsonSingletonArray = function (j) {
    return $foreign.fromArray([ j ]);
};
var jsonFalse = /* #__PURE__ */ $foreign.fromBoolean(false);
var jsonEmptyString = /* #__PURE__ */ $foreign.fromString("");
var jsonEmptyObject = /* #__PURE__ */ $foreign.fromObject(Foreign_Object.empty);
var jsonEmptyArray = /* #__PURE__ */ $foreign.fromArray([  ]);
var isJsonType = /* #__PURE__ */ verbJsonType(false)(/* #__PURE__ */ Data_Function["const"](true));
var ordJson = {
    compare: function (a) {
        return function (b) {
            return $foreign["_compare"](Data_Ordering.EQ.value, Data_Ordering.GT.value, Data_Ordering.LT.value, a, b);
        };
    },
    Eq0: function () {
        return eqJson;
    }
};
var eqJson = {
    eq: function (j1) {
        return function (j2) {
            return eq(Data_Ord.compare(ordJson)(j1)(j2))(Data_Ordering.EQ.value);
        };
    }
};
var eqJNull = {
    eq: function (v) {
        return function (v1) {
            return true;
        };
    }
};
var ordJNull = {
    compare: function (v) {
        return function (v1) {
            return Data_Ordering.EQ.value;
        };
    },
    Eq0: function () {
        return eqJNull;
    }
};
var caseJsonString = function (d) {
    return function (f) {
        return function (j) {
            return $foreign["_caseJson"](Data_Function["const"](d), Data_Function["const"](d), Data_Function["const"](d), f, Data_Function["const"](d), Data_Function["const"](d), j);
        };
    };
};
var isString = /* #__PURE__ */ isJsonType(caseJsonString);
var toString = /* #__PURE__ */ toJsonType(caseJsonString);
var caseJsonObject = function (d) {
    return function (f) {
        return function (j) {
            return $foreign["_caseJson"](Data_Function["const"](d), Data_Function["const"](d), Data_Function["const"](d), Data_Function["const"](d), Data_Function["const"](d), f, j);
        };
    };
};
var isObject = /* #__PURE__ */ isJsonType(caseJsonObject);
var toObject = /* #__PURE__ */ toJsonType(caseJsonObject);
var caseJsonNumber = function (d) {
    return function (f) {
        return function (j) {
            return $foreign["_caseJson"](Data_Function["const"](d), Data_Function["const"](d), f, Data_Function["const"](d), Data_Function["const"](d), Data_Function["const"](d), j);
        };
    };
};
var isNumber = /* #__PURE__ */ isJsonType(caseJsonNumber);
var toNumber = /* #__PURE__ */ toJsonType(caseJsonNumber);
var caseJsonNull = function (d) {
    return function (f) {
        return function (j) {
            return $foreign["_caseJson"](f, Data_Function["const"](d), Data_Function["const"](d), Data_Function["const"](d), Data_Function["const"](d), Data_Function["const"](d), j);
        };
    };
};
var isNull = /* #__PURE__ */ isJsonType(caseJsonNull);
var toNull = /* #__PURE__ */ toJsonType(caseJsonNull);
var caseJsonBoolean = function (d) {
    return function (f) {
        return function (j) {
            return $foreign["_caseJson"](Data_Function["const"](d), f, Data_Function["const"](d), Data_Function["const"](d), Data_Function["const"](d), Data_Function["const"](d), j);
        };
    };
};
var isBoolean = /* #__PURE__ */ isJsonType(caseJsonBoolean);
var toBoolean = /* #__PURE__ */ toJsonType(caseJsonBoolean);
var caseJsonArray = function (d) {
    return function (f) {
        return function (j) {
            return $foreign["_caseJson"](Data_Function["const"](d), Data_Function["const"](d), Data_Function["const"](d), Data_Function["const"](d), f, Data_Function["const"](d), j);
        };
    };
};
var isArray = /* #__PURE__ */ isJsonType(caseJsonArray);
var toArray = /* #__PURE__ */ toJsonType(caseJsonArray);
var caseJson = function (a) {
    return function (b) {
        return function (c) {
            return function (d) {
                return function (e) {
                    return function (f) {
                        return function (json) {
                            return $foreign["_caseJson"](a, b, c, d, e, f, json);
                        };
                    };
                };
            };
        };
    };
};
export {
    fromBoolean,
    fromNumber,
    fromString,
    fromArray,
    fromObject,
    jsonNull,
    stringify,
    stringifyWithIndent
} from "./foreign.js";
export {
    caseJson,
    caseJsonNull,
    caseJsonBoolean,
    caseJsonNumber,
    caseJsonString,
    caseJsonArray,
    caseJsonObject,
    isNull,
    isBoolean,
    isNumber,
    isString,
    isArray,
    isObject,
    toNull,
    toBoolean,
    toNumber,
    toString,
    toArray,
    toObject,
    jsonTrue,
    jsonFalse,
    jsonZero,
    jsonEmptyString,
    jsonEmptyArray,
    jsonSingletonArray,
    jsonEmptyObject,
    jsonSingletonObject,
    eqJson,
    ordJson
};
//# sourceMappingURL=index.js.map
