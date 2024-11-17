// Generated by purs version 0.15.15
import * as Data_Generic_Rep from "../Data.Generic.Rep/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
var genericSemigroupNoConstructors = {
    "genericAppend'": function (a) {
        return function (v) {
            return a;
        };
    }
};
var genericSemigroupNoArguments = {
    "genericAppend'": function (a) {
        return function (v) {
            return a;
        };
    }
};
var genericSemigroupArgument = function (dictSemigroup) {
    var append = Data_Semigroup.append(dictSemigroup);
    return {
        "genericAppend'": function (v) {
            return function (v1) {
                return append(v)(v1);
            };
        }
    };
};
var genericAppend$prime = function (dict) {
    return dict["genericAppend'"];
};
var genericSemigroupConstructor = function (dictGenericSemigroup) {
    var genericAppend$prime1 = genericAppend$prime(dictGenericSemigroup);
    return {
        "genericAppend'": function (v) {
            return function (v1) {
                return genericAppend$prime1(v)(v1);
            };
        }
    };
};
var genericSemigroupProduct = function (dictGenericSemigroup) {
    var genericAppend$prime1 = genericAppend$prime(dictGenericSemigroup);
    return function (dictGenericSemigroup1) {
        var genericAppend$prime2 = genericAppend$prime(dictGenericSemigroup1);
        return {
            "genericAppend'": function (v) {
                return function (v1) {
                    return new Data_Generic_Rep.Product(genericAppend$prime1(v.value0)(v1.value0), genericAppend$prime2(v.value1)(v1.value1));
                };
            }
        };
    };
};
var genericAppend = function (dictGeneric) {
    var to = Data_Generic_Rep.to(dictGeneric);
    var from = Data_Generic_Rep.from(dictGeneric);
    return function (dictGenericSemigroup) {
        var genericAppend$prime1 = genericAppend$prime(dictGenericSemigroup);
        return function (x) {
            return function (y) {
                return to(genericAppend$prime1(from(x))(from(y)));
            };
        };
    };
};
export {
    genericAppend$prime,
    genericAppend,
    genericSemigroupNoConstructors,
    genericSemigroupNoArguments,
    genericSemigroupProduct,
    genericSemigroupConstructor,
    genericSemigroupArgument
};
//# sourceMappingURL=index.js.map
