// Generated by purs version 0.15.15
import * as Data_Functor from "../Data.Functor/index.js";
import * as Matryoshka_Class_Corecursive from "../Matryoshka.Class.Corecursive/index.js";
import * as Matryoshka_Class_Recursive from "../Matryoshka.Class.Recursive/index.js";
var traverseR = function (dictRecursive) {
    var project = Matryoshka_Class_Recursive.project(dictRecursive);
    return function (dictCorecursive) {
        var embed = Matryoshka_Class_Corecursive.embed(dictCorecursive);
        return function (dictFunctor) {
            var map = Data_Functor.map(dictFunctor);
            return function (f) {
                var $11 = map(embed);
                return function ($12) {
                    return $11(f(project($12)));
                };
            };
        };
    };
};
var mapR = function (dictRecursive) {
    var project = Matryoshka_Class_Recursive.project(dictRecursive);
    return function (dictCorecursive) {
        var embed = Matryoshka_Class_Corecursive.embed(dictCorecursive);
        return function (f) {
            return function ($13) {
                return embed(f(project($13)));
            };
        };
    };
};
export {
    mapR,
    traverseR
};
//# sourceMappingURL=index.js.map