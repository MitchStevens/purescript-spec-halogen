// Generated by purs version 0.15.15
import * as Control_Alternative from "../Control.Alternative/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Array_NonEmpty from "../Data.Array.NonEmpty/index.js";
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Options_Applicative_Common from "../Options.Applicative.Common/index.js";
import * as Options_Applicative_Help_Chunk from "../Options.Applicative.Help.Chunk/index.js";
import * as Options_Applicative_Help_Types from "../Options.Applicative.Help.Types/index.js";
import * as Options_Applicative_Types from "../Options.Applicative.Types/index.js";
import * as Text_PrettyPrint_Leijen from "../Text.PrettyPrint.Leijen/index.js";
var over = /* #__PURE__ */ Data_Newtype.over()();
var mempty = /* #__PURE__ */ Data_Monoid.mempty(Options_Applicative_Help_Types.parserHelpMonoid);
var fold = /* #__PURE__ */ Data_Array.fold(Data_Monoid.monoidArray);
var un = /* #__PURE__ */ Data_Newtype.un();
var chunkMonoid = /* #__PURE__ */ Options_Applicative_Help_Chunk.chunkMonoid(Text_PrettyPrint_Leijen.docSemigroup);
var mempty1 = /* #__PURE__ */ Data_Monoid.mempty(chunkMonoid);
var eq1 = /* #__PURE__ */ Data_Eq.eq(Options_Applicative_Types.optVisibilityEq);
var map = /* #__PURE__ */ Data_Functor.map(Data_Functor.functorArray);
var sort = /* #__PURE__ */ Data_Array.sort(Options_Applicative_Types.optNameOrd);
var append = /* #__PURE__ */ Data_Semigroup.append(/* #__PURE__ */ Options_Applicative_Help_Chunk.chunkSemigroup(Text_PrettyPrint_Leijen.docSemigroup));
var map1 = /* #__PURE__ */ Data_Functor.map(Options_Applicative_Help_Chunk.chunkFunctor);
var listToChunk = /* #__PURE__ */ Options_Applicative_Help_Chunk.listToChunk(Text_PrettyPrint_Leijen.docMonoid);
var identity = /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn);
var map2 = /* #__PURE__ */ Data_Functor.map(Data_Maybe.functorMaybe);
var discard = /* #__PURE__ */ Control_Bind.discard(Control_Bind.discardUnit)(Data_Maybe.bindMaybe);
var guard = /* #__PURE__ */ Control_Alternative.guard(Data_Maybe.alternativeMaybe);
var pure = /* #__PURE__ */ Control_Applicative.pure(Data_Maybe.applicativeMaybe);
var extractChunk = /* #__PURE__ */ Options_Applicative_Help_Chunk.extractChunk(Text_PrettyPrint_Leijen.docMonoid);
var bind = /* #__PURE__ */ Control_Bind.bind(Control_Bind.bindArray);
var pure1 = /* #__PURE__ */ Control_Applicative.pure(Control_Applicative.applicativeArray);
var mempty2 = /* #__PURE__ */ Data_Monoid.mempty(/* #__PURE__ */ Data_Tuple.monoidTuple(/* #__PURE__ */ Data_Maybe.monoidMaybe(Data_Semigroup.semigroupString))(chunkMonoid));
var append1 = /* #__PURE__ */ Data_Semigroup.append(Data_Semigroup.semigroupArray);
var eq2 = /* #__PURE__ */ Data_Eq.eq(/* #__PURE__ */ Data_Maybe.eqMaybe(Data_Eq.eqString));
var OptDescStyle = function (x) {
    return x;
};
var usageHelp = function (chunk) {
    return over(Options_Applicative_Help_Types.ParserHelp)(function (v) {
        return {
            helpBody: v.helpBody,
            helpError: v.helpError,
            helpFooter: v.helpFooter,
            helpHeader: v.helpHeader,
            helpSuggestions: v.helpSuggestions,
            helpUsage: chunk
        };
    })(mempty);
};
var suggestionsHelp = function (chunk) {
    return over(Options_Applicative_Help_Types.ParserHelp)(function (v) {
        return {
            helpBody: v.helpBody,
            helpError: v.helpError,
            helpFooter: v.helpFooter,
            helpHeader: v.helpHeader,
            helpUsage: v.helpUsage,
            helpSuggestions: chunk
        };
    })(mempty);
};
var newtypeOptDescStyle = {
    Coercible0: function () {
        return undefined;
    }
};
var intersperse = function (sep) {
    var $64 = Data_Array.mapWithIndex(function (idx) {
        return function (e) {
            var $49 = idx === 0;
            if ($49) {
                return [ e ];
            };
            return [ sep, e ];
        };
    });
    return function ($65) {
        return fold($64($65));
    };
};
var optDesc = function (pprefs) {
    return function (style) {
        return function (info) {
            return function (opt) {
                var suffix = (function () {
                    if ((un(Options_Applicative_Types.OptHelpInfo)(info)).hinfoMulti) {
                        return Options_Applicative_Help_Chunk.stringChunk((un(Options_Applicative_Types.ParserPrefs)(pprefs)).prefMultiSuffix);
                    };
                    if (Data_Boolean.otherwise) {
                        return mempty1;
                    };
                    throw new Error("Failed pattern match at Options.Applicative.Help.Core (line 58, column 7 - line 62, column 17): " + [  ]);
                })();
                var show_opt = (function () {
                    if ((un(Options_Applicative_Types.OptHelpInfo)(info)).hinfoDefault && !(un(OptDescStyle)(style)).descOptional) {
                        return false;
                    };
                    if (eq1(Options_Applicative_Types.optVisibility(opt))(Options_Applicative_Types.Hidden.value)) {
                        return (un(OptDescStyle)(style)).descHidden;
                    };
                    if (Data_Boolean.otherwise) {
                        return eq1(Options_Applicative_Types.optVisibility(opt))(Options_Applicative_Types.Visible.value);
                    };
                    throw new Error("Failed pattern match at Options.Applicative.Help.Core (line 51, column 7 - line 57, column 39): " + [  ]);
                })();
                var ns = Options_Applicative_Common.optionNames((un(Options_Applicative_Types.Option)(opt)).optMain);
                var mv = Options_Applicative_Help_Chunk.stringChunk(Options_Applicative_Types.optMetaVar(opt));
                var descs = map(function ($66) {
                    return Text_PrettyPrint_Leijen.string(Options_Applicative_Common.showOption($66));
                })(sort(ns));
                var render = function (chunk) {
                    if (!show_opt) {
                        return mempty1;
                    };
                    if (Options_Applicative_Help_Chunk.isEmpty(chunk) || !(un(OptDescStyle)(style)).descSurround) {
                        return append(chunk)(suffix);
                    };
                    if ((un(Options_Applicative_Types.OptHelpInfo)(info)).hinfoDefault) {
                        return append(map1(Text_PrettyPrint_Leijen.brackets)(chunk))(suffix);
                    };
                    if (Data_Array["null"](Data_Array.drop(1)(descs))) {
                        return append(chunk)(suffix);
                    };
                    if (Data_Boolean.otherwise) {
                        return append(map1(Text_PrettyPrint_Leijen.parens)(chunk))(suffix);
                    };
                    throw new Error("Failed pattern match at Options.Applicative.Help.Core (line 63, column 7 - line 73, column 43): " + [ chunk.constructor.name ]);
                };
                var desc$prime = Options_Applicative_Help_Chunk.chunkBeside(listToChunk(intersperse((un(OptDescStyle)(style)).descSep)(descs)))(mv);
                return Data_Maybe.maybe(identity)(map1)(Options_Applicative_Types.optDescMod(opt))(render(desc$prime));
            };
        };
    };
};
var headerHelp = function (chunk) {
    return over(Options_Applicative_Help_Types.ParserHelp)(function (v) {
        return {
            helpBody: v.helpBody,
            helpError: v.helpError,
            helpFooter: v.helpFooter,
            helpSuggestions: v.helpSuggestions,
            helpUsage: v.helpUsage,
            helpHeader: chunk
        };
    })(mempty);
};
var fullDesc = function (pprefs) {
    var style = {
        descSep: Text_PrettyPrint_Leijen.string(","),
        descHidden: true,
        descOptional: true,
        descSurround: false
    };
    var doc = function (info) {
        return function (opt) {
            var show_def = function (s) {
                return Text_PrettyPrint_Leijen.parens(Text_PrettyPrint_Leijen.appendWithSpace(Text_PrettyPrint_Leijen.string("default:"))(Text_PrettyPrint_Leijen.string(s)));
            };
            var n = optDesc(pprefs)(style)(info)(opt);
            var hdef = map2(show_def)(Options_Applicative_Types.optShowDefault(opt));
            var h = Options_Applicative_Types.optHelp(opt);
            return discard(guard(!Options_Applicative_Help_Chunk.isEmpty(n)))(function () {
                return discard(guard(!Options_Applicative_Help_Chunk.isEmpty(h)))(function () {
                    return pure(new Data_Tuple.Tuple(extractChunk(n), Text_PrettyPrint_Leijen.align(extractChunk(Options_Applicative_Help_Chunk.chunkBeside(h)(hdef)))));
                });
            });
        };
    };
    var $67 = Options_Applicative_Common.mapParser(doc);
    return function ($68) {
        return Options_Applicative_Help_Chunk.tabulate(Data_Array.catMaybes($67($68)));
    };
};
var footerHelp = function (chunk) {
    return over(Options_Applicative_Help_Types.ParserHelp)(function (v) {
        return {
            helpBody: v.helpBody,
            helpError: v.helpError,
            helpHeader: v.helpHeader,
            helpSuggestions: v.helpSuggestions,
            helpUsage: v.helpUsage,
            helpFooter: chunk
        };
    })(mempty);
};
var fold_tree = function (v) {
    if (v instanceof Options_Applicative_Types.Leaf) {
        return v.value0;
    };
    if (v instanceof Options_Applicative_Types.MultNode) {
        return Data_Array.foldr(function ($69) {
            return Options_Applicative_Help_Chunk.chunkBesideOrBelow(fold_tree($69));
        })(mempty1)(v.value0);
    };
    if (v instanceof Options_Applicative_Types.AltNode) {
        var alt_node = function (v1) {
            if (v1.length === 1) {
                return v1[0];
            };
            return map1(Text_PrettyPrint_Leijen.parens)(Data_Array.foldr(Options_Applicative_Help_Chunk.chunked(function (x) {
                return function (y) {
                    return Text_PrettyPrint_Leijen.appendWithSoftline(x)(Text_PrettyPrint_Leijen.appendWithSoftline(Text_PrettyPrint_Leijen["char"]("|"))(y));
                };
            }))(mempty1)(v1));
        };
        return alt_node(Data_Array.filter(function ($70) {
            return !Options_Applicative_Help_Chunk.isEmpty($70);
        })(map(fold_tree)(v.value0)));
    };
    throw new Error("Failed pattern match at Options.Applicative.Help.Core (line 116, column 1 - line 116, column 46): " + [ v.constructor.name ]);
};
var errorHelp = function (chunk) {
    return over(Options_Applicative_Help_Types.ParserHelp)(function (v) {
        return {
            helpBody: v.helpBody,
            helpFooter: v.helpFooter,
            helpHeader: v.helpHeader,
            helpSuggestions: v.helpSuggestions,
            helpUsage: v.helpUsage,
            helpError: chunk
        };
    })(mempty);
};
var cmdDesc = /* #__PURE__ */ (function () {
    var desc = function (v) {
        return function (opt) {
            var v1 = (un(Options_Applicative_Types.Option)(opt)).optMain;
            if (v1 instanceof Options_Applicative_Types.CmdReader) {
                return new Data_Tuple.Tuple(v1.value0, Options_Applicative_Help_Chunk.tabulate(bind(Data_Array.reverse(v1.value1))(function (cmd) {
                    return bind(Data_Maybe.maybe([  ])(pure1)(map2((function () {
                        var $71 = un(Options_Applicative_Types.ParserInfo);
                        return function ($72) {
                            return (function (v2) {
                                return v2.infoProgDesc;
                            })($71($72));
                        };
                    })())(v1.value2(cmd))))(function (d) {
                        return pure1(new Data_Tuple.Tuple(Text_PrettyPrint_Leijen.string(cmd), Text_PrettyPrint_Leijen.align(extractChunk(d))));
                    });
                })));
            };
            return mempty2;
        };
    };
    return Options_Applicative_Common.mapParser(desc);
})();
var briefDesc$prime = function (showOptional) {
    return function (pprefs) {
        var style = {
            descSep: Text_PrettyPrint_Leijen.string("|"),
            descHidden: false,
            descOptional: showOptional,
            descSurround: true
        };
        var $73 = Options_Applicative_Common.treeMapParser(optDesc(pprefs)(style));
        return function ($74) {
            return fold_tree($73($74));
        };
    };
};
var missingDesc = /* #__PURE__ */ briefDesc$prime(false);
var briefDesc = /* #__PURE__ */ briefDesc$prime(true);
var parserUsage = function (pprefs) {
    return function (p) {
        return function (progn) {
            return Text_PrettyPrint_Leijen.hsep([ Text_PrettyPrint_Leijen.string("Usage:"), Text_PrettyPrint_Leijen.string(progn), Text_PrettyPrint_Leijen.align(extractChunk(briefDesc(pprefs)(p))) ]);
        };
    };
};
var bodyHelp = function (chunk) {
    return over(Options_Applicative_Help_Types.ParserHelp)(function (v) {
        return {
            helpError: v.helpError,
            helpFooter: v.helpFooter,
            helpHeader: v.helpHeader,
            helpSuggestions: v.helpSuggestions,
            helpUsage: v.helpUsage,
            helpBody: chunk
        };
    })(mempty);
};
var parserHelp = function (pprefs) {
    return function (p) {
        var with_title = function (title) {
            return map1(function (v) {
                return Text_PrettyPrint_Leijen.appendWithLine(Text_PrettyPrint_Leijen.string(title))(v);
            });
        };
        var group_title = function (arr) {
            var v = Data_Array_NonEmpty.uncons(arr);
            return with_title(Data_Maybe.fromMaybe("Available commands:")(Data_Tuple.fst(v.head)))(Options_Applicative_Help_Chunk.vcatChunks(append1([ Data_Tuple.snd(v.head) ])(map(Data_Tuple.snd)(v.tail))));
        };
        var cs = Data_Array.groupBy(Data_Function.on(eq2)(Data_Tuple.fst))(cmdDesc(p));
        return bodyHelp(Options_Applicative_Help_Chunk.vsepChunks(append1([ with_title("Available options:")(fullDesc(pprefs)(p)) ])(map(group_title)(cs))));
    };
};
export {
    cmdDesc,
    briefDesc,
    missingDesc,
    fold_tree,
    fullDesc,
    errorHelp,
    headerHelp,
    suggestionsHelp,
    usageHelp,
    bodyHelp,
    footerHelp,
    parserHelp,
    parserUsage
};
export {
    ParserHelp
} from "../Options.Applicative.Types/index.js";
//# sourceMappingURL=index.js.map
