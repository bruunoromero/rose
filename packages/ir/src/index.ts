import { CompilerProgram } from "@rose/common";
import {
  makeDummyCompilerList,
  makeDummyCompilerSymbol,
} from "@rose/test-helper";
import { internalize } from "./internalizer";
import { squash } from "./squasher";

export * from "./internalizer";

const program = new CompilerProgram("", [
  makeDummyCompilerList([
    makeDummyCompilerSymbol("require"),
    makeDummyCompilerSymbol("rose.value"),
  ]),
  makeDummyCompilerList([
    makeDummyCompilerSymbol("def"),
    makeDummyCompilerSymbol("rose.core/test"),
    makeDummyCompilerList([
      makeDummyCompilerSymbol("fn"),
      makeDummyCompilerList([
        makeDummyCompilerSymbol("a"),
        makeDummyCompilerSymbol("b"),
      ]),
      makeDummyCompilerList([
        makeDummyCompilerSymbol("rose.value/+"),
        makeDummyCompilerSymbol("a"),
        makeDummyCompilerSymbol("b"),
      ]),
    ]),
  ]),
]);

const ir = internalize(program);

console.log(squash(ir).nodes[1]);
