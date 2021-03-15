import { ParserSymbol } from "@rose/common";
import { isInternalSymbol } from "./logic/symbol";

export * from "./internalizer";

const emptyPosition = {
  line: 0,
  column: 0,
};

const makeSymbol = (value: string) =>
  new ParserSymbol({ start: emptyPosition, end: emptyPosition }, value);

console.log(isInternalSymbol(makeSymbol("a")));
