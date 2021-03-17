import { CompilerNodeType, CompilerSymbol, Constants } from "@rose/common";

export const isDeclaredSymbol = (symbol: CompilerSymbol) =>
  !isInternalSymbol(symbol);

export const isInternalSymbol = (symbol: CompilerSymbol) =>
  [
    CompilerNodeType.FN as string,
    CompilerNodeType.DEFINE,
    CompilerNodeType.REQUIRE,
  ].includes(symbol.value);
