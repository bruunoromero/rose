import { ParserSymbol, Constants } from "@rose/common";

export const isDeclaredSymbol = (symbol: ParserSymbol) =>
  symbol.value.includes(Constants.NAMESPACE_SEPARATOR);

export const isInternalSymbol = (symbol: ParserSymbol) =>
  !isDeclaredSymbol(symbol);
