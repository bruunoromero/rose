import * as _ from "lodash/fp";

import { ParserSymbol, Constants } from "@rose/common";

export const isInternalSymbol = (symbol: ParserSymbol) =>
  _.contains(symbol.value, Constants.NAMESPACE_SEPARATOR);

export const isDeclaredSymbol = (symbol: ParserSymbol) =>
  !isInternalSymbol(symbol);
