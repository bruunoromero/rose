import "jest";

import {
  makeDummyCompilerList,
  makeDummyCompilerNumber,
  makeDummyCompilerSymbol,
  makeDummyCompilerString,
} from "@rose/test-helper";
import { ParserType } from "../../compiler/types";

describe("type", () => {
  it("should return the correct type for the correct node", () => {
    expect(makeDummyCompilerList([]).type).toEqual(ParserType.LIST);
    expect(makeDummyCompilerSymbol("").type).toEqual(ParserType.SYMBOL);
    expect(makeDummyCompilerString("").type).toEqual(ParserType.STRING);
    expect(makeDummyCompilerNumber(10).type).toEqual(ParserType.NUMBER);
  });
});
