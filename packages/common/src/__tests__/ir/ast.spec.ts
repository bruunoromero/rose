import "jest";

import {
  makeDummyIrList,
  makeDummyIrNumber,
  makeDummyIrSymbol,
  makeDummyIrString,
} from "@rose/test-helper";
import { ParserType } from "../../ir/types";

describe("type", () => {
  it("should return the correct type for the correct node", () => {
    expect(makeDummyIrList([]).type).toEqual(ParserType.LIST);
    expect(makeDummyIrSymbol("").type).toEqual(ParserType.SYMBOL);
    expect(makeDummyIrString("").type).toEqual(ParserType.STRING);
    expect(makeDummyIrNumber(10).type).toEqual(ParserType.NUMBER);
  });
});
