import "jest";

import { makeDummyIrSymbol } from "@rose/test-helper";
import { isInternalSymbol, isDeclaredSymbol } from "../../logic/symbol";

describe("isDeclaredSymbol", () => {
  it("should return true if the symbol is namespaced", () => {
    expect(isDeclaredSymbol(makeDummyIrSymbol("a/b"))).toEqual(true);
    expect(isDeclaredSymbol(makeDummyIrSymbol("abc/def"))).toEqual(true);
    expect(isDeclaredSymbol(makeDummyIrSymbol("cba/fed"))).toEqual(true);
  });

  it("should return false if the symbol is not namespaced", () => {
    expect(isDeclaredSymbol(makeDummyIrSymbol("a"))).toEqual(false);
    expect(isDeclaredSymbol(makeDummyIrSymbol("abc"))).toEqual(false);
    expect(isDeclaredSymbol(makeDummyIrSymbol("cba"))).toEqual(false);
  });
});

describe("isInternalSymbol", () => {
  it("should return true if the symbol is not namespaced", () => {
    expect(isInternalSymbol(makeDummyIrSymbol("a"))).toEqual(true);
    expect(isInternalSymbol(makeDummyIrSymbol("abc"))).toEqual(true);
    expect(isInternalSymbol(makeDummyIrSymbol("cba"))).toEqual(true);
  });

  it("should return false if the symbol is namespaced", () => {
    expect(isInternalSymbol(makeDummyIrSymbol("a/b"))).toEqual(false);
    expect(isInternalSymbol(makeDummyIrSymbol("abc/def"))).toEqual(false);
    expect(isInternalSymbol(makeDummyIrSymbol("cba/fed"))).toEqual(false);
  });
});
