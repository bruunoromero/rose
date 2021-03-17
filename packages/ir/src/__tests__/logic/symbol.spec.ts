import "jest";

import { makeDummyCompilerSymbol } from "@rose/test-helper";
import { isInternalSymbol, isDeclaredSymbol } from "../../logic/symbol";

describe("isDeclaredSymbol", () => {
  it("should return true if the symbol it not one of CompilerNodeType", () => {
    expect(isDeclaredSymbol(makeDummyCompilerSymbol("a/b"))).toEqual(true);
    expect(isDeclaredSymbol(makeDummyCompilerSymbol("abc/def"))).toEqual(true);
    expect(isDeclaredSymbol(makeDummyCompilerSymbol("cba/fed"))).toEqual(true);
  });

  it("should return false if the symbol is one of CompilerNodeType", () => {
    expect(isDeclaredSymbol(makeDummyCompilerSymbol("fn"))).toEqual(false);
    expect(isDeclaredSymbol(makeDummyCompilerSymbol("def"))).toEqual(false);
    expect(isDeclaredSymbol(makeDummyCompilerSymbol("require"))).toEqual(false);
  });
});

describe("isInternalSymbol", () => {
  it("should return true if the symbol is one of CompilerNodeType", () => {
    expect(isInternalSymbol(makeDummyCompilerSymbol("fn"))).toEqual(true);
    expect(isInternalSymbol(makeDummyCompilerSymbol("def"))).toEqual(true);
    expect(isInternalSymbol(makeDummyCompilerSymbol("require"))).toEqual(true);
  });

  it("should return false if the symbol is namespaced", () => {
    expect(isInternalSymbol(makeDummyCompilerSymbol("a/b"))).toEqual(false);
    expect(isInternalSymbol(makeDummyCompilerSymbol("abc/def"))).toEqual(false);
    expect(isInternalSymbol(makeDummyCompilerSymbol("cba/fed"))).toEqual(false);
  });
});
