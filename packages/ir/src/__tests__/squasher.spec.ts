import { makeDummyCompilerList } from "@rose/test-helper";
import "jest";
import {
  squashNumber,
  squashRequire,
  squashString,
  squashSymbol,
} from "../squasher";
import {
  SquashNumber,
  SquashRequire,
  SquashString,
  SquashSymbol,
} from "../squasher/ast";
import {
  makeDummyInternalizerList,
  makeDummyInternalizerNumber,
  makeDummyInternalizerRequire,
  makeDummyInternalizerString,
  makeDummyInternalizerSymbol,
} from "../__test_helpers__/internalizer";

describe("squashSymbol", () => {
  it("should take an IrSymbol and returns a SquashSymbol", () => {
    expect(squashSymbol(makeDummyInternalizerSymbol("a"))).toBeInstanceOf(
      SquashSymbol
    );
    expect(squashSymbol(makeDummyInternalizerSymbol("a")).value).toEqual("a");
    expect(squashSymbol(makeDummyInternalizerSymbol("abc"))).toBeInstanceOf(
      SquashSymbol
    );
    expect(
      squashSymbol(makeDummyInternalizerSymbol("rose.core/value")).value
    ).toEqual("rose.core/value");
    expect(
      squashSymbol(makeDummyInternalizerSymbol("rose.core/value"))
    ).toBeInstanceOf(SquashSymbol);
    expect(squashSymbol(makeDummyInternalizerSymbol("123"))).toBeInstanceOf(
      SquashSymbol
    );
    expect(squashSymbol(makeDummyInternalizerSymbol("123")).value).toEqual(
      "123"
    );
  });
});

describe("squashString", () => {
  it("should take an IrString and returns an SquashString", () => {
    expect(squashString(makeDummyInternalizerString(""))).toBeInstanceOf(
      SquashString
    );
    expect(squashString(makeDummyInternalizerString("")).value).toEqual("");
    expect(squashString(makeDummyInternalizerString("a"))).toBeInstanceOf(
      SquashString
    );
    expect(squashString(makeDummyInternalizerString("a")).value).toEqual("a");
    expect(squashString(makeDummyInternalizerString("abc"))).toBeInstanceOf(
      SquashString
    );
    expect(
      squashString(makeDummyInternalizerString("rose.core/value"))
    ).toBeInstanceOf(SquashString);
    expect(
      squashString(makeDummyInternalizerString("rose.core/value")).value
    ).toEqual("rose.core/value");
    expect(squashString(makeDummyInternalizerString("123"))).toBeInstanceOf(
      SquashString
    );
    expect(squashString(makeDummyInternalizerString("123")).value).toEqual(
      "123"
    );
  });
});

describe("squashNumber", () => {
  it("should take an IrNumber and returns an SquashNumber", () => {
    expect(squashNumber(makeDummyInternalizerNumber(20))).toBeInstanceOf(
      SquashNumber
    );
    expect(squashNumber(makeDummyInternalizerNumber(20)).value).toEqual(20);
    expect(squashNumber(makeDummyInternalizerNumber(0))).toBeInstanceOf(
      SquashNumber
    );
    expect(squashNumber(makeDummyInternalizerNumber(0)).value).toEqual(0);
    expect(squashNumber(makeDummyInternalizerNumber(0))).toBeInstanceOf(
      SquashNumber
    );
    expect(squashNumber(makeDummyInternalizerNumber(-7))).toBeInstanceOf(
      SquashNumber
    );
    expect(squashNumber(makeDummyInternalizerNumber(-7)).value).toEqual(-7);
    expect(squashNumber(makeDummyInternalizerNumber(0.3))).toBeInstanceOf(
      SquashNumber
    );
    expect(squashNumber(makeDummyInternalizerNumber(0.3)).value).toEqual(0.3);
  });
});

describe("squashRequire", () => {
  it("should take an IrList with IrRequire as first node and returns an SquashRequire", () => {
    const requireNode1 = squashRequire(
      makeDummyInternalizerList([
        makeDummyInternalizerRequire(),
        makeDummyInternalizerSymbol("symbol"),
      ])
    );

    const requireNode2 = squashRequire(
      makeDummyInternalizerList([
        makeDummyInternalizerRequire(),
        makeDummyInternalizerSymbol("rose.core"),
      ])
    );

    expect(requireNode1).toBeInstanceOf(SquashRequire);
    expect(requireNode1.symbol.value).toEqual("symbol");
    expect(requireNode1.symbol).toBeInstanceOf(SquashSymbol);

    expect(requireNode2).toBeInstanceOf(SquashRequire);
    expect(requireNode2.symbol.value).toEqual("rose.core");
    expect(requireNode2.symbol).toBeInstanceOf(SquashSymbol);
  });

  it("should throw an error when the first node is not an IrRequire", () => {
    const requireNode1 = () =>
      squashRequire(
        makeDummyInternalizerList([
          makeDummyInternalizerSymbol("require"),
          makeDummyInternalizerSymbol("symbol"),
        ])
      );

    const requireNode2 = () =>
      squashRequire(
        makeDummyInternalizerList([
          makeDummyInternalizerNumber(10),
          makeDummyInternalizerSymbol("rose.core"),
        ])
      );

    expect(requireNode1).toThrow();
    expect(requireNode2).toThrow();
  });

  it("should throw an error when the second node is not an IrSymbol", () => {
    const requireNode1 = () =>
      squashRequire(
        makeDummyInternalizerList([
          makeDummyInternalizerRequire(),
          makeDummyInternalizerString("symbol"),
        ])
      );

    const requireNode2 = () =>
      squashRequire(
        makeDummyInternalizerList([
          makeDummyInternalizerRequire(),
          makeDummyInternalizerList([]),
        ])
      );

    const requireNode3 = () =>
      squashRequire(
        makeDummyInternalizerList([
          makeDummyInternalizerRequire(),
          makeDummyInternalizerNumber(10),
        ])
      );

    const requireNode4 = () =>
      squashRequire(
        makeDummyInternalizerList([makeDummyInternalizerRequire()])
      );

    expect(requireNode1).toThrow();
    expect(requireNode2).toThrow();
    expect(requireNode3).toThrow();
    expect(requireNode4).toThrow();
  });
});
