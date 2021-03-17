import "jest";
import {
  makeDummyCompilerList,
  makeDummyCompilerNumber,
  makeDummyCompilerString,
  makeDummyCompilerSymbol,
} from "@rose/test-helper";

import {
  IrDefine,
  IrList,
  IrNumber,
  IrString,
  IrSymbol,
} from "../internalizer/ast";
import * as internalizer from "../internalizer";

const internalizeInternalSymbolSpy = jest.spyOn(
  internalizer,
  "internalizeInternalSymbol"
);

const internalizeDeclaredSymbolSpy = jest.spyOn(
  internalizer,
  "internalizeDeclaredSymbol"
);

const internalizeNodeSpy = jest.spyOn(internalizer, "internalizeNode");
const internalizeListSpy = jest.spyOn(internalizer, "internalizeList");
const internalizeSymbolSpy = jest.spyOn(internalizer, "internalizeSymbol");
const internalizeStringSpy = jest.spyOn(internalizer, "internalizeString");
const internalizeNumberSpy = jest.spyOn(internalizer, "internalizeNumber");

const {
  internalizeNode,
  internalizeList,
  internalizeNumber,
  internalizeString,
  internalizeDeclaredSymbol,
  internalizeInternalSymbol,
} = internalizer;

beforeEach(() => {
  internalizeListSpy.mockClear();
  internalizeNodeSpy.mockClear();
  internalizeStringSpy.mockClear();
  internalizeNumberSpy.mockClear();
  internalizeSymbolSpy.mockClear();
  internalizeInternalSymbolSpy.mockClear();
  internalizeDeclaredSymbolSpy.mockClear();
});

describe("internalizeDeclaredSymbol", () => {
  it("should take a symbol and returns an IrSymbol", () => {
    expect(
      internalizeDeclaredSymbol(makeDummyCompilerSymbol("rose.core/value"))
    ).toBeInstanceOf(IrSymbol);
    expect(
      internalizeDeclaredSymbol(makeDummyCompilerSymbol("rose.core/value"))
        .value
    ).toEqual("rose.core/value");

    expect(
      internalizeDeclaredSymbol(makeDummyCompilerSymbol("the/thing"))
    ).toBeInstanceOf(IrSymbol);
    expect(
      internalizeDeclaredSymbol(makeDummyCompilerSymbol("the/thing")).value
    ).toEqual("the/thing");

    expect(
      internalizeDeclaredSymbol(makeDummyCompilerSymbol("rose.core/*"))
    ).toBeInstanceOf(IrSymbol);
    expect(
      internalizeDeclaredSymbol(makeDummyCompilerSymbol("rose.core/*")).value
    ).toEqual("rose.core/*");
  });
});

describe("internalizeInternalSymbol", () => {
  it("should take a `def` string and returns a define node", () => {
    expect(
      internalizeInternalSymbol(makeDummyCompilerSymbol("def"))
    ).toBeInstanceOf(IrDefine);
  });

  it("should take a random string and throws if not a internal symbol", () => {
    expect(() =>
      internalizeInternalSymbol(makeDummyCompilerSymbol("abcde"))
    ).toThrow();

    expect(() =>
      internalizeInternalSymbol(makeDummyCompilerSymbol("random"))
    ).toThrow();
  });
});

describe("internalizeSymbol", () => {
  it("should call internalizeDeclaredSymbol if namespaced", () => {
    internalizer.internalizeSymbol(makeDummyCompilerSymbol("rose.core/value"));
    internalizer.internalizeSymbol(makeDummyCompilerSymbol("the/thing"));
    internalizer.internalizeSymbol(makeDummyCompilerSymbol("rose.core/*"));

    expect(internalizer.internalizeDeclaredSymbol).toHaveBeenCalledTimes(3);
  });

  it("should call internalizeInternalSymbol if not namespaced", () => {
    internalizer.internalizeSymbol(makeDummyCompilerSymbol("def"));

    expect(internalizer.internalizeInternalSymbol).toHaveBeenCalledTimes(1);
  });
});

describe("internalizeNumber", () => {
  it("should take a number and returns an IrNumber", () => {
    expect(internalizeNumber(makeDummyCompilerNumber(10))).toBeInstanceOf(
      IrNumber
    );
    expect(internalizeNumber(makeDummyCompilerNumber(10)).value).toEqual(10);

    expect(internalizeNumber(makeDummyCompilerNumber(0))).toBeInstanceOf(
      IrNumber
    );
    expect(internalizeNumber(makeDummyCompilerNumber(0)).value).toEqual(0);

    expect(internalizeNumber(makeDummyCompilerNumber(0.3))).toBeInstanceOf(
      IrNumber
    );
    expect(internalizeNumber(makeDummyCompilerNumber(0.3)).value).toEqual(0.3);

    expect(internalizeNumber(makeDummyCompilerNumber(-7))).toBeInstanceOf(
      IrNumber
    );
    expect(internalizeNumber(makeDummyCompilerNumber(-7)).value).toEqual(-7);
  });
});

describe("internalizeString", () => {
  it("should take a string and returns an IrString", () => {
    expect(internalizeString(makeDummyCompilerString("a"))).toBeInstanceOf(
      IrString
    );
    expect(internalizeString(makeDummyCompilerString("a")).value).toEqual("a");

    expect(internalizeString(makeDummyCompilerString(""))).toBeInstanceOf(
      IrString
    );
    expect(internalizeString(makeDummyCompilerString("")).value).toEqual("");

    expect(internalizeString(makeDummyCompilerString("0.3"))).toBeInstanceOf(
      IrString
    );
    expect(internalizeString(makeDummyCompilerString("0.3")).value).toEqual(
      "0.3"
    );
  });
});

describe("internalizeList", () => {
  it("should take a empty list and returns an IrList with empty values", () => {
    const list = internalizeList(makeDummyCompilerList([]));

    expect(list).toBeInstanceOf(IrList);
    expect(list.value).toEqual([]);
    expect(internalizer.internalizeNode).toBeCalledTimes(0);
  });

  it("should take a 3-length list and returns an IrList with 3-length values", () => {
    const list = internalizeList(
      makeDummyCompilerList([
        makeDummyCompilerNumber(1),
        makeDummyCompilerString("a"),
        makeDummyCompilerSymbol("def"),
      ])
    );

    expect(list).toBeInstanceOf(IrList);
    expect(list.value.length).toEqual(3);
    expect(internalizer.internalizeNode).toBeCalledTimes(3);
  });

  it(" should recursevely call it self if one of the nodes is list", () => {
    const list = internalizeList(
      makeDummyCompilerList([
        makeDummyCompilerList([
          makeDummyCompilerNumber(1),
          makeDummyCompilerString("a"),
          makeDummyCompilerSymbol("def"),
        ]),
      ])
    );

    expect(list).toBeInstanceOf(IrList);
    expect(list.value.length).toEqual(1);
    expect(internalizer.internalizeNode).toBeCalledTimes(4);
    expect(internalizer.internalizeList).toBeCalledTimes(2);
  });

  it("should return the correct node instances", () => {
    const list = internalizeList(
      makeDummyCompilerList([
        makeDummyCompilerList([]),
        makeDummyCompilerNumber(1),
        makeDummyCompilerString("a"),
        makeDummyCompilerSymbol("def"),
        makeDummyCompilerSymbol("def/a"),
      ])
    );

    const {
      value: [irList, irNumber, irString, irDefine, irSymbol],
    } = list;

    expect(list).toBeInstanceOf(IrList);
    expect(irList).toBeInstanceOf(IrList);
    expect(irNumber).toBeInstanceOf(IrNumber);
    expect(irString).toBeInstanceOf(IrString);
    expect(irDefine).toBeInstanceOf(IrDefine);
    expect(irSymbol).toBeInstanceOf(IrSymbol);
  });
});

describe("internalizeNode", () => {
  it("should call internalizeSymbol and returns an IrSymbol", () => {
    const symbol = internalizeNode(makeDummyCompilerSymbol("rose.core/value"));

    expect(symbol).toBeInstanceOf(IrSymbol);
    expect(internalizeSymbolSpy).toHaveBeenCalled();
  });

  it("should call internalizeSymbol and returns an IrDefine", () => {
    const symbol = internalizeNode(makeDummyCompilerSymbol("def"));

    expect(symbol).toBeInstanceOf(IrDefine);
    expect(internalizeSymbolSpy).toHaveBeenCalled();
  });

  it("should call internalizeNumber and returns an IrNumber", () => {
    const number = internalizeNode(makeDummyCompilerNumber(10));

    expect(number).toBeInstanceOf(IrNumber);
    expect(internalizeNumberSpy).toHaveBeenCalled();
  });

  it("should call internalizeString and returns an IrString", () => {
    const str = internalizeNode(makeDummyCompilerString("a"));

    expect(str).toBeInstanceOf(IrString);
    expect(internalizeStringSpy).toHaveBeenCalled();
  });

  it("should call internalizeList and returns an IrList", () => {
    const list = internalizeNode(makeDummyCompilerList([]));

    expect(list).toBeInstanceOf(IrList);
    expect(internalizeListSpy).toHaveBeenCalled();
  });
});
