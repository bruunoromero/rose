import "jest";
import {
  squashCall,
  squashDefine,
  squashFn,
  squashNumber,
  squashRequire,
  squashString,
  squashSymbol,
} from "../squasher";
import {
  SquashCall,
  SquashDefine,
  SquashFn,
  SquashNumber,
  SquashRequire,
  SquashString,
  SquashSymbol,
} from "../squasher/ast";
import {
  makeDummyInternalizerDefine,
  makeDummyInternalizerFn,
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

describe("squashDefine", () => {
  it("should take an IrList with IrDefine as first node and returns an SquashDefine", () => {
    const requireNode1 = squashDefine(
      makeDummyInternalizerList([
        makeDummyInternalizerDefine(),
        makeDummyInternalizerSymbol("symbol"),
        makeDummyInternalizerNumber(10),
      ])
    );

    const requireNode2 = squashDefine(
      makeDummyInternalizerList([
        makeDummyInternalizerDefine(),
        makeDummyInternalizerSymbol("rose.core"),
        makeDummyInternalizerSymbol("rose.core"),
      ])
    );

    expect(requireNode1).toBeInstanceOf(SquashDefine);
    expect(requireNode1.symbol.value).toEqual("symbol");
    expect(requireNode1.symbol).toBeInstanceOf(SquashSymbol);
    expect(requireNode1.value).toBeInstanceOf(SquashNumber);

    expect(requireNode2).toBeInstanceOf(SquashDefine);
    expect(requireNode2.symbol.value).toEqual("rose.core");
    expect(requireNode2.value).toBeInstanceOf(SquashSymbol);
    expect(requireNode2.symbol).toBeInstanceOf(SquashSymbol);
  });

  it("should throw an error when the first node is not an IrDefine", () => {
    const requireNode1 = () =>
      squashDefine(
        makeDummyInternalizerList([
          makeDummyInternalizerRequire(),
          makeDummyInternalizerSymbol("symbol"),
          makeDummyInternalizerNumber(10),
        ])
      );

    const requireNode2 = () =>
      squashDefine(
        makeDummyInternalizerList([
          makeDummyInternalizerSymbol("a"),
          makeDummyInternalizerSymbol("rose.core"),
          makeDummyInternalizerSymbol("rose.core"),
        ])
      );

    expect(requireNode1).toThrow();
    expect(requireNode2).toThrow();
  });

  it("should throw an error when the second node is not an IrSymbol", () => {
    const requireNode1 = () =>
      squashDefine(
        makeDummyInternalizerList([
          makeDummyInternalizerDefine(),
          makeDummyInternalizerNumber(10),
          makeDummyInternalizerSymbol("symbol"),
        ])
      );

    const requireNode2 = () =>
      squashDefine(
        makeDummyInternalizerList([
          makeDummyInternalizerDefine(),
          makeDummyInternalizerString("rose.core"),
          makeDummyInternalizerSymbol("rose.core"),
        ])
      );

    expect(requireNode1).toThrow();
    expect(requireNode2).toThrow();
  });

  it("should throw an error when the third node is not defined", () => {
    const requireNode1 = () =>
      squashDefine(
        makeDummyInternalizerList([
          makeDummyInternalizerDefine(),
          makeDummyInternalizerSymbol("symbol"),
        ])
      );

    expect(requireNode1).toThrow();
  });
});

describe("squashFn", () => {
  it("should take an IrList with IrFn as first node and returns an SquashFn", () => {
    const requireNode1 = squashFn(
      makeDummyInternalizerList([
        makeDummyInternalizerFn(),
        makeDummyInternalizerList([]),
        makeDummyInternalizerNumber(10),
      ])
    );

    const requireNode2 = squashFn(
      makeDummyInternalizerList([
        makeDummyInternalizerFn(),
        makeDummyInternalizerList([
          makeDummyInternalizerSymbol("a"),
          makeDummyInternalizerSymbol("b"),
        ]),
        makeDummyInternalizerSymbol("rose.core"),
      ])
    );

    expect(requireNode1.args).toEqual([]);
    expect(requireNode1).toBeInstanceOf(SquashFn);
    expect(requireNode1.value).toBeInstanceOf(SquashNumber);

    expect(requireNode2).toBeInstanceOf(SquashFn);
    expect(requireNode2.args[0]?.value).toEqual("a");
    expect(requireNode2.args[1]?.value).toEqual("b");
    expect(requireNode2.value).toBeInstanceOf(SquashSymbol);
  });

  it("should throw an error when the first node is not an IrFn", () => {
    const requireNode1 = () =>
      squashFn(
        makeDummyInternalizerList([
          makeDummyInternalizerRequire(),
          makeDummyInternalizerList([
            makeDummyInternalizerSymbol("a"),
            makeDummyInternalizerSymbol("b"),
          ]),
          makeDummyInternalizerSymbol("rose.core"),
        ])
      );

    const requireNode2 = () =>
      squashFn(
        makeDummyInternalizerList([
          makeDummyInternalizerSymbol("a"),
          makeDummyInternalizerList([
            makeDummyInternalizerSymbol("a"),
            makeDummyInternalizerSymbol("b"),
          ]),
          makeDummyInternalizerSymbol("rose.core"),
        ])
      );

    expect(requireNode1).toThrow();
    expect(requireNode2).toThrow();
  });

  it("should throw an error when the second node is not an IrList", () => {
    const requireNode1 = () =>
      squashFn(
        makeDummyInternalizerList([
          makeDummyInternalizerFn(),
          makeDummyInternalizerSymbol("rose.core"),
          makeDummyInternalizerSymbol("symbol"),
        ])
      );

    const requireNode2 = () =>
      squashFn(
        makeDummyInternalizerList([
          makeDummyInternalizerFn(),
          makeDummyInternalizerNumber(10),
          makeDummyInternalizerSymbol("rose.core"),
        ])
      );

    expect(requireNode1).toThrow();
    expect(requireNode2).toThrow();
  });

  it("should not all nodes of the params list is IrSymbol", () => {
    const requireNode1 = () =>
      squashFn(
        makeDummyInternalizerList([
          makeDummyInternalizerFn(),
          makeDummyInternalizerList([
            makeDummyInternalizerSymbol("a"),
            makeDummyInternalizerString("b"),
          ]),
        ])
      );

    const requireNode2 = () =>
      squashFn(
        makeDummyInternalizerList([
          makeDummyInternalizerFn(),
          makeDummyInternalizerList([
            makeDummyInternalizerString("b"),
            makeDummyInternalizerSymbol("a"),
          ]),
        ])
      );

    const requireNode3 = () =>
      squashFn(
        makeDummyInternalizerList([
          makeDummyInternalizerFn(),
          makeDummyInternalizerList([
            makeDummyInternalizerString("a"),
            makeDummyInternalizerNumber(10),
          ]),
        ])
      );

    expect(requireNode1).toThrow();
    expect(requireNode2).toThrow();
    expect(requireNode3).toThrow();
  });

  it("should throw an error when the third node is not defined", () => {
    const requireNode1 = () =>
      squashFn(
        makeDummyInternalizerList([
          makeDummyInternalizerFn(),
          makeDummyInternalizerList([
            makeDummyInternalizerSymbol("a"),
            makeDummyInternalizerSymbol("b"),
          ]),
        ])
      );

    expect(requireNode1).toThrow();
  });
});

describe("squashCall", () => {
  it("should take an IrList and returns a SquashCall", () => {
    const requireNode1 = squashCall(
      makeDummyInternalizerList([
        makeDummyInternalizerSymbol("a"),
        makeDummyInternalizerNumber(10),
      ])
    );

    const requireNode2 = squashCall(
      makeDummyInternalizerList([
        makeDummyInternalizerList([
          makeDummyInternalizerSymbol("b"),
          makeDummyInternalizerSymbol("rose.core"),
          makeDummyInternalizerNumber(10),
        ]),
        makeDummyInternalizerSymbol("rose.core"),
        makeDummyInternalizerString("hi!"),
      ])
    );

    const requireNode3 = squashCall(
      makeDummyInternalizerList([
        makeDummyInternalizerSymbol("rose.core/value"),
      ])
    );

    expect(requireNode1).toBeInstanceOf(SquashCall);
    expect(requireNode1.callee).toBeInstanceOf(SquashSymbol);
    expect(requireNode1.args[0]).toBeInstanceOf(SquashNumber);

    expect(requireNode2).toBeInstanceOf(SquashCall);
    expect(requireNode2.callee).toBeInstanceOf(SquashCall);
    expect(requireNode2.args[0]).toBeInstanceOf(SquashSymbol);
    expect(requireNode2.args[1]).toBeInstanceOf(SquashString);

    expect(requireNode3.args).toEqual([]);
    expect(requireNode3).toBeInstanceOf(SquashCall);
    expect(requireNode3.callee).toBeInstanceOf(SquashSymbol);
  });

  it("should throw an error when the first node is not defined", () => {
    const requireNode1 = () => squashFn(makeDummyInternalizerList([]));

    expect(requireNode1).toThrow();
  });
});
