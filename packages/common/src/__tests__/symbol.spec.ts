import "jest";
import { Normalized } from "../constants";
import { Symbol } from "../symbol";

const symbol = new Symbol();

describe("normalize", () => {
  it("should take an string and returns a new string without special characters", () => {
    expect(symbol.normalize("<")).toEqual(Normalized.LT);
    expect(symbol.normalize(">")).toEqual(Normalized.GT);
    expect(symbol.normalize("=")).toEqual(Normalized.EQ);
    expect(symbol.normalize(".")).toEqual(Normalized.DOT);
    expect(symbol.normalize("!")).toEqual(Normalized.BANG);
    expect(symbol.normalize("*")).toEqual(Normalized.STAR);
    expect(symbol.normalize("+")).toEqual(Normalized.PLUS);
    expect(symbol.normalize("-")).toEqual(Normalized.MINUS);
    expect(symbol.normalize("?")).toEqual(Normalized.QMARK);
    expect(symbol.normalize("/")).toEqual(Normalized.FSLASH);
    expect(symbol.normalize("\\")).toEqual(Normalized.BSLASK);

    expect(symbol.normalize("<>=")).toEqual(
      `${Normalized.LT}${Normalized.GT}${Normalized.EQ}`
    );
    expect(symbol.normalize("*!.")).toEqual(
      `${Normalized.STAR}${Normalized.BANG}${Normalized.DOT}`
    );

    expect(symbol.normalize("abc*def!.ghi")).toEqual(
      `abc${Normalized.STAR}def${Normalized.BANG}${Normalized.DOT}ghi`
    );
  });

  it("should return the same string with it does not contains special characters", () => {
    expect(symbol.normalize("abcd")).toEqual("abcd");
    expect(symbol.normalize("rose")).toEqual("rose");
    expect(symbol.normalize("1234")).toEqual("1234");
  });
});
