import "jest";
import { Namespace } from "../namespace";

const namespace = new Namespace();

describe("unnamespace", () => {
  it("should take an string and returns the namespace and value", () => {
    expect(namespace.unnamespace("a/b")).toEqual(["a", "b"]);
    expect(namespace.unnamespace("___/+++")).toEqual(["___", "+++"]);
    expect(namespace.unnamespace("rose.core/value")).toEqual([
      "rose.core",
      "value",
    ]);
  });

  it("should should throw an expection if the string has more than 2 parts", () => {
    expect(() => namespace.unnamespace("a/b/c")).toThrow();
    expect(() => namespace.unnamespace("___/+++/---")).toThrow();
    expect(() => namespace.unnamespace("rose.core/value/some")).toThrow();
  });

  it("should should throw an expection if the string has less than 2 parts", () => {
    expect(() => namespace.unnamespace("a")).toThrow();
    expect(() => namespace.unnamespace("___")).toThrow();
    expect(() => namespace.unnamespace("rose.core")).toThrow();
  });

  it("should throw if at least one of the parts is empty", () => {
    expect(() => namespace.unnamespace("/")).toThrow();
    expect(() => namespace.unnamespace("a/")).toThrow();
    expect(() => namespace.unnamespace("/b")).toThrow();
  });
});
