import { Normalized, Unormalized } from "./constants";

const r = (regex: string) => new RegExp(regex, "g");

export class Symbol {
  normalize(name: string) {
    return name
      .replace(r(Unormalized.MINUS), Normalized.MINUS)
      .replace(r(Unormalized.GT), Normalized.GT)
      .replace(r(Unormalized.LT), Normalized.LT)
      .replace(r(Unormalized.EQ), Normalized.EQ)
      .replace(r(Unormalized.BANG), Normalized.BANG)
      .replace(r(Unormalized.DOT), Normalized.DOT)
      .replace(r(Unormalized.PIPE), Normalized.PIPE)
      .replace(r(Unormalized.STAR), Normalized.STAR)
      .replace(r(Unormalized.PLUS), Normalized.PLUS)
      .replace(r(Unormalized.QMARK), Normalized.QMARK)
      .replace(r(Unormalized.FSLASH), Normalized.FSLASH)
      .replace(r(Unormalized.BSLASK), Normalized.BSLASK);
  }
}
