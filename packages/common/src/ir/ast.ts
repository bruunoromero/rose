import { ParserType } from "./types";
import { Location } from "../ast";

export abstract class ParserNode<T> {
  constructor(public readonly loc: Location, public readonly value: T) {}

  get type() {
    if (this instanceof ParserString) {
      return ParserType.STRING;
    }

    if (this instanceof ParserSymbol) {
      return ParserType.SYMBOL;
    }

    if (this instanceof ParserNumber) {
      return ParserType.NUMBER;
    }

    if (this instanceof ParserList) {
      return ParserType.LIST;
    }

    throw new Error("unexpected type");
  }
}

export type ParserConcreteNode =
  | ParserList
  | ParserString
  | ParserSymbol
  | ParserNumber;

export class ParserString extends ParserNode<string> {}
export class ParserSymbol extends ParserNode<string> {}
export class ParserList extends ParserNode<ParserConcreteNode[]> {}

export class ParserNumber extends ParserNode<number> {}

export class Program {
  constructor(
    public readonly namespace: string,
    public readonly exprs: ParserConcreteNode[]
  ) {}
}
