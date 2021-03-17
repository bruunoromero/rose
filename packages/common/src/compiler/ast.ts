import { CompilerType } from "./types";
import { Location } from "../ast";

export abstract class CompilerNode<T> {
  constructor(public readonly loc: Location, public readonly value: T) {}

  get type() {
    if (this instanceof CompilerString) {
      return CompilerType.STRING;
    }

    if (this instanceof CompilerSymbol) {
      return CompilerType.SYMBOL;
    }

    if (this instanceof CompilerNumber) {
      return CompilerType.NUMBER;
    }

    if (this instanceof CompilerList) {
      return CompilerType.LIST;
    }

    throw new Error("unexpected type");
  }
}

export type CompilerConcreteNode =
  | CompilerList
  | CompilerString
  | CompilerSymbol
  | CompilerNumber;

export class CompilerString extends CompilerNode<string> {}
export class CompilerSymbol extends CompilerNode<string> {}
export class CompilerList extends CompilerNode<CompilerConcreteNode[]> {}

export class CompilerNumber extends CompilerNode<number> {}

export class CompilerProgram {
  constructor(
    public readonly namespace: string,
    public readonly nodes: CompilerConcreteNode[]
  ) {}
}
