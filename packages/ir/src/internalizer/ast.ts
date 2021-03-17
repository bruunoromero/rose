import { Location, CompilerNode } from "@rose/common";
import { IrNodeType } from "./types";

export abstract class IrNode<T = any> {
  readonly loc: Location;
  readonly value: T;

  constructor(node: CompilerNode<T>) {
    this.loc = node.loc;
    this.value = node.value;
  }

  get type() {
    if (this instanceof IrDefine) {
      return IrNodeType.DEFINE;
    }

    if (this instanceof IrDefine) {
      return IrNodeType.DEFINE;
    }

    if (this instanceof IrRequire) {
      return IrNodeType.REQUIRE;
    }

    if (this instanceof IrFn) {
      return IrNodeType.FN;
    }

    if (this instanceof IrString) {
      return IrNodeType.STRING;
    }

    if (this instanceof IrNumber) {
      return IrNodeType.NUMBER;
    }

    if (this instanceof IrList) {
      return IrNodeType.LIST;
    }

    return new Error("unexpected type");
  }
}

export class IrFn extends IrNode<string> {}
export class IrDefine extends IrNode<string> {}
export class IrRequire extends IrNode<string> {}
export class IrSymbol extends IrNode<string> {}
export class IrNumber extends IrNode<number> {}
export class IrString extends IrNode<string> {}
export class IrList extends IrNode<IrNode[]> {}
export class IrProgram {
  constructor(
    public readonly namespace: string,
    public readonly nodes: IrNode[]
  ) {}
}
