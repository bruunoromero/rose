import {
  Location,
  ParserConcreteNode,
  ParserList,
  ParserNode,
  Program,
} from "@rose/common";
import { IrNodeType } from "./types";

export abstract class IrNode<T = any> {
  readonly loc: Location;
  readonly value: T;

  constructor(node: ParserNode<T>) {
    this.loc = node.loc;
    this.value = node.value;
  }

  get type() {
    if (this instanceof IrDefine) {
      return IrNodeType.DEFINE;
    }

    if (this instanceof IrCall) {
      return IrNodeType.CALL;
    }

    if (this instanceof IrSymbol) {
      return IrNodeType.SYMBOL;
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

export class IrDefine extends IrNode<string> {}
export class IrSymbol extends IrNode<string> {}
export class IrNumber extends IrNode<number> {}
export class IrString extends IrNode<string> {}
export class IrList extends IrNode<IrNode[]> {}
export class IrCall extends IrNode<ParserConcreteNode[]> {
  constructor(
    node: ParserList,
    public readonly callee: IrNode<any>,
    public readonly args: IrNode<any>[]
  ) {
    super(node);
  }
}
export class IrProgram {
  constructor(
    public readonly namespace: string,
    public readonly nodes: IrNode[]
  ) {}
}
