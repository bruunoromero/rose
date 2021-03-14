import {
  Location,
  ParserConcreteNode,
  ParserList,
  ParserNode,
  ParserNumber,
  ParserString,
  ParserSymbol,
} from "@rose/common";
import { IrNodeType } from "./type";

export abstract class IrNode<T = any, C extends ParserNode<T> = ParserNode<T>> {
  readonly loc: Location;
  readonly value: T;

  constructor(node: C) {
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

    return new Error("unexpected type");
  }
}

export class IrDefine extends IrNode<string, ParserSymbol> {}
export class IrSymbol extends IrNode<string, ParserSymbol> {}
export class IrNumber extends IrNode<number, ParserNumber> {}
export class IrString extends IrNode<string, ParserString> {}
export class IrCall extends IrNode<ParserConcreteNode[], ParserList> {
  constructor(
    node: ParserList,
    public readonly callee: IrNode,
    public readonly args: IrNode[]
  ) {
    super(node);
  }
}
