import { Location } from "@rose/common";
import { IrNode } from "../internalizer/ast";

export abstract class SquashNode<T = any> {
  readonly loc: Location;

  constructor(node: IrNode<T>) {
    this.loc = node.loc;
  }

  get type() {
    return new Error("unexpected type");
  }
}

export abstract class SquashValue<T = any> extends SquashNode<T> {
  readonly value: T;
  constructor(node: IrNode<T>) {
    super(node);
    this.value = node.value;
  }
}

export class SquashNumber extends SquashValue<number> {}
export class SquashString extends SquashValue<string> {}
export class SquashSymbol extends SquashValue<string> {}

export class SquashRequire extends SquashNode {
  constructor(node: IrNode, public readonly symbol: SquashSymbol) {
    super(node);
  }
}

export class SquashDefine extends SquashNode {
  constructor(
    node: IrNode,
    public readonly symbol: SquashSymbol,
    public readonly value: SquashNode
  ) {
    super(node);
  }
}

export class SquashFn extends SquashNode {
  constructor(
    node: IrNode,
    public readonly args: SquashSymbol[],
    public readonly value: SquashNode
  ) {
    super(node);
  }
}

export class SquashCall extends SquashNode {
  constructor(
    node: IrNode,
    public readonly callee: SquashNode,
    public readonly args: SquashNode[]
  ) {
    super(node);
  }
}

export class SquashProgram {
  constructor(public readonly nodes: SquashNode[]) {}
}
