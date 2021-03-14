import { CodegenNodeType } from "./types";

export abstract class CodegenNode {
  get type() {
    if (this instanceof CodegenDefine) {
      return CodegenNodeType.DEFINE;
    }

    if (this instanceof CodegenSymbol) {
      return CodegenNodeType.SYMBOL;
    }

    if (this instanceof CodegenDot) {
      return CodegenNodeType.DOT;
    }

    if (this instanceof CodegenNumber) {
      return CodegenNodeType.NUMBER;
    }

    if (this instanceof CodegenCall) {
      return CodegenNodeType.CALL;
    }

    return new Error("unexpected node");
  }
}

export class CodegenDefine extends CodegenNode {}
export class CodegenSymbol extends CodegenNode {
  constructor(public readonly name: string) {
    super();
  }
}

export class CodegenDot extends CodegenNode {
  constructor(
    public readonly owner: CodegenNode,
    public readonly prop: CodegenNode
  ) {
    super();
  }
}

export class CodegenCall extends CodegenNode {
  constructor(
    public readonly callee: CodegenNode,
    public readonly args: CodegenNode[]
  ) {
    super();
  }
}

export class CodegenNumber extends CodegenNode {
  constructor(public readonly value: number) {
    super();
  }
}
