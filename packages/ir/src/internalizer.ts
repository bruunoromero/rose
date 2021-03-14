import {
  Program,
  ParserList,
  ParserConcreteNode,
  ParserNumber,
  ParserString,
  ParserSymbol,
  CompilerNodeType,
} from "@rose/common";

import { isInternalSymbol } from "./logic/symbol";
import { IrCall, IrDefine, IrNode, IrNumber, IrString } from "./ast";

const internalizeExpr = (node: ParserConcreteNode): IrNode => {
  if (node instanceof ParserList) {
    return internalizeList(node);
  }

  if (node instanceof ParserNumber) {
    return internalizeNumber(node);
  }

  if (node instanceof ParserString) {
    return internalizeString(node);
  }

  if (node instanceof ParserSymbol) {
    return internalizeSymbol(node);
  }

  throw new Error("unexpected type");
};

const internalizeList = (listNode: ParserList) => {
  const [callee, ...args] = listNode.value.map((node) => internalizeExpr(node));

  if (!callee) throw new Error("cannot call from undefined");

  return new IrCall(listNode, callee, args);
};

export const internalizeInternalSymbol = (symbolNode: ParserSymbol) => {
  switch (symbolNode.value) {
    case CompilerNodeType.DEFINE:
      return new IrDefine(symbolNode);
    default:
      throw new Error("unexpected symbol");
  }
};

export const internalizeDeclaredSymbol = (symbolNode: ParserSymbol) => {
  switch (symbolNode.value) {
    case CompilerNodeType.DEFINE:
      return new IrDefine(symbolNode);
    default:
      throw new Error("unexpected symbol");
  }
};

const internalizeNumber = (numberNode: ParserNumber) =>
  new IrNumber(numberNode);

const internalizeString = (stringNode: ParserString) =>
  new IrString(stringNode);

const internalizeSymbol = (symbolNode: ParserSymbol) => {
  if (isInternalSymbol(symbolNode)) {
    return internalizeInternalSymbol(symbolNode);
  } else {
    return internalizeDeclaredSymbol(symbolNode);
  }
};

export const internalize = (program: Program) => {
  const internalizedExprs = program.exprs.map((expr) => internalizeExpr(expr));

  return internalizedExprs;
};
