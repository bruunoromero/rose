import {
  Program,
  ParserList,
  ParserNumber,
  ParserString,
  ParserSymbol,
  CompilerNodeType,
  ParserConcreteNode,
} from "@rose/common";

import { isInternalSymbol } from "./logic/symbol";
import {
  IrDefine,
  IrList,
  IrNode,
  IrNumber,
  IrProgram,
  IrString,
  IrSymbol,
} from "./ast";

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

  throw new Error("unexpected node");
};

const internalizeList = ({ type, loc, value }: ParserList) =>
  new IrList({
    loc,
    type,
    value: value.map((node) => internalizeExpr(node)),
  });

export const internalizeInternalSymbol = (symbolNode: ParserSymbol) => {
  switch (symbolNode.value) {
    case CompilerNodeType.DEFINE:
      return new IrDefine(symbolNode);
    default:
      throw new Error(`unexpected symbol ${symbolNode.value}`);
  }
};

export const internalizeDeclaredSymbol = (symbolNode: ParserSymbol) =>
  new IrSymbol(symbolNode);

const internalizeNumber = (numberNode: ParserNumber) =>
  new IrNumber(numberNode);

const internalizeString = (stringNode: ParserString) =>
  new IrString(stringNode);

const internalizeSymbol = (symbolNode: ParserSymbol) =>
  isInternalSymbol(symbolNode)
    ? internalizeInternalSymbol(symbolNode)
    : internalizeDeclaredSymbol(symbolNode);

export const internalize = ({ exprs, namespace }: Program) =>
  new IrProgram(
    namespace,
    exprs.map((expr) => internalizeExpr(expr))
  );
