import {
  CompilerList,
  CompilerNumber,
  CompilerString,
  CompilerSymbol,
  CompilerNodeType,
  CompilerConcreteNode,
  CompilerProgram,
} from "@rose/common";
import {
  IrNode,
  IrList,
  IrDefine,
  IrSymbol,
  IrNumber,
  IrString,
  IrProgram,
  IrRequire,
  IrFn,
} from "./ast";

import { isInternalSymbol } from "../logic/symbol";

export const internalizeNode = (node: CompilerConcreteNode): IrNode => {
  if (node instanceof CompilerList) {
    return internalizeList(node);
  }

  if (node instanceof CompilerNumber) {
    return internalizeNumber(node);
  }

  if (node instanceof CompilerString) {
    return internalizeString(node);
  }

  if (node instanceof CompilerSymbol) {
    return internalizeSymbol(node);
  }

  throw new Error("unexpected node");
};

export const internalizeList = ({ type, loc, value }: CompilerList) =>
  new IrList({
    loc,
    type,
    value: value.map((node) => internalizeNode(node)),
  });

export const internalizeInternalSymbol = (symbolNode: CompilerSymbol) => {
  switch (symbolNode.value) {
    case CompilerNodeType.FN:
      return new IrFn(symbolNode);
    case CompilerNodeType.DEFINE:
      return new IrDefine(symbolNode);
    case CompilerNodeType.REQUIRE:
      return new IrRequire(symbolNode);
    default:
      throw new Error(`unexpected symbol ${symbolNode.value}`);
  }
};

export const internalizeDeclaredSymbol = (symbolNode: CompilerSymbol) =>
  new IrSymbol(symbolNode);

export const internalizeNumber = (numberNode: CompilerNumber) =>
  new IrNumber(numberNode);

export const internalizeString = (stringNode: CompilerString) =>
  new IrString(stringNode);

export const internalizeSymbol = (symbolNode: CompilerSymbol) =>
  isInternalSymbol(symbolNode)
    ? internalizeInternalSymbol(symbolNode)
    : internalizeDeclaredSymbol(symbolNode);

export const internalize = ({ nodes, namespace }: CompilerProgram) =>
  new IrProgram(
    namespace,
    nodes.map((expr) => internalizeNode(expr))
  );
