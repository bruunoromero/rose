import {
  IrDefine,
  IrFn,
  IrList,
  IrNode,
  IrNumber,
  IrProgram,
  IrRequire,
  IrString,
  IrSymbol,
} from "../internalizer/ast";
import {
  SquashCall,
  SquashDefine,
  SquashFn,
  SquashNode,
  SquashNumber,
  SquashProgram,
  SquashRequire,
  SquashString,
  SquashSymbol,
} from "./ast";

export const squashNumber = (node: IrNumber) => new SquashNumber(node);

export const squashString = (node: IrString) => new SquashString(node);

export const squashSymbol = (node: IrSymbol) => new SquashSymbol(node);

export const squashRequire = (node: IrList) => {
  const [requireNode, required] = node.value;

  if (!(requireNode instanceof IrRequire))
    throw new Error("cannot squash node other than IrRequire");

  if (!(required instanceof IrSymbol)) {
    throw new Error("require should take a symbol");
  }

  return new SquashRequire(node, squashSymbol(required));
};

export const squashDefine = (node: IrList): SquashDefine => {
  const [defineNode, symbol, value] = node.value;

  if (!(defineNode instanceof IrDefine)) {
    throw new Error("cannot squash node other than IrDefine");
  }

  if (!(symbol instanceof IrSymbol)) {
    throw new Error("define should take a symbol as first argument");
  }

  if (!value) {
    throw new Error("define requires a value");
  }

  return new SquashDefine(node, squashSymbol(symbol), squashNode(value));
};

export const squashFn = (node: IrList): SquashFn => {
  const [fnNode, list, value] = node.value;

  if (!(fnNode instanceof IrFn)) {
    throw new Error("cannot squash node other than IrFn");
  }

  if (!(list instanceof IrList)) {
    throw new Error("fn should take a list as first argument");
  }

  if (!list.value.every((node) => node instanceof IrSymbol)) {
    throw new Error("fn should take a list where all elements are symbols");
  }

  if (!value) {
    throw new Error("fn requires a value");
  }

  return new SquashFn(node, list.value.map(squashSymbol), squashNode(value));
};

export const squashCall = (node: IrList): SquashCall => {
  const [callee, ...args] = node.value;

  if (!callee) {
    throw new Error("cannot call from undefined");
  }

  return new SquashCall(node, squashNode(callee), args.map(squashNode));
};

export const squashList = (node: IrList) => {
  const {
    value: [firstNode],
  } = node;

  if (firstNode instanceof IrRequire) {
    return squashRequire(node);
  }

  if (firstNode instanceof IrDefine) {
    return squashDefine(node);
  }

  if (firstNode instanceof IrFn) {
    return squashFn(node);
  }

  return squashCall(node);
};

export const squashNode = (node: IrNode) => {
  if (node instanceof IrList) {
    return squashList(node);
  }

  if (node instanceof IrNumber) {
    return squashNumber(node);
  }

  if (node instanceof IrString) {
    return squashString(node);
  }

  if (node instanceof IrSymbol) {
    return squashSymbol(node);
  }

  throw new Error(`cannot squash node ${node.type}`);
};

export const squash = ({ nodes }: IrProgram) =>
  new SquashProgram(nodes.map(squashNode));
