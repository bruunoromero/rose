import { CompilerConcreteNode, CompilerNodeType } from "@rose/common";
import {
  emptyLocation,
  makeDummyCompilerList,
  makeDummyCompilerNumber,
  makeDummyCompilerString,
  makeDummyCompilerSymbol,
} from "@rose/test-helper";
import {
  IrDefine,
  IrFn,
  IrList,
  IrNode,
  IrNumber,
  IrRequire,
  IrString,
  IrSymbol,
} from "../internalizer/ast";

export const makeDummyInternalizerSymbol = (value: string) =>
  new IrSymbol(makeDummyCompilerSymbol(value));

export const makeDummyInternalizerString = (value: string) =>
  new IrString(makeDummyCompilerString(value));

export const makeDummyInternalizerNumber = (value: number) =>
  new IrNumber(makeDummyCompilerNumber(value));

export const makeDummyInternalizerList = (value: IrNode[]) =>
  new IrList({
    value,
    loc: emptyLocation,
    type: makeDummyCompilerList([]).type,
  });

export const makeDummyInternalizerRequire = () =>
  new IrRequire(makeDummyCompilerSymbol(CompilerNodeType.REQUIRE));

export const makeDummyInternalizerDefine = () =>
  new IrDefine(makeDummyCompilerSymbol(CompilerNodeType.DEFINE));

export const makeDummyInternalizerFn = () =>
  new IrFn(makeDummyCompilerSymbol(CompilerNodeType.FN));
