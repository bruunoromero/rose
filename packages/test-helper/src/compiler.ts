import {
  CompilerList,
  CompilerNumber,
  CompilerString,
  CompilerSymbol,
  CompilerConcreteNode,
} from "@rose/common";
import { emptyLocation } from "./location";

export const makeDummyCompilerSymbol = (value: string) =>
  new CompilerSymbol(emptyLocation, value);

export const makeDummyCompilerString = (value: string) =>
  new CompilerString(emptyLocation, value);

export const makeDummyCompilerNumber = (value: number) =>
  new CompilerNumber(emptyLocation, value);

export const makeDummyCompilerList = (value: CompilerConcreteNode[]) =>
  new CompilerList(emptyLocation, value);
