import {
  ParserList,
  ParserNumber,
  ParserString,
  ParserSymbol,
  ParserConcreteNode,
} from "@rose/common";
import { emptyLocation } from "./location";

export const makeDummyIrSymbol = (value: string) =>
  new ParserSymbol(emptyLocation, value);

export const makeDummyIrString = (value: string) =>
  new ParserString(emptyLocation, value);

export const makeDummyIrNumber = (value: number) =>
  new ParserNumber(emptyLocation, value);

export const makeDummyIrList = (value: ParserConcreteNode[]) =>
  new ParserList(emptyLocation, value);
