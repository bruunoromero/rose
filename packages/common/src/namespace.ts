import { Constants } from "./constants";

export class Namespace {
  unnamespace(name: string): [namespace: string, symbol: string] {
    const parts = name.split(Constants.NAMESPACE_SEPARATOR);
    const [namespace, symbol] = parts;

    if (parts.length > 2) throw new Error("reference with  more than 2 parts");

    if (parts.length < 2) throw new Error("reference with less than 2 parts");

    if (!namespace) throw new Error("namespace is empty");
    if (!symbol) throw new Error("symbol is empty");

    return parts as [namespace: string, symbol: string];
  }
}
