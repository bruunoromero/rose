import { Constants } from "./constants";

export class Namespace {
  unnamespace(name: string): [namespace: string, symbol: string] {
    const parts = name.split(Constants.NAMESPACE_SEPARATOR);

    if (parts.length === 2) {
      return parts as [string, string];
    }

    throw new Error("reference with more the 2 parts");
  }
}
