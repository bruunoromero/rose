import { Namespace } from "./namespace";
import { Symbol } from "./symbol";

export class Utils {
  constructor(
    public readonly symbol: Symbol,
    public readonly namespace: Namespace
  ) {}
}
