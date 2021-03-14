import { RoseValue } from "./value";

export class Numbers {
  static add(
    left: RoseValue<number>,
    right: RoseValue<number>
  ): RoseValue<number> {
    return left.map((value) => value + right.get());
  }

  static sub(
    left: RoseValue<number>,
    right: RoseValue<number>
  ): RoseValue<number> {
    return left.map((value) => value - right.get());
  }

  static mul(
    left: RoseValue<number>,
    right: RoseValue<number>
  ): RoseValue<number> {
    return left.map((value) => value * right.get());
  }

  static div(
    left: RoseValue<number>,
    right: RoseValue<number>
  ): RoseValue<number> {
    return left.map((value) => value / right.get());
  }

  static mod(
    left: RoseValue<number>,
    right: RoseValue<number>
  ): RoseValue<number> {
    return left.map((value) => value % right.get());
  }

  static pow(
    left: RoseValue<number>,
    right: RoseValue<number>
  ): RoseValue<number> {
    return left.map((value) => Math.pow(value, right.get()));
  }
}
