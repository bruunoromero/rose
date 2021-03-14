import { Gettable } from "./gettable";
import { Lazy } from "./lazy";
import { Mappable } from "./mappable";

export class RoseValue<T> implements Gettable<T>, Mappable<T, RoseValue<T>> {
  private _cache: T | undefined;
  private constructor(private _getter: Lazy<T>) {}

  static new<T>(getter: Lazy<T>): RoseValue<T> {
    return new RoseValue(getter);
  }

  static fromConstant<T>(value: T): RoseValue<T> {
    return RoseValue.new(() => value);
  }

  static fromAny<T>(getterOrValue: Lazy<T> | RoseValue<T> | T): RoseValue<T> {
    if (getterOrValue instanceof RoseValue) {
      return getterOrValue;
    }

    if (getterOrValue instanceof Function) {
      return RoseValue.new(getterOrValue);
    }

    return RoseValue.fromConstant(getterOrValue);
  }

  get() {
    if (this._cache) {
      return this._cache;
    }

    return (this._cache = this._getter());
  }

  map(fn: (value: T) => T): RoseValue<T> {
    return RoseValue.new(() => fn(this.get()));
  }

  toString() {
    return `RoseValue(${this.get()})`;
  }
}
