import { Gettable } from "./gettable";
import { Mappable } from "./mappable";

import { RoseValue } from "./value";

export class RoseList<T>
  implements Gettable<RoseList<T>>, Mappable<RoseValue<T>, RoseList<T>> {
  private constructor(
    private _data: RoseValue<RoseValue<T>[]> = RoseValue.fromConstant([])
  ) {}

  static new<T>(value: RoseValue<RoseValue<T>[]>): RoseList<T> {
    return new RoseList(value);
  }

  static fromConstant<T>(data: RoseValue<T>[]): RoseList<T> {
    return RoseList.new(RoseValue.fromConstant(data));
  }

  get() {
    return this;
  }

  head(): RoseValue<T | null> {
    if (this._data.get().length > 0) {
      return this._data.get()[0] as RoseValue<T>;
    }

    return RoseValue.fromConstant(null);
  }

  tail() {
    return RoseList.new(RoseValue.new(() => this._data.get().slice(1)));
  }

  map(fn: (node: RoseValue<T>) => RoseValue<T>) {
    return RoseList.new(RoseValue.new(() => this._data.get().map(fn)));
  }

  filter(fn: (node: RoseValue<T>) => RoseValue<boolean>) {
    return RoseList.new(
      RoseValue.new(() => this._data.get().filter((node) => fn(node).get()))
    );
  }
}
