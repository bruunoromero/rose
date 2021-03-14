export interface Mappable<T, D> {
  map(fn: (value: T) => T): D;
}
