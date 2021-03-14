export interface Position {
  line: number;
  offset?: number;
  column: number;
}

export interface Location {
  end: Position;
  start: Position;
}
