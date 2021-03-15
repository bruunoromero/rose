import { Location, Position } from "@rose/common";

export const emptyPosition: Position = {
  line: 0,
  column: 0,
};

export const emptyLocation: Location = {
  start: emptyPosition,
  end: emptyPosition,
};
