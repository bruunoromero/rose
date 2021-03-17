import { Namespace } from "./namespace";
import { Symbol } from "./symbol";
import { Utils } from "./utils";
export { Constants } from "./constants";
export * from "./ast";
export * from "./codegen";
export * from "./compiler";

export const utils = new Utils(new Symbol(), new Namespace());
