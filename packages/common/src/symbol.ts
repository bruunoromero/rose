export class Symbol {
  normalize(name: string) {
    return name
      .replace(/-/g, "_")
      .replace(/>/g, "_GT_")
      .replace(/</g, "_LT_")
      .replace(/=/g, "_EQ_")
      .replace(/!/g, "_BANG_")
      .replace(/\./g, "_DOT_")
      .replace(/\|/g, "_PIPE_")
      .replace(/\*/g, "_STAR_")
      .replace(/\+/g, "_PLUS_")
      .replace(/\?/g, "_QMARK_")
      .replace(/\//g, "_FSLASH_")
      .replace(/\\/g, "_BSLASH_");
  }
}
