package scalaz.parsers

object escapes {
  def escapeJava(s: String): String = {
    def go(i: Int, result: StringBuilder): String = if (i < s.length) {
      s(i) match {
        case '"'  => go(i + 1, result append "\\\"")
        case '\\' => go(i + 1, result append "\\\\")

        case '\b' => go(i + 1, result append "\\b")
        case '\n' => go(i + 1, result append "\\n")
        case '\t' => go(i + 1, result append "\\t")
        case '\f' => go(i + 1, result append "\\f")
        case '\r' => go(i + 1, result append "\\r")

        case x if x < 32 || x > 0x7f =>
          val chars = Integer.toString(x.toInt & 0xFFFF, 16)
          val sb =
            if (chars.length >= 4) result append "\\u" append chars
            else if (chars.length == 3) result append "\\u0" append chars
            else if (chars.length == 2) result append "\\u00" append chars
            else result append "\\u000" append chars
          go(i + 1, sb)

        case x => go(i + 1, result append x)
      }
    } else result.toString()

    go(0, new StringBuilder())
  }
}
