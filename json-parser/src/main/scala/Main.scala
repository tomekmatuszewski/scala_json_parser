

@main def run(args: String*): Unit =
  JsonParser.parse(args.mkString(" "))

