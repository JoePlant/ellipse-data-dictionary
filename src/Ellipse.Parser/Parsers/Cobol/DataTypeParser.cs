using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    public class DataTypeParser : CombinationParser
    {
        private class Line03Parser : SingleLineParser
        {
            public Line03Parser()
                : base("DataType",
                       Line.Multiple(
                           Line.And(Line.StartsWith(Prefix.Prefix03), Line.Contains("PIC")),
                           Line.Optional(
                               Line.Repeat(Line.StartsWith(Prefix.Empty))
                               )
                           ), Data.OnLine(0,
                                          Data.IgnoreStart(Prefix.Prefix03)
                                              .IgnoreAfter(".")
                                              .RemoveSpaces()
                                              .Trim())
                                  .IgnoreAll(),
                       Comment.IgnoreBefore(".").RemoveSpaces().Trim())
            {
            }
        }

        private class Line05Parser : SingleLineParser
        {
            public Line05Parser()
                : base("DataType",
                       Line.Multiple(
                           Line.And(Line.StartsWith(Prefix.Prefix05), Line.Contains("PIC")),
                           Line.Optional(
                               Line.Repeat(Line.StartsWith(Prefix.Empty))
                               )
                           ), Data.OnLine(0,
                                          Data.IgnoreStart(Prefix.Prefix05)
                                              .IgnoreAfter(".")
                                              .RemoveSpaces()
                                              .Trim())
                                  .IgnoreAll(),
                       Comment.IgnoreBefore(".").RemoveSpaces().Trim())
            {
            }
        }

        private class Line07Parser : SingleLineParser
        {
            public Line07Parser()
                : base("DataType",
                       Line.Multiple(
                           Line.And(Line.StartsWith(Prefix.Prefix07), Line.Contains("PIC")),
                           Line.Optional(
                               Line.Repeat(Line.StartsWith(Prefix.Empty))
                               )
                           ), Data.OnLine(0,
                                          Data.IgnoreStart(Prefix.Prefix07)
                                              .IgnoreAfter(".")
                                              .RemoveSpaces()
                                              .Trim())
                                  .IgnoreAll(),
                       Comment.IgnoreBefore(".").RemoveSpaces().Trim())
            {
            }
        }

        private class Line09Parser : SingleLineParser
        {
            public Line09Parser()
                : base("DataType",
                       Line.Multiple(
                           Line.And(Line.StartsWith(Prefix.Prefix09), Line.Contains("PIC")),
                           Line.Optional(
                               Line.Repeat(Line.StartsWith(Prefix.Empty))
                               )
                           ), Data.OnLine(0,
                                          Data.IgnoreStart(Prefix.Prefix09)
                                              .IgnoreAfter(".")
                                              .RemoveSpaces()
                                              .Trim())
                                  .IgnoreAll(),
                       Comment.IgnoreBefore(".").RemoveSpaces().Trim())
            {
            }
        }

        private class Line11Parser : SingleLineParser
        {
            public Line11Parser()
                : base("DataType",
                       Line.Multiple(
                           Line.And(Line.StartsWith(Prefix.Prefix11), Line.Contains("PIC")),
                           Line.Optional(
                               Line.Repeat(Line.StartsWith(Prefix.Empty))
                               )
                           ), Data.OnLine(0,
                                          Data.IgnoreStart(Prefix.Prefix11)
                                              .IgnoreAfter(".")
                                              .RemoveSpaces()
                                              .Trim())
                                  .IgnoreAll(),
                       Comment.IgnoreBefore(".").RemoveSpaces().Trim())
            {
            }
        }

        private class Line13Parser : SingleLineParser
        {
            public Line13Parser()
                : base("DataType",
                       Line.Multiple(
                           Line.And(Line.StartsWith(Prefix.Prefix13), Line.Contains("PIC")),
                           Line.Optional(
                               Line.Repeat(Line.StartsWith(Prefix.Empty))
                               )
                           ), Data.OnLine(0,
                                          Data.IgnoreStart(Prefix.Prefix13)
                                              .IgnoreAfter(".")
                                              .RemoveSpaces()
                                              .Trim())
                                  .IgnoreAll(),
                       Comment.IgnoreBefore(".").RemoveSpaces().Trim())
            {
            }
        }

        private class Line15Parser : SingleLineParser
        {
            public Line15Parser()
                : base("DataType",
                       Line.Multiple(
                           Line.And(Line.StartsWith(Prefix.Prefix15), Line.Contains("PIC")),
                           Line.Optional(
                               Line.Repeat(Line.StartsWith(Prefix.Empty))
                               )
                           ), Data.OnLine(0,
                                          Data.IgnoreStart(Prefix.Prefix15)
                                              .IgnoreAfter(".")
                                              .RemoveSpaces()
                                              .Trim())
                                  .IgnoreAll(),
                       Comment.IgnoreBefore(".").RemoveSpaces().Trim())
            {
            }
        }

        public DataTypeParser()
            : base(
                new Line03Parser(),
                new Line05Parser(),
                new Line07Parser(),
                new Line09Parser(),
                new Line11Parser(),
                new Line13Parser(),
                new Line15Parser()
                )
        {
        }
    }
}