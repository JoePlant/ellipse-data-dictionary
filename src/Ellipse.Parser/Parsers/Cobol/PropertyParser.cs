using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    public class PropertyParser : CombinationParser
    {
        private class Line03Parser : SingleLineParser
        {
            public Line03Parser()
                : base("Property", Line.Multiple(
                    Line.And(Line.StartsWith(Prefix.Prefix03), Line.DoesNotContain("PIC")),
                    Line.Optional(
                        Line.Repeat(Line.StartsWith(Prefix.Empty))
                        )
                                       ),
                       Data.OnLine(0,
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
                : base("Property",
                       Line.Multiple(
                           Line.And(Line.StartsWith(Prefix.Prefix05), Line.DoesNotContain("PIC")),
                           Line.Optional(
                               Line.Repeat(Line.StartsWith(Prefix.Empty))
                               )
                           ),
                       Data.OnLine(0,
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
                : base("Property",
                       Line.Multiple(
                           Line.And(Line.StartsWith(Prefix.Prefix07), Line.DoesNotContain("PIC")),
                           Line.Optional(
                               Line.Repeat(Line.StartsWith(Prefix.Empty))
                               )
                           ),
                       Data.OnLine(0,
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
                : base("Property",
                       Line.Multiple(
                           Line.And(Line.StartsWith(Prefix.Prefix09), Line.DoesNotContain("PIC")),
                           Line.Optional(
                               Line.Repeat(Line.StartsWith(Prefix.Empty))
                               )
                           ),
                       Data.OnLine(0,
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
                : base("Property",
                       Line.Multiple(
                           Line.And(Line.StartsWith(Prefix.Prefix11), Line.DoesNotContain("PIC")),
                           Line.Optional(
                               Line.Repeat(Line.StartsWith(Prefix.Empty))
                               )
                           ),
                       Data.OnLine(0,
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
                : base("Property",
                       Line.Multiple(
                           Line.And(Line.StartsWith(Prefix.Prefix13), Line.DoesNotContain("PIC")),
                           Line.Optional(
                               Line.Repeat(Line.StartsWith(Prefix.Empty))
                               )
                           ),
                       Data.OnLine(0,
                                   Data.IgnoreStart(Prefix.Prefix13)
                                       .IgnoreAfter(".")
                                       .RemoveSpaces()
                                       .Trim())
                           .IgnoreAll(),
                       Comment.IgnoreBefore(".").RemoveSpaces().Trim())
            {
            }
        }

        public PropertyParser()
            : base(
                new Line03Parser(),
                new Line05Parser(),
                new Line07Parser(),
                new Line09Parser(),
                new Line11Parser(),
                new Line13Parser()
                )
        {
        }
    }
}