using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    public class PropertyParser : CombinationParser
    {
        private class Line03Parser : SingleLineParser
        {
            public Line03Parser()
                : base("Property", Line.Multiple(
                    Line.And(Line.StartsWith(Prefix.Prefix03),Line.DoesNotContain("PIC")),
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
                : base("Property",
                       Line.FollowedBy(
                           Line.StartsWith(Prefix.Prefix05),
                           Line.StartsWith(Prefix.Prefix07)
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

        public PropertyParser()
            : base(new Line03Parser(), new Line05Parser())
        {
        }
    }
}