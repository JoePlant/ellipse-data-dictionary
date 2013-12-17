using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    public class PropertyParser : CombinationParser
    {
        private class LineParser : SingleLineParser
        {
            public LineParser(string prefix)
                : base("Property", Line.Multiple(
                    Line.And(Line.StartsWith(prefix), Line.DoesNotContain("PIC")),
                    Line.Optional(
                        Line.Repeat(Line.StartsWith(Prefix.Empty))
                        )
                                       ),
                       Data.OnLine(0,
                                   Data
                                       .TruncateAtColumn(60)
                                       .IgnoreStart(prefix)
                                       .IgnoreAfter(".")
                                       .RemoveSpaces()
                                       .Trim())
                           .TruncateAt(60)
                           .IgnoreAfter(".")
                           .RemoveSpaces()
                           .Trim(),
                       Comment
                           .IgnoreBefore(".")
                           .RemoveSpaces()
                           .Trim()
                    )
            {
            }
        }

        public PropertyParser()
            : base(
                new LineParser(Prefix.Prefix03),
                new LineParser(Prefix.Prefix05),
                new LineParser(Prefix.Prefix07),
                new LineParser(Prefix.Prefix09),
                new LineParser(Prefix.Prefix11),
                new LineParser(Prefix.Prefix13),
                new LineParser(Prefix.Prefix15),
                new LineParser(Prefix.Prefix17),
                new LineParser(Prefix.Prefix19),
                new LineParser(Prefix.Prefix21),
                new LineParser(Prefix.Prefix23),
                new LineParser(Prefix.Prefix25),
                new LineParser(Prefix.Prefix27),
                new LineParser(Prefix.Prefix29)
                )
        {
        }
    }
}