using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    public class EnumValueParser : CombinationParser
    {
        private class Level03Parser : SingleLineParser
        {
            public Level03Parser()
                : base("EnumValue",
                       Line.Multiple(
                           Line.StartsWith(Prefix.Level0388),
                           Line.Optional(
                               Line.Repeat(Line.StartsWith(Prefix.Empty))
                               )
                           ),
                       Data.OnLine(0,
                                   Data.IgnoreStart(Prefix.Level0388)
                                       .IgnoreAfter(".")
                                       .RemoveSpaces()
                                       .Trim())
                           .IgnoreAll(),
                       Comment.IgnoreBefore("."))
            {
            }
        }

        private class Level07Parser : SingleLineParser
        {
            public Level07Parser()
                : base("EnumValue",
                      Line.Multiple(
                           Line.StartsWith(Prefix.Level0788),
                           Line.Optional(
                               Line.Repeat(Line.StartsWith(Prefix.Empty))
                               )
                           ),
                       Data.OnLine(0,
                                   Data.IgnoreStart(Prefix.Level0788)
                                       .IgnoreAfter(".")
                                       .RemoveSpaces()
                                       .Trim())
                           .IgnoreAll(),
                       Comment.IgnoreBefore("."))
            {
            }
        }

        public EnumValueParser()
            : base(new Level03Parser(), new Level07Parser())
        {
        }
    }
}