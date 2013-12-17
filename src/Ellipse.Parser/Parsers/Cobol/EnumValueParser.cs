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

        private class Level05Parser : SingleLineParser
        {
            public Level05Parser()
                : base("EnumValue",
                       Line.Multiple(
                           Line.StartsWith(Prefix.Level0588),
                           Line.Optional(
                               Line.Repeat(Line.StartsWith(Prefix.Empty))
                               )
                           ),
                       Data.OnLine(0,
                                   Data.IgnoreStart(Prefix.Level0588)
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

        private class Level09Parser : SingleLineParser
        {
            public Level09Parser()
                : base("EnumValue",
                       Line.Multiple(
                           Line.StartsWith(Prefix.Level0988),
                           Line.Optional(
                               Line.Repeat(Line.StartsWith(Prefix.Empty))
                               )
                           ),
                       Data.OnLine(0,
                                   Data.IgnoreStart(Prefix.Level0988)
                                       .IgnoreAfter(".")
                                       .RemoveSpaces()
                                       .Trim())
                           .IgnoreAll(),
                       Comment.IgnoreBefore("."))
            {
            }
        }

        private class Level11Parser : SingleLineParser
        {
            public Level11Parser()
                : base("EnumValue",
                       Line.Multiple(
                           Line.StartsWith(Prefix.Level1188),
                           Line.Optional(
                               Line.Repeat(Line.StartsWith(Prefix.Empty))
                               )
                           ),
                       Data.OnLine(0,
                                   Data.IgnoreStart(Prefix.Level1188)
                                       .IgnoreAfter(".")
                                       .RemoveSpaces()
                                       .Trim())
                           .IgnoreAll(),
                       Comment.IgnoreBefore("."))
            {
            }
        }

        private class Level13Parser : SingleLineParser
        {
            public Level13Parser()
                : base("EnumValue",
                       Line.Multiple(
                           Line.StartsWith(Prefix.Level1388),
                           Line.Optional(
                               Line.Repeat(Line.StartsWith(Prefix.Empty))
                               )
                           ),
                       Data.OnLine(0,
                                   Data
                                       .TruncateAtColumn(60)
                                       .IgnoreStart(Prefix.Level1388)
                                       .IgnoreAfter(".")
                                       .RemoveSpaces()
                                       .Trim())
                           .TruncateAt(60)
                           .IgnoreAfter(".")
                           .Trim(),
                       Comment.IgnoreBefore(60))
            {
            }
        }

        public EnumValueParser()
            : base(
                new Level03Parser(),
                new Level05Parser(),
                new Level07Parser(),
                new Level09Parser(),
                new Level11Parser(),
                new Level13Parser()
                )
        {
        }
    }
}