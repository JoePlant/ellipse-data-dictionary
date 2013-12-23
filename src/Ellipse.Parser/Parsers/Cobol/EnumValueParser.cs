using System.Collections.Generic;
using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    public class EnumValueParser : CombinationParser
    {
        private static readonly Dictionary<int, string> LevelDictionary = new Dictionary<int, string>
            {
                {03, Prefix.Level0588},
                {04, Prefix.Level0788},
                {05, Prefix.Level0988},
                {06, Prefix.Level1188},
                {07, Prefix.Level1388},
                {08, Prefix.Level1588},
            };

        private class LevelParser : SingleLineParser
        {
            public LevelParser(int level)
                : base("EnumValue",
                       Line.Multiple(
                           Line.StartsWithMarker(Prefix.Marker(88)),
                           Line.Optional(
                               Line.Repeat(Line.StartsWith(Prefix.Empty))
                               )
                           ),
                       Data.OnLine(0,
                                   Data
                                       .TruncateAtColumn(60)
                                       .IgnoreBefore(Prefix.Marker(88))
                                       .IgnoreAfter(".")
                                       .RemoveSpaces()
                                       .Trim())
                           .TruncateAt(60)
                           .IgnoreAfter(".")
                           .Trim(),
                       Comment.IgnoreBefore("."))
            {
            }
        }


        public EnumValueParser()
            : base(
                new LevelParser(3),
                new LevelParser(4),
                new LevelParser(5),
                new LevelParser(6),
                new LevelParser(7),
                new LevelParser(8)
                )
        {
        }

        public static IModelParser HierarchyParser(int level)
        {
            if (LevelDictionary.ContainsKey(level))
            {
                return new LevelParser(level);
            }
            return new EmptyParser();
        }

        private static IModelParser SimpleLineParser(int level)
        {
            if (LevelDictionary.ContainsKey(level))
            {
                return new LevelParser(level);
            }
            return new EmptyParser();
        }
    }
}