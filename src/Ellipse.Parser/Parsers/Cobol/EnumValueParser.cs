using System.Collections.Generic;
using Ellipse.DataDictionary.Models;
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

        private static readonly Dictionary<int, IModelParser> ParserDictionary = new Dictionary<int, IModelParser>();

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
                                       .IgnoreBefore(Prefix.Marker(88)).ExcludeMarker()
                                       .IgnoreAfter(".").ExcludeMarker()
                                       .RemoveSpaces()
                                       .Trim())
                           .TruncateAt(60)
                           .IgnoreAfter(".").ExcludeMarker()
                           .Trim(),
                       Comment
                           .IgnoreBefore(60)
                           .Trim(),
                       new IImpliedModelParser[]
                           {

                           },
                       CobolModel.Factory
                    )
            {
            }
        }


        public EnumValueParser()
            : base(
                SimpleLineParser(3),
                SimpleLineParser(4),
                SimpleLineParser(5),
                SimpleLineParser(6),
                SimpleLineParser(7),
                SimpleLineParser(8)
                )
        {
        }

        public static IModelParser HierarchyParser(int level)
        {
            if (LevelDictionary.ContainsKey(level))
            {
                IModelParser parser;
                if (!ParserDictionary.TryGetValue(level, out parser))
                {
                    parser = SimpleLineParser(level);
                    ParserDictionary.Add(level, parser);
                }
                return parser;
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