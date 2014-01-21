using System.Collections.Generic;
using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    public class DataTypeParser : CombinationParser
    {
        private static readonly Dictionary<int, string> LevelDictionary = new Dictionary<int, string>
            {
                //{01, Prefix.Prefix01},
                {02, Prefix.Prefix03},
                {03, Prefix.Prefix05},
                {04, Prefix.Prefix07},
                {05, Prefix.Prefix09},
                {06, Prefix.Prefix11},
                {07, Prefix.Prefix13},
                {08, Prefix.Prefix15},
                {09, Prefix.Prefix17},
                {10, Prefix.Prefix19},
                {11, Prefix.Prefix21},
                {12, Prefix.Prefix23},
                {13, Prefix.Prefix25},
                {14, Prefix.Prefix27},
                {15, Prefix.Prefix29},
                {16, Prefix.Prefix31},
            };

        private static readonly Dictionary<int, HierarchyParser> ParserDictionary = new Dictionary<int, HierarchyParser>();

        private class LevelParser : SingleLineParser
        {
            public LevelParser(int level)
                : base("DataType",
                       Line.Multiple(
                           Line.And(
                               Line.StartsWithMarker(Prefix.Marker(level)),
                               Line.Contains("PIC ")
                               ),
                           Line.Optional(
                               Line.Repeat(Line.StartsWith(Prefix.Empty))
                               )
                           ),
                       Data.OnLine(0,
                                   Data
                                       .TruncateAtColumn(59)
                                       .IgnoreBefore(Prefix.Marker(level)).ExcludeMarker()
                                       .IgnoreAfter(".").ExcludeMarker()
                                       .RemoveSpaces()
                                       .Trim())
                           .TruncateAt(59)
                           .IgnoreAfter(".").ExcludeMarker()
                           .Trim(),
                       Comment
                           .IgnoreBefore(59)
                           .RemoveSpaces()
                           .Trim())
            {
            }
        }

        private class ImpliedDataTypeParser : ImpliedModelParser
        {
            public ImpliedDataTypeParser()
                : base("DataType",
                       Line.Contains("PIC "),
                       Data.SplitOn(" ").Find("PIC").Ignore(0, +1).Join(" "),
                       Data.SplitOn(" ").Find("PIC").Select(0, +1).Join(" "))
            {
            }
        }

        public DataTypeParser()
            : base(
                SimpleLevelParser(2),
                SimpleLevelParser(3),
                SimpleLevelParser(4),
                SimpleLevelParser(5),
                SimpleLevelParser(6),
                SimpleLevelParser(7),
                SimpleLevelParser(8),
                SimpleLevelParser(9),
                SimpleLevelParser(10),
                SimpleLevelParser(11),
                SimpleLevelParser(12),
                SimpleLevelParser(13),
                SimpleLevelParser(14),
                SimpleLevelParser(15),
                SimpleLevelParser(16)
                )
        {
        }

        public static IModelParser HierarchyParser(int level)
        {
            if (LevelDictionary.ContainsKey(level))
            {
                HierarchyParser hierarchyParser;
                if (!ParserDictionary.TryGetValue(level, out hierarchyParser))
                {
                    hierarchyParser = new HierarchyParser(
                        SimpleLevelParser(level),
                        new[]
                            {
                                EnumValueParser.HierarchyParser(level + 1)
                            }
                        );
                    ParserDictionary.Add(level, hierarchyParser);
                }
                return hierarchyParser;
            }
            return new EmptyParser();
        }
        
        private static IModelParser SimpleLevelParser(int level)
        {
            if (LevelDictionary.ContainsKey(level))
            {
                return new LevelParser(level);
            }
            return new EmptyParser();
        }

        public static IImpliedModelParser ImpliedParser()
        {
            return new ImpliedDataTypeParser();
        }
    }
}