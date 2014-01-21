using System.Collections.Generic;
using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    public class PropertyParser : CombinationParser
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

        private class LineParser : SingleLineParser
        {
            public LineParser(int level)
                : base("Property",
                       Line.Multiple(
                           Line.And(Line.StartsWithMarker(Prefix.Marker(level)),
                                    Line.And(
                                        Line.DoesNotContain("PIC "),
                                        Line.DoesNotContain("REDEFINES "),
                                        Line.DoesNotContain("OCCURS")
                                        )),
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
                           .RemoveSpaces()
                           .Trim(),
                       Comment
                           .IgnoreBefore(59)
                           .RemoveSpaces()
                           .Trim()
                    )
            {
            }
        }


        public PropertyParser()
            : base(
                SimpleLineParser(2),
                SimpleLineParser(3),
                SimpleLineParser(4),
                SimpleLineParser(5),
                SimpleLineParser(6),
                SimpleLineParser(7),
                SimpleLineParser(8),
                SimpleLineParser(9),
                SimpleLineParser(10),
                SimpleLineParser(11),
                SimpleLineParser(12),
                SimpleLineParser(13),
                SimpleLineParser(14),
                SimpleLineParser(15)
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
                        SimpleLineParser(level),
                        new[]
                            {
                                PropertyParser.HierarchyParser(level + 1),
                                DataTypeParser.HierarchyParser(level + 1),
                                RedefinesParser.HierarchyParser(level + 1),
                                OccursParser.HierarchyParser(level + 1)
                            }
                        );
                    ParserDictionary.Add(level, hierarchyParser);
                }
                return hierarchyParser;
            }
            return new EmptyParser();
        }

        private static IModelParser SimpleLineParser(int level)
        {
            if (LevelDictionary.ContainsKey(level))
            {
                return new LineParser(level);
            }
            return new EmptyParser();
        }
    }
}