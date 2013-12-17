using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Models
{
    public class IgnoreParser : CombinationParser
    {
        private class LineParser : SingleLineParser
        {
            public LineParser(string line)
                : base("Ignore", Line.IsEqual(line), Data.Trim(), Comment.Trim())
            {
            }
        }

        public IgnoreParser()
            : base(new LineParser("1"))
        {

        }
    }
}