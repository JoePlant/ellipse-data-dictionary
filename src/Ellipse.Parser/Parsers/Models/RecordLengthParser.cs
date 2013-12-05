using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Models
{
    public class RecordLengthParser : SingleLineParser
    {
        private const string prefix = "Record length   :";
        private const string suffix = "bytes";
        private const string name = "RecordLength";

        public RecordLengthParser()
            : base(name, Line.StartsWith(prefix), Data.IgnoreStart(prefix).IgnoreEnd(suffix).Trim())
        {
        }
    }
}