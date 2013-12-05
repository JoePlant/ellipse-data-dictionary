using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Models
{
    public class RecordParser : SingleLineParser
    {
        private const string prefix = "Record          :";
        private const string name = "Record";
           
        public RecordParser() : base(name, Line.StartsWith(prefix), Data.IgnoreStart(prefix).Trim())
        {
        }
    }
}