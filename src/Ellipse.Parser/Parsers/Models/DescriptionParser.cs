using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Models
{
    public class DescriptionParser : SingleLineParser
    {
        private const string prefix = "Description     :";
        private const string name = "Description";

        public DescriptionParser() : base(name, Line.StartsWith(prefix), Data.IgnoreStart(prefix).Trim())
        {
        }
    }
}