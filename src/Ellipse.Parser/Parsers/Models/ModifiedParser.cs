using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Models
{
    public class ModifiedParser : SingleLineParser
    {
        private const string prefix = "Modified        :";
        private const string name = "Modified";

        public ModifiedParser()
            : base(name, Line.StartsWith(prefix), Data.IgnoreStart(prefix).Trim())
        {
        }
    }
}