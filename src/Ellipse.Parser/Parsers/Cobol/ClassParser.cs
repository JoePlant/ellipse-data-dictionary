using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    public class ClassParser : SingleLineParser
    {
        public ClassParser()
            : base("Class", 
            Line.StartsWith(Prefix.Prefix01), 
            Data.IgnoreStart(Prefix.Prefix01).IgnoreAfter("."),
            Comment.IgnoreBefore("."))
        {
        }
    }
}