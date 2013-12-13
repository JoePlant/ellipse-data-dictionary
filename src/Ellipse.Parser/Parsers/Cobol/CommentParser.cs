using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    public class CommentParser : SingleLineParser
    {
        public CommentParser() : base("Comment", Line.StartsWith(Prefix.Empty), Data.Trim())
        {
        }
    }
}