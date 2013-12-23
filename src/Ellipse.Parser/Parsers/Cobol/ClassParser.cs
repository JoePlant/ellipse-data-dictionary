using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    public class ClassParser : SingleLineParser
    {
        public ClassParser()
            : base("Class",
                   Line
                       .StartsWithMarker(Prefix.Marker(1)),
                   Data
                       .IgnoreBefore(Prefix.Marker(1))
                       .IgnoreAfter(".")
                       .Trim(),
                   Comment
                       .IgnoreBefore(".")
                )
        {
        }
    }
}