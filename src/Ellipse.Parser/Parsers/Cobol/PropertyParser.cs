using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    public class PropertyParser : SingleLineParser
    {
        public PropertyParser()
            : base("Property",
                   Line.Multiple(
                       Line.StartsWith(Prefix.Prefix03),
                       Line.Optional(
                           Line.Repeat(Line.StartsWith(Prefix.Empty))
                           )
                       ),
                   Data.OnLine(0,
                               Data.IgnoreStart(Prefix.Prefix03).IgnoreAfter(".").RemoveSpaces().Trim())
                       .IgnoreAll(),
                   Comment.IgnoreBefore(".").RemoveSpaces().Trim()
                )
        {
        }
    }
}