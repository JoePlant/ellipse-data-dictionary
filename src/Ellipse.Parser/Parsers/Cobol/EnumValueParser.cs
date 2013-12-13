using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    public class EnumValueParser : SingleLineParser
    {
        private const string marker = ".";

        public EnumValueParser()
            : base("EnumValue",
                   Line.StartsWith(Prefix.Prefix88),
                   Data.IgnoreStart(Prefix.Prefix88).IgnoreAfter(marker))
        {
        }
    }
}