using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    public class EnumParser : SingleLineParser
    {
        private const string marker = ".";

        public EnumParser()
            : base("Enum",
                   Line.FollowedBy(Line.StartsWith(Prefix.Prefix05),
                                   Line.StartsWith(Prefix.Level0788)),
                   Data.IgnoreStart(Prefix.Prefix05).IgnoreAfter(marker))
        {
        }
    }
}