using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    public class DataTypeParser : SingleLineParser
    {
        public DataTypeParser()
            : base("DataType",
                       Line.Multiple(
                       Line.StartsWith(Prefix.Prefix05),
                       Line.Optional(
                           Line.Repeat(Line.StartsWith(Prefix.Empty))
                           )
                       ),
                   Data.OnLine(0,
                               Data.IgnoreStart(Prefix.Prefix05).IgnoreAfter(".").RemoveSpaces().Trim())
                       .IgnoreAll(),
                   Comment.IgnoreBefore(".").RemoveSpaces().Trim())
        {
        }
    }
}