
namespace Ellipse.DataDictionary.Parsers.Lines
{
    public interface ILineParser
    {
        ILineParser IgnoreStart(string start);
        ILineParser IgnoreEnd(string end);
        ILineParserWithMarker IgnoreAfter(string marker);
        ILineParserWithMarker IgnoreBefore(string marker);
        ILineParser Trim();
        ILineParser RemoveSpaces();
        ILineParser IgnoreAll();
        ILineParser TruncateAt(int columnNo);

        string Parse(int lineNo, string line);

    }
}