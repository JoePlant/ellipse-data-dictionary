
namespace Ellipse.DataDictionary.Parsers.Lines
{
    public interface ILineParser
    {
        ILineParser IgnoreStart(string start);
        ILineParser IgnoreEnd(string end);
        ILineParser IgnoreAfter(string marker);
        ILineParser IgnoreBefore(string marker);
        ILineParser Trim();
        ILineParser RemoveSpaces();
        ILineParser IgnoreAll();
        ILineParser TruncateAt(int columnNo);

        string Parse(int lineNo, string line);

    }
}