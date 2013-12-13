
namespace Ellipse.DataDictionary.Parsers.Lines
{
    public interface ILineParser
    {
        ILineParser IgnoreStart(string start);
        ILineParser IgnoreEnd(string end);
        ILineParser IgnoreAfter(string marker);
        ILineParser Trim();
        ILineParser RemoveSpaces();
        ILineParser IgnoreAll();

        string Parse(int lineNo, string line);

    }
}