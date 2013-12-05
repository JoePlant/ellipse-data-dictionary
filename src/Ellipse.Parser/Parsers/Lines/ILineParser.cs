
namespace Ellipse.DataDictionary.Parsers.Lines
{
    public interface ILineParser
    {
        ILineParser IgnoreStart(string start);
        ILineParser IgnoreEnd(string end);
        ILineParser Trim();

        string Parse(string line);

    }
}