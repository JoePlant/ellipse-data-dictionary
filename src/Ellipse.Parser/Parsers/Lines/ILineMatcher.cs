using Ellipse.DataDictionary.Readers;

namespace Ellipse.DataDictionary.Parsers.Lines
{
    public interface ILineMatcher
    {
        bool Matches(IReader reader, int offset, out int linesRead);
    }
}