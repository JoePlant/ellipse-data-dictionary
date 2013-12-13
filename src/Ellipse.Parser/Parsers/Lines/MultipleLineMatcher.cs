using Ellipse.DataDictionary.Readers;

namespace Ellipse.DataDictionary.Parsers.Lines
{
    public class MultipleLineMatcher : ILineMatcher
    {
        private readonly ILineMatcher[] lineMatchers;

        public MultipleLineMatcher(params ILineMatcher[] lineMatchers)
        {
            this.lineMatchers = lineMatchers;
        }

        public bool Matches(IReader reader, int offset, out int linesRead)
        {
            foreach (ILineMatcher lineMatcher in lineMatchers)
            {
                int currentLinesRead;
                if (!lineMatcher.Matches(reader, offset, out currentLinesRead))
                {
                    linesRead = 0;
                    return false;
                }
                offset += currentLinesRead;
            }
            linesRead = offset;
            return linesRead > 0;
        }
    }
}