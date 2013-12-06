using Ellipse.DataDictionary.Readers;

namespace Ellipse.DataDictionary.Parsers.Lines
{
    public class OptionalLineMatcher : ILineMatcher
    {
        private readonly ILineMatcher lineMatcher;

        public OptionalLineMatcher(ILineMatcher lineMatcher)
        {
            this.lineMatcher = lineMatcher;
        }

        public bool Matches(IReader reader, int offset, out int linesRead)
        {
            int optionalLines;
            if (lineMatcher.Matches(reader, offset, out optionalLines))
            {
                linesRead = optionalLines;
                return true;
            }
            linesRead = 0;
            return true;
        }

    }
}