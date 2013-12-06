using Ellipse.DataDictionary.Readers;

namespace Ellipse.DataDictionary.Parsers.Lines
{
    public class OrLineMatcher : ILineMatcher
    {
        private readonly ILineMatcher primary;
        private readonly ILineMatcher alternate;

        public OrLineMatcher(ILineMatcher primary, ILineMatcher alternate)
        {
            this.primary = primary;
            this.alternate = alternate;
        }

        public bool Matches(IReader reader, int offset, out int linesRead)
        {
            int lines;
            if (primary.Matches(reader, offset, out lines))
            {
                linesRead = lines;
                return true;
            }

            if (alternate.Matches(reader, offset, out lines))
            {
                linesRead = lines;
                return true;
            }

            linesRead = 0;
            return false;
        }
    }
}