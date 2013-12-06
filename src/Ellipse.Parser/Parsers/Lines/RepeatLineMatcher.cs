using Ellipse.DataDictionary.Readers;

namespace Ellipse.DataDictionary.Parsers.Lines
{
    public class RepeatLineMatcher : IRepeatLineMatcher
    {
        private readonly ILineMatcher lineMatcher;
        private ILineMatcher until;

        public RepeatLineMatcher(ILineMatcher lineMatcher)
        {
            this.lineMatcher = lineMatcher;
            until = null;
        }

        public bool Matches(IReader reader, int offset, out int linesRead)
        {
            int totalLinesRead = 0;
            int currentLinesRead;
            int currentOffset = offset;

            bool nextLine = true;
            while (nextLine & (until == null || !until.Matches(reader, currentOffset, out currentLinesRead)))
            {
                nextLine = lineMatcher.Matches(reader, currentOffset, out currentLinesRead);
                if (nextLine)
                {
                    totalLinesRead += currentLinesRead;
                    currentOffset += currentLinesRead;
                }
            }
            
            linesRead = totalLinesRead;
            return linesRead > 0;
        }

        public IRepeatLineMatcher Until(ILineMatcher untilMatcher)
        {
            until = untilMatcher;
            return this;
        }

    }
}