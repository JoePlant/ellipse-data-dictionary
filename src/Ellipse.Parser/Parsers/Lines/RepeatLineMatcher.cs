using Ellipse.DataDictionary.Readers;

namespace Ellipse.DataDictionary.Parsers.Lines
{
    public class RepeatLineMatcher : IRepeatLineMatcher
    {
        private readonly ILineMatcher lineMatcher;
        private ILineMatcher until;
        private int count = int.MaxValue;
        private int atLeast = 0;

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
            int repeatCount = 0;
            while (nextLine & (until == null || !until.Matches(reader, currentOffset, out currentLinesRead)))
            {
                nextLine = lineMatcher.Matches(reader, currentOffset, out currentLinesRead);
                if (nextLine)
                {
                    totalLinesRead += currentLinesRead;
                    currentOffset += currentLinesRead;
                    repeatCount++;
                }
            }
            
            linesRead = totalLinesRead;
            return linesRead > atLeast;
        }

        public IRepeatLineMatcher Until(ILineMatcher untilMatcher)
        {
            until = untilMatcher;
            return this;
        }

        public IRepeatLineMatcher Count(int lines)
        {
            count = lines;
            return this;
        }

        public IRepeatLineMatcher AtLeast(int lines)
        {
            atLeast = lines;
            return this;
        }
    }
}