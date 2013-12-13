using Ellipse.DataDictionary.Readers;

namespace Ellipse.DataDictionary.Parsers.Lines
{
    public class FollowedByMatcher : ILineMatcher
    {
        private readonly ILineMatcher line;
        private readonly ILineMatcher followedBy;

        public FollowedByMatcher(ILineMatcher line, ILineMatcher followedBy)
        {
            this.line = line;
            this.followedBy = followedBy;
        }

        public bool Matches(IReader reader, int offset, out int linesRead)
        {
            int lines;
            if (line.Matches(reader, offset, out lines))
            {
                int followLines;
                if (followedBy.Matches(reader, offset+lines, out followLines))
                {
                    linesRead = lines;
                    return true;
                }
            }

            linesRead = 0;
            return false;
        }
    }
}