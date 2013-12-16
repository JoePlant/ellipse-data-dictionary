using System;
using Ellipse.DataDictionary.Readers;

namespace Ellipse.DataDictionary.Parsers.Lines
{
    public class AndLineMatcher : ILineMatcher
    {
        private readonly ILineMatcher primary;
        private readonly ILineMatcher secondary;

        public AndLineMatcher(ILineMatcher primary, ILineMatcher secondary)
        {
            this.primary = primary;
            this.secondary = secondary;
        }

        public bool Matches(IReader reader, int offset, out int linesRead)
        {
            int primarylines;
            int secondarylines;
            if (primary.Matches(reader, offset, out primarylines) &&
                secondary.Matches(reader, offset, out secondarylines))
            {
                linesRead = Math.Max(primarylines, secondarylines);
                return true;
            }
            linesRead = 0;
            return false;
        }
    }
}