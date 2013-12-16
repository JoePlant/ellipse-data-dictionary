using System.Linq;
using Ellipse.DataDictionary.Readers;

namespace Ellipse.DataDictionary.Parsers.Lines
{
    public class OrLineMatcher : ILineMatcher
    {
        private readonly ILineMatcher[] options;

        public OrLineMatcher(ILineMatcher[] options)
        {
            this.options = options;
        }

        public bool Matches(IReader reader, int offset, out int linesRead)
        {
            int lines = 0;
            if (options.Any(option => option.Matches(reader, offset, out lines)))
            {
                linesRead = lines;
                return true;
            }

            linesRead = 0;
            return false;
        }
    }
}