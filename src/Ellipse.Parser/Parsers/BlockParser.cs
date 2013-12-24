using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Parsers.Lines;
using Ellipse.DataDictionary.Readers;

namespace Ellipse.DataDictionary.Parsers
{
    public abstract class BlockParser : IModelParser
    {
        private readonly ILineMatcher[] lineMatchers;

        protected BlockParser(ILineMatcher[] lineMatchers)
        {
            this.lineMatchers = lineMatchers;
        }
        
        public bool Matches(IReader reader)
        {
            int linesRead;
            return MatchLines(reader, out linesRead);
        }

        private bool MatchLines(IReader reader, out int linesRead)
        {
            int offset = 0;
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

        protected abstract IModel CreateModel(string[] lines);

        public IModel Parse(IReader reader)
        {
            int linesRead;
            if (MatchLines(reader, out linesRead))
            {
                string[] lines = reader.ReadLines(linesRead);
                return CreateModel(lines);
            }
            return null;
        }
    }
}