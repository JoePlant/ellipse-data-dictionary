using Ellipse.DataDictionary.Readers;

namespace Ellipse.DataDictionary.Parsers.Lines
{
    public abstract class LineMatcher : ILineMatcher
    {
        public class LineStartsWith : LineMatcher
        {
            private readonly string startsWith;

            public LineStartsWith(string startsWith)
            {
                this.startsWith = startsWith;
            }

            public override bool Matches(string line)
            {
                return line != null && line.StartsWith(startsWith);
            }
        }

        public class AnyLine : LineMatcher
        {
            public override bool Matches(string line)
            {
                return true;
            }
        }

        public class LineContains : LineMatcher
        {
            private readonly string contains;

            public LineContains(string contains)
            {
                this.contains = contains;
            }

            public override bool Matches(string line)
            {
                return line != null && line.Contains(contains);
            }
        }

        public class LineIsEmpty : LineMatcher
        {
            public override bool Matches(string line)
            {
                return string.IsNullOrEmpty(line);
            }
        }

        public class LineIsEqual : LineMatcher
        {
            private readonly string text;

            public LineIsEqual(string text)
            {
                this.text = text;
            }

            public override bool Matches(string line)
            {
                return string.Equals(line, text);
            }
        }

        public abstract bool Matches(string line);

        public bool Matches(IReader reader, int offset, out int linesRead)
        {
            linesRead = 0;
            if (!reader.IsEndOfFile(offset))
            {
                if (Matches(reader.PeekAhead(offset)))
                {
                    linesRead = 1;
                    return true;
                }
            }
            return false;
        }

        public override string ToString()
        {
            return this.GetType().Name;
        }
    }
}