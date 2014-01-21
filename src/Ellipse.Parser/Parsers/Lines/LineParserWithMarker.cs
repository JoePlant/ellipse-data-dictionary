using System;

namespace Ellipse.DataDictionary.Parsers.Lines
{
    public class LineParserWithMarker : ILineParserWithMarker
    {
        private readonly string marker;
        private bool includeMarker;

        public LineParserWithMarker(LineParser parentParser, string marker)
        {
            this.marker = marker;
            Parent = parentParser;
            includeMarker = false;
        }

        LineParser Parent { get; set; }

        public ILineParser IncludeMarker()
        {
            includeMarker = true;
            return Parent;
        }

        public ILineParser ExcludeMarker()
        {
            includeMarker = false;
            return Parent;
        }

        public string IgnoreAfter(int lineNo, string line)
        {
            int offset = includeMarker ? marker.Length : 0;

            return line != null && line.Contains(marker)
                       ? line.Substring(0, line.IndexOf(marker, StringComparison.InvariantCulture) + offset)
                       : line;
        }

        public string IgnoreBefore(int lineNo, string line)
        {
            int offset = includeMarker ? 0 : marker.Length;

            return line != null && line.Contains(marker)
                       ? line.Substring(line.IndexOf(marker, StringComparison.InvariantCulture) + offset)
                       : line;
        }
    }
}