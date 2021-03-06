﻿namespace Ellipse.DataDictionary.Parsers.Lines
{
    public class Comment
    {
        public static ILineParser IgnoreStart(string start)
        {
            return new LineParser().IgnoreStart(start);
        }

        public static ILineParser IgnoreEnd(string end)
        {
            return new LineParser().IgnoreEnd(end);
        }

        public static ILineParserWithMarker IgnoreAfter(string marker)
        {
            return new LineParser().IgnoreAfter(marker);
        }

        public static ILineParser Trim()
        {
            return new LineParser().Trim();
        }

        public static ILineParser RemoveSpaces()
        {
            return new LineParser().RemoveSpaces();
        }

        public static ILineParserWithMarker IgnoreBefore(string marker)
        {
            return new LineParser().IgnoreBefore(marker);
        }

        public static ILineParser IgnoreBefore(int columnNo)
        {
            return new LineParser().IgnoreBefore(columnNo);
        }

        public static ILineParser IgnoreAll()
        {
            return new LineParser().IgnoreAll();
        }
    }
}