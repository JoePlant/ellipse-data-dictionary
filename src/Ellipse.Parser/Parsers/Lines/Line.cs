namespace Ellipse.DataDictionary.Parsers.Lines
{
    public class Line
    {
        public static ILineMatcher StartsWith(string startsWith)
        {
            return new LineMatcher.LineStartsWith(startsWith);
        }

        public static ILineMatcher Contains(string marker)
        {
            return new LineMatcher.LineContains(marker);
        }

        public static ILineMatcher IsEmpty()
        {
            return new LineMatcher.LineIsEmpty();
        }

        public static ILineMatcher Any()
        {
            return new LineMatcher.AnyLine();
        }

        public static IRepeatLineMatcher Repeat(ILineMatcher lineMatcher)
        {
            return new RepeatLineMatcher(lineMatcher);
        }

        public static ILineMatcher IsEqual(string text)
        {
            return new LineMatcher.LineIsEqual(text);
        }
    }
}