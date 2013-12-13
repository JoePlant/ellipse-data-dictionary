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

        public static ILineMatcher IsNotEmpty()
        {
            return new LineMatcher.LineIsNotEmpty();
        }

        public static ILineMatcher Optional(ILineMatcher lineMatcher)
        {
            return new OptionalLineMatcher(lineMatcher);
        }

        public static ILineMatcher Or(ILineMatcher primary, ILineMatcher alternate)
        {
            return new OrLineMatcher(primary, alternate);
        }

        public static ILineMatcher FollowedBy(ILineMatcher line, ILineMatcher followedBy)
        {
            return new FollowedByMatcher(line, followedBy);
        }

        public static ILineMatcher Multiple(params ILineMatcher[] lineMatchers)
        {
            return new MultipleLineMatcher(lineMatchers);
        }
    }
}