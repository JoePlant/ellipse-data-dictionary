namespace Ellipse.DataDictionary.Parsers.Lines
{
    public class Line
    {
        public static ILineMatcher StartsWithMarker(string startsWith)
        {
            return new LineMatcher.LineStartsWith(startsWith)
                .IgnoreLeadingSpaces();
        }

        public static ILineMatcher StartsWith(string startsWith)
        {
            return new LineMatcher.LineStartsWith(startsWith);
        }

        public static ILineMatcher Contains(string marker)
        {
            return new LineMatcher.LineContains(marker);
        }

        public static ILineMatcher DoesNotContain(string marker)
        {
            return new LineMatcher.LineContains(marker).Invert();
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

        public static ILineMatcher Or(ILineMatcher option1, ILineMatcher option2)
        {
            return new OrLineMatcher(new [] {option1, option2});
        }

        public static ILineMatcher Or(ILineMatcher option1, ILineMatcher option2, ILineMatcher option3)
        {
            return new OrLineMatcher(new[] { option1, option2, option3});
        }
        
        public static ILineMatcher And(ILineMatcher primary, ILineMatcher alternate)
        {
            return new AndLineMatcher(primary, alternate);
        }

        public static ILineMatcher And(ILineMatcher one, ILineMatcher two, ILineMatcher three)
        {
            return new AndLineMatcher(new AndLineMatcher(one, two), three);
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