namespace Ellipse.DataDictionary.Parsers.Lines
{
    public interface IRepeatLineMatcher : ILineMatcher
    {
        IRepeatLineMatcher Until(ILineMatcher lineMatcher);
    }
} 