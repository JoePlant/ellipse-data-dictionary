namespace Ellipse.DataDictionary.Parsers.Lines
{
    public interface ILineSplitter
    {
        ILineSplitter Find(string marker);
        ILineSplitter Ignore(params int[] offsets);
        ILineSplitter AndFollowing();
        ILineSplitter Select(params int[] offsets);
        ILineParser Join(string marker);
    }
}