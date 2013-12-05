namespace Ellipse.DataDictionary.Parsers.Lines
{
    public class Data
    {
        public static ILineParser IgnoreStart(string start)
        {
            return new LineParser().IgnoreStart(start);
        }
    }
}