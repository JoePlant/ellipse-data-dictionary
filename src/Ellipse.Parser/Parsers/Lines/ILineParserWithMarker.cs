namespace Ellipse.DataDictionary.Parsers.Lines
{
    public interface ILineParserWithMarker
    {
        ILineParser IncludeMarker();

        ILineParser ExcludeMarker();
        
    }
}