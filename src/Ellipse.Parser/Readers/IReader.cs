namespace Ellipse.DataDictionary.Readers
{
    public interface IReader
    {
        string PeekAhead(int plusLines);
        string PeekNext();

        string[] ReadLines(int numLines);
        string ReadLine();
        
        int LineNumber { get; }
        bool EndOfFile { get; }
        
        bool IsEndOfFile(int offset);
    }
}