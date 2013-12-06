namespace Ellipse.DataDictionary.Readers
{
    public class TrimReader : ModifyReader
    {
        public TrimReader(IReader reader) : base(reader, s => s.Trim())
        {
        }

        public override string ToString()
        {
            return "[Trim()] " + Reader;
        }
    }
}