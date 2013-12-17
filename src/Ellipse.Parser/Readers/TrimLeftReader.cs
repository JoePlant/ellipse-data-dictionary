namespace Ellipse.DataDictionary.Readers
{
    public class TrimLeftReader : ModifyReader
    {
        private readonly int numChars;

        public TrimLeftReader(IReader reader, int numChars)
            : base(reader, s => s.Length > numChars ? s.Substring(numChars) : s)
        {
            this.numChars = numChars;
        }

        public override string ToString()
        {
            return "[TrimLeft(" + numChars +")] " + Reader;
        }
    }
}