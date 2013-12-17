namespace Ellipse.DataDictionary.Readers
{
    public class AddPrefixReader : ModifyReader
    {
        private readonly string prefix;

        public AddPrefixReader(IReader reader, string prefix)
            : base(reader, s => prefix + s)
        {
            this.prefix = prefix;
        }

        public override string ToString()
        {
            string s = string.Format("[AddPrefix('{0}')] ", prefix);
            return s + Reader;
        }
    }
}