namespace Ellipse.DataDictionary.Parsers.Cobol
{
    public class CobolParser : CombinationParser
    {
        public CobolParser()
            : base(
                new ClassParser(),
                new PropertyParser(),
                new DataTypeParser(),
                new EnumValueParser()
                )
        {
        }
    }
}