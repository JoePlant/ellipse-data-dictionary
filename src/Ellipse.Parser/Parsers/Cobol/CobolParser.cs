namespace Ellipse.DataDictionary.Parsers.Cobol
{
    public class CobolParser : CombinationParser
    {
        public CobolParser()
            : base(
                new ClassParser(),
                new PropertyParser(),
                new DataTypeParser(),
                new RedefinesParser(),
                new EnumValueParser(),
                new OccursParser()
                )
        {
        }

        public static IModelParser CobolHierarchy()
        {
            return new HierarchyParser(
                new ClassParser(),
                PropertyParser.HierarchyParser(2),
                DataTypeParser.HierarchyParser(2),
                RedefinesParser.HierarchyParser(2),
                OccursParser.HierarchyParser(2));
        }
    }
}