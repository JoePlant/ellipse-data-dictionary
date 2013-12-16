using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    [TestFixture]
    public class ClassParserUnitTests : ParserTestFixture<ClassParser>
    {
        [Test]
        public void SingleLine()
        {
            StringReader reader = new StringReader(ExampleStrings.Class.Case1);
            
            IDataParser parser = CreateDataParser(reader);

            AssertParsed(parser, new StringModel("Class", "MSF001-RECORD"));
        }

        [Test]
        public void PropertyCases()
        {
            AssertDoesNotParse(ExampleStrings.Property.SimpleCases());
        }
    }
}