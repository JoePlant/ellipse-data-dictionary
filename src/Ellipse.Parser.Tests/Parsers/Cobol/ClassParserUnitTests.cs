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
            StringReader reader = new StringReader(ExampleStrings.Class.SingleLine01);
            
            IDataParser parser = CreateDataParser(reader);

            AssertParsed(parser, new StringModel("Class", "MSF001-RECORD"));
        }

        [Test]
        public void SingleLineWithSpace()
        {
            StringReader reader = new StringReader(" " + ExampleStrings.Class.SingleLine01);

            IDataParser parser = CreateDataParser(reader);

            AssertParsed(parser, new StringModel("Class", "MSF001-RECORD"));
        }

        [Test]
        public void PropertyCases()
        {
            AssertDoesNotParse(ExampleStrings.Property.SimpleCases());
        }

        [Test]
        public void DataTypeCases()
        {
            AssertDoesNotParse(ExampleStrings.DataType.AllCases());
        }

        [Test]
        public void EnumDataTypeCases()
        {
            AssertDoesNotParse(ExampleStrings.EnumValue.AllCases());
        }

        [Test]
        public void RedefinesDataTypeCases()
        {
            AssertDoesNotParse(ExampleStrings.Redefines.AllCases());
        }
    }
}