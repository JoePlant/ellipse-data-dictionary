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
            Reader reader = Reader.CreateStringReader(ExampleStrings.Class.SingleLine01);
            
            IDataParser parser = CreateDataParser(reader);

            AssertParsedUsingXml(parser, ClassModel.Factory("Class", "MSF001-RECORD", null));
        }

        [Test]
        public void SingleLineWithSpace()
        {
            Reader reader = Reader.CreateStringReader(" " + ExampleStrings.Class.SingleLine01);

            IDataParser parser = CreateDataParser(reader);

            AssertParsedUsingXml(parser, ClassModel.Factory("Class", "MSF001-RECORD", null));
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

        [Test]
        public void OccursCases()
        {
            AssertDoesNotParse(ExampleStrings.Occurs.AllCases());
        }
    }
}