using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    [TestFixture]
    public class EnumValueParserUnitTests : ParserTestFixture<EnumValueParser>
    {
        [Test]
        public void SingleLine07()
        {
            StringReader reader = new StringReader(ExampleStrings.EnumValue.Case1);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("EnumValue", "MIMS-CONTROL VALUE 'M'", "Indicates MIMS System Control Account"));
        }

        [Test]
        public void SingleLine03()
        {
            StringReader reader = new StringReader(ExampleStrings.EnumValue.Case2);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("EnumValue", "PO-NO-ITEM VALUE 'PO'", "Purchase Order Number Item"));
        }

        [Test]
        public void MultiLine03()
        {
            StringReader reader = new StringReader(ExampleStrings.EnumValue.Case3);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("EnumValue", "TARGT-NO-AUTOGEN VALUE 'N'", "No Autogenerate Interdistrict Account\nEntries"));
        }
        [Test]
        public void ClassCases()
        {
            AssertDoesNotParse(ExampleStrings.Class.AllCases());
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

    }
}