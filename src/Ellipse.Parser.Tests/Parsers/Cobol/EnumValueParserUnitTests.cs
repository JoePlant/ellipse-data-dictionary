using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    [TestFixture]
    public class EnumValueParserUnitTests : ParserTestFixture<EnumValueParser>
    {
        [Test]
        public void SingleLine03()
        {
            StringReader reader = new StringReader(ExampleStrings.EnumValue.SingleLine03);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("EnumValue", "PO-NO-ITEM VALUE 'PO'", "Purchase Order Number Item"));
        }

        [Test]
        public void SingleLine07()
        {
            StringReader reader = new StringReader(ExampleStrings.EnumValue.SingleLine07);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("EnumValue", "MIMS-CONTROL VALUE 'M'", "Indicates MIMS System Control Account"));
        }

        [Test]
        public void SingleLine09()
        {
            StringReader reader = new StringReader(ExampleStrings.EnumValue.SingleLine09);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("EnumValue", "EGI-TYPE VALUE 'G'", "EGI type record"));
        }

        [Test]
        public void SingleLine11()
        {
            StringReader reader = new StringReader(ExampleStrings.EnumValue.SingleLine11);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("EnumValue", "RES-TY VALUE 'R'", "Resource Type"));
        }

        [Test]
        public void MultiLine03()
        {
            StringReader reader = new StringReader(ExampleStrings.EnumValue.MultiLine03);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("EnumValue", "TARGT-NO-AUTOGEN VALUE 'N'", "No Autogenerate Interdistrict Account\nEntries"));
        }


        [Test]
        public void MultiLine13()
        {
            StringReader reader = new StringReader(ExampleStrings.EnumValue.SingleLine13);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("EnumValue", "MSF062-ETP-TRAIN-PROG VALUE 'P'", "Employee Training Plan Program"));
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