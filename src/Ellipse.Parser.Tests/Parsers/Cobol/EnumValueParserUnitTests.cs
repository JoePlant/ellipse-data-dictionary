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
            Reader reader = Reader.CreateStringReader(ExampleStrings.EnumValue.SingleLine05);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("EnumValue", "PO-NO-ITEM VALUE 'PO'", "Purchase Order Number Item"));
        }

        [Test]
        public void SingleLine07()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.EnumValue.SingleLine09);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("EnumValue", "MIMS-CONTROL VALUE 'M'", "Indicates MIMS System Control Account"));
        }

        [Test]
        public void SingleLine07Trimmed()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.EnumValue.SingleLine09.Substring(2));
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("EnumValue", "MIMS-CONTROL VALUE 'M'", "Indicates MIMS System Control Account"));
        }

        [Test]
        public void SingleLine09()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.EnumValue.SingleLine11);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("EnumValue", "EGI-TYPE VALUE 'G'", "EGI type record"));
        }

        [Test]
        public void SingleLine11()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.EnumValue.SingleLine13);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("EnumValue", "RES-TY VALUE 'R'", "Resource Type"));
        }

        [Test]
        public void MultiLine03()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.EnumValue.MultiLine05);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("EnumValue", "TARGT-NO-AUTOGEN VALUE 'N'", "No Autogenerate Interdistrict Account\nEntries"));
        }


        [Test]
        public void MultiLine13()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.EnumValue.SingleLine15);
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

        [Test]
        public void RedefinesDataTypeCases()
        {
            AssertDoesNotParse(ExampleStrings.Redefines.AllCases());
        }
    }
}