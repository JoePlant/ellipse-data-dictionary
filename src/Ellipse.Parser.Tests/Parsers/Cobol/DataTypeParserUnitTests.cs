using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    [TestFixture]
    public class DataTypeParserUnitTests : ParserTestFixture<DataTypeParser>
    {
        [Test]
        public void SingleLine03()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.DataType.SingleLine03);
            IDataParser parser = CreateDataParser(reader);
            AssertParsedUsingXml(parser, new CobolModel("DataType", "TAX-PERIOD-CLOSED PIC X(1)", "[ 27] Tax Period Closed DB"));
        }

        [Test]
        public void SingleLine05()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.DataType.SingleLine05);
            IDataParser parser = CreateDataParser(reader);
            AssertParsedUsingXml(parser, new CobolModel("DataType", "FULL-PERIOD PIC X(6)", "[ 5] Full Period CCYYPP DB,KEY:0"));
        }

        [Test]
        public void SingleLine05Trimmed()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.DataType.SingleLine05.Substring(1));
            IDataParser parser = CreateDataParser(reader);
            AssertParsedUsingXml(parser, new CobolModel("DataType", "FULL-PERIOD PIC X(6)", "[ 5] Full Period CCYYPP DB,KEY:0"));
        }

        [Test]
        public void SingleLine09()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.DataType.SingleLine09);
            IDataParser parser = CreateDataParser(reader);
            AssertParsedUsingXml(parser, new CobolModel("DataType", "AVAIL-SOH PIC S9(9) COMP-3", "[1851] Available SOH"));
        }

        [Test]
        public void SingleLine11()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.DataType.SingleLine11);
            IDataParser parser = CreateDataParser(reader);
            AssertParsedUsingXml(parser, new CobolModel("DataType", "SUBLEDGER-TYPE PIC X(2)", "[ 30] Subledger Account Type OPTIONAL TABLE ('SA')"));
        }

        [Test]
        public void SingleLine13()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.DataType.SingleLine13);
            IDataParser parser = CreateDataParser(reader);
            AssertParsedUsingXml(parser, new CobolModel("DataType", "MSF061-MSB566-RUN-1A PIC X(4)", "[ 5] MSB566 Run Number NUMERIC RANGE (11)"));
        }

        [Test]
        public void SingleLine15()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.DataType.SingleLine15);
            IDataParser parser = CreateDataParser(reader);
            AssertParsedUsingXml(parser, new CobolModel("DataType", "MSF062-REQ-NO-RC PIC X(6)", "[ 38] Requisition number"));
        }

        [Test]
        public void MultiLine05()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.DataType.MultiLines05);
            IDataParser parser = CreateDataParser(reader);
            AssertParsedUsingXml(parser, new CobolModel("DataType", "DSTRCT-CODE PIC X(4)", "[ 1] District Code MANDATORY VALUE\n(DSTRCT-CODE) ERROR\n(6534) ACTIVE\nDB,KEY:0"));
        }

        [Test]
        public void MultiLine31()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.DataType.MultiLines31);
            IDataParser parser = CreateDataParser(reader);
            AssertParsedUsingXml(parser, new CobolModel("DataType", "MSF062-DATA-2-062-PB PIC X(24)", "[ 33] Reference data 2"));
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

        [Test]
        public void SameInstance()
        {
            AssertSameParser(() => DataTypeParser.HierarchyParser(3));
        }
    }
}