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
            StringReader reader = new StringReader(ExampleStrings.DataType.Case03SingleLine);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("DataType", "TAX-PERIOD-CLOSED PIC X(1)", "[ 27] Tax Period Closed DB"));
        }

        [Test]
        public void SingleLine05()
        {
            StringReader reader = new StringReader(ExampleStrings.DataType.Case05SingleLine);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("DataType", "FULL-PERIOD PIC X(6)", "[ 5] Full Period CCYYPP DB,KEY:0"));
        }

        [Test]
        public void SingleLine09()
        {
            StringReader reader = new StringReader(ExampleStrings.DataType.Case09SingleLine);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("DataType", "AVAIL-SOH PIC S9(9) COMP-3", "[1851] Available SOH"));
        }

        [Test]
        public void SingleLine11()
        {
            StringReader reader = new StringReader(ExampleStrings.DataType.Case11SingleLine);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("DataType", "SUBLEDGER-TYPE PIC X(2)", "[ 30] Subledger Account Type OPTIONAL TABLE ('SA')"));
        }

        [Test]
        public void SingleLine13()
        {
            StringReader reader = new StringReader(ExampleStrings.DataType.Case13SingleLine);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("DataType", "MSF061-MSB566-RUN-1A PIC X(4)", "[ 5] MSB566 Run Number NUMERIC RANGE (11)"));
        }

        [Test]
        public void SingleLine15()
        {
            StringReader reader = new StringReader(ExampleStrings.DataType.Case15SingleLine);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("DataType", "MSF062-REQ-NO-RC PIC X(6)", "[ 38] Requisition number"));
        }

        [Test]
        public void Multiline()
        {
            StringReader reader = new StringReader(ExampleStrings.DataType.Case05MultipleLines);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("DataType", "DSTRCT-CODE PIC X(4)", "[ 1] District Code MANDATORY VALUE\n(DSTRCT-CODE) ERROR\n(6534) ACTIVE\nDB,KEY:0"));
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

    }
}