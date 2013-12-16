using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    [TestFixture]
    public class PropertyParserUnitTests : ParserTestFixture<PropertyParser>
    {
        [Test]
        public void SingleLine03()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.Case1);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "KEY-004", "[ 1] key of MSF004 FK:0"));
        }

        [Test]
        public void MultLineLevel03()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.Case2);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "END-DATE", "[ 11] Ending date DATE\nDB"));
        }

        [Test]
        public void MultiLineLevel05()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.Case3);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "CONTROL-ID", "[ 29] ID's Subledger,MIMS Sys & InterComp Ctl MANDATORY\nDB,KEY:0"));
        }

        [Test]
        public void SingleLineLevel07()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.Case4);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "CONTROL-NUMBER", "[ 30] No Identifying MIMS System Ctl Account"));
        }

        [Test]
        public void SingleLineLevel09()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.Case5);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "INT-DSTRCT", "[ 30] InterDist Dist Code Ident. Target Dist MANDATORY VALUE\n(DSTRCT-CODE) ERROR\n(6534) ACTIVE"));
        }

        [Test]
        public void PropertyFollowedByDataType()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.FollowingCase6);
            IDataParser parser = CreateDataParser(reader, new DataTypeParser());
            AssertParsed(parser,
                         new CobolModel("Property", "LINE-NO", "[  19] Line No. of description"),
                         new CobolModel("DataType", "LINE-NO-9 PIC 9(4)", "[  19] Line No. of description"));
        }

        [Test]
        public void ClassCases()
        {
            AssertDoesNotParse(ExampleStrings.Class.AllCases());
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

    }
}