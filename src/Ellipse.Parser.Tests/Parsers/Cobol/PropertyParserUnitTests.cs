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
        public void MultiLineLevel03()
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
        public void SingleLineLevel11()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.Case6);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "MSF061-DATA-1-061-1A", "[ 5] Reference Data 1a"));
        }

        [Test]
        public void SingleLineLevel13()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.Case7);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "MSF062-CONTRACT-NO-RC", "[ 38] Contract Number"));
        }

        [Test]
        public void SingleLineLevel15()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.Case8);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "MSF062-ACCOUNT-CODE-NA", "[ 33] Account Code number ACCOUNT-CODE"));
        }

        [Test]
        public void SingleLineLevel17()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.Case9);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "MSF062-DATA-2-062-PA", "[ 33] Reference data 2"));
        }

        [Test]
        public void SingleLineLevel19()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.Case10);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "MSF062-TOP-PAR-PA-PB", "[ 33] Parent Account Code"));
        }

        [Test]
        public void SingleLineLevel21()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.Case11);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "MSF062-DATA-2-062-WA", "[ 33] Reference data 2"));
        }

        [Test]
        public void SingleLineLevel23()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.Case12);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "MSF062-WLD-ACCT-WA-WB", "[ 33] Account Code number ACCOUNT-CODE"));
        }

        [Test]
        public void SingleLineLevel25()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.Case13);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "MSF062-DATA-2-062-WB", "[ 33] Reference data 2"));
        }

        [Test]
        public void SingleLineLevel27()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.Case14);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "MSF062-DATA-2-062-BA", "[ 33] Reference data 2"));
        }

        [Test]
        public void SingleLineLevel29()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.Case15);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "MSF062-BUDG-ACCT-CODE-BA", "[ 33] Account Code number ACCOUNT-CODE"));
        }

        [Test]
        public void PropertyFollowedByDataType()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.FollowingCase7);
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