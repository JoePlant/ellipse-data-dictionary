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
            StringReader reader = new StringReader(ExampleStrings.Property.SingleLine03);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "KEY-004", "[ 1] key of MSF004 FK:0"));
        }

        [Test]
        public void SingleLine03Trimmed()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.SingleLine03.Substring(2));
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "KEY-004", "[ 1] key of MSF004 FK:0"));
        }

        [Test]
        public void MultiLineLevel03()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.MultiLine03);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "END-DATE", "[ 11] Ending date DATE\nDB"));
        }

        [Test]
        public void MultiLineLevel05()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.MultiLine05);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "CONTROL-ID", "[ 29] ID's Subledger,MIMS Sys & InterComp Ctl MANDATORY\nDB,KEY:0"));
        }

        [Test]
        public void SingleLineLevel07()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.SingleLine07);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "CONTROL-NUMBER", "[ 30] No Identifying MIMS System Ctl Account"));
        }

        [Test]
        public void MultiLineLevel09()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.MultiLine09);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "INT-DSTRCT", "[ 30] InterDist Dist Code Ident. Target Dist MANDATORY VALUE\n(DSTRCT-CODE) ERROR\n(6534) ACTIVE"));
        }

        [Test]
        public void SingleLineLevel11()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.SingleLine11);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "MSF061-DATA-1-061-1A", "[ 5] Reference Data 1a"));
        }

        [Test]
        public void SingleLineLevel13()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.SingleLine13);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "MSF062-CONTRACT-NO-RC", "[ 38] Contract Number"));
        }

        [Test]
        public void SingleLineLevel15()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.SingleLine15);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "MSF062-ACCOUNT-CODE-NA", "[ 33] Account Code number ACCOUNT-CODE"));
        }

        [Test]
        public void SingleLineLevel17()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.SingleLine17);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "MSF062-DATA-2-062-PA", "[ 33] Reference data 2"));
        }

        [Test]
        public void SingleLineLevel19()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.SingleLine19);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "MSF062-TOP-PAR-PA-PB", "[ 33] Parent Account Code"));
        }

        [Test]
        public void SingleLineLevel21()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.SingleLine21);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "MSF062-DATA-2-062-WA", "[ 33] Reference data 2"));
        }

        [Test]
        public void SingleLineLevel23()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.SingleLine23);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "MSF062-WLD-ACCT-WA-WB", "[ 33] Account Code number ACCOUNT-CODE"));
        }

        [Test]
        public void SingleLineLevel25()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.SingleLine25);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "MSF062-DATA-2-062-WB", "[ 33] Reference data 2"));
        }

        [Test]
        public void SingleLineLevel27()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.SingleLine27);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "MSF062-DATA-2-062-BA", "[ 33] Reference data 2"));
        }

        [Test]
        public void MultiLineLevel29()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.MultiLine29);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "MSF062-BUDG-ACCT-CODE-BA", "[ 33] Account Code number ACCOUNT-CODE"));
        }

        [Test]
        public void PropertyFollowedByDataType()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.Property05DataType07);
            IDataParser parser = CreateDataParser(reader, PropertyParser.HierarchyParser(3));

            Model model = Build
                .Property("LINE-NO", "[ 19] Line No. of description")
                .With(
                    Build.DataType("LINE-NO-9 PIC 9(4)", "[ 19] Line No. of description")
                ).Model();

            AssertParsed(parser,model);
        }

        [Test]
        public void PropertyContainingPic()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.PropertyContainingReservedWord);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "LST-CON-PICK-NO", "[ 11] Last Consolidate Picking Slip Number DB"));
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

        [Test]
        public void RedefinesDataTypeCases()
        {
            AssertDoesNotParse(ExampleStrings.Redefines.AllCases());
        }
    }
}