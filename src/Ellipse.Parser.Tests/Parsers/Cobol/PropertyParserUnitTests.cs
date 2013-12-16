using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    [TestFixture]
    public class PropertyParserUnitTests : ParserTestFixture<PropertyParser>
    {
        [Test]
        public void SingleLine()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.Case1);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "KEY-004", "[ 1] key of MSF004 FK:0"));
        }

        [Test]
        public void Twolines()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.Case2);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Property", "END-DATE", "[ 11] Ending date DATE\nDB"));
        }

        [Test]
        public void PropertyFollowedByDataType()
        {
            StringReader reader = new StringReader(ExampleStrings.Property.FollowingCase3);
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
    }
}