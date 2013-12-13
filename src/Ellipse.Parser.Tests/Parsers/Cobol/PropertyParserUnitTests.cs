using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    [TestFixture]
    public class PropertyParserUnitTests : ParserTestFixture<PropertyParser>
    {
        private const string case1 =
            "    03  TAX-PERIOD-CLOSED       PIC X(1).                   [  27] Tax Period Closed                        DB";

        private const string case2 =
            "    03  END-DATE.                                           [  11] Ending date                              DATE\n" +
            "                                                                                                            DB";

        [Test]
        public void SingleLine()
        {
            StringReader reader = new StringReader(case1);

            IDataParser parser = CreateDataParser(reader);
            parser.Parse();

            AssertResults(parser, new CobolModel("Property", "TAX-PERIOD-CLOSED PIC X(1)", "[ 27] Tax Period Closed DB"));
        }

        [Test]
        public void Twolines()
        {
            StringReader reader = new StringReader(case2);

            IDataParser parser = CreateDataParser(reader);
            parser.Parse();

            AssertResults(parser, new CobolModel("Property", "END-DATE", "[ 11] Ending date DATE\nDB"));
        }
    }
}