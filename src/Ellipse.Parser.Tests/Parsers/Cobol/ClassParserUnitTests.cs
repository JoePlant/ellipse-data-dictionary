using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    [TestFixture]
    public class ClassParserUnitTests : ParserTestFixture<ClassParser>
    {
        private const string case1 = "01  MSF001-RECORD.";

        [Test]
        public void SingleLine()
        {
            StringReader reader = new StringReader(case1);
            
            IDataParser parser = CreateDataParser(reader);
            parser.Parse();

            AssertResults(parser, new StringModel("Class", "MSF001-RECORD"));
        }
    }
}