using System;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers.Lines
{
    [TestFixture]
    public class LineParserUnitTests : TestFixture
    {
        [Test]
        public void LineStartsWith()
        {
            ILineParser lineParser = Data.IgnoreStart("Testing:");

            AssertDoesNotMatch(lineParser, "Data", null, "", "TESTING: ", "Testin", "   Testing", "testing", "Ending with Testing");
            AssertMatches(lineParser, "Data", "Testing:Data");
            AssertMatches(lineParser, "", "Testing:");
        }

        [Test]
        public void LineContains()
        {
            ILineParser lineParser = Data.IgnoreStart("Testing:").Trim();

            AssertMatches(lineParser, "Data", "Testing:Data", "Testing: Data ", "Testing:Data ", "Testing:   Data");
            AssertMatches(lineParser, "", "Testing:", "Testing:     ", "Testing: ");
        }

        private void AssertMatches(ILineParser parser, string expected, params string[] args)
        {
            foreach (string line in args)
            {
                Assert.That(parser.Parse(line), Is.EqualTo(expected), line);
            }
        }

        private void AssertDoesNotMatch(ILineParser matcher, string expected, params string[] args)
        {
            foreach (string line in args)
            {
                Assert.That(matcher.Parse(line), Is.Not.EqualTo(expected), line);
            }
        } 
    }
}