using System;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers.Lines
{
    [TestFixture]
    public class LineParserUnitTests : TestFixture
    {
        [Test]
        public void IgnoreStart()
        {
            ILineParser lineParser = Data.IgnoreStart("Testing:");

            AssertNoChange(lineParser, null, "", "TESTING: ", "Testin", "   Testing", "testing", "Ending with Testing");
            AssertWillParse(lineParser, "Data", "Testing:Data");
            AssertWillParse(lineParser, "", "Testing:");
        }

        [Test]
        public void IgnoreStartAndTrim()
        {
            ILineParser lineParser = Data.IgnoreStart("Testing:").Trim();

            AssertWillParse(lineParser, "Data", "Testing:Data", "Testing: Data ", "Testing:Data ", "Testing:   Data");
            AssertWillParse(lineParser, "", "Testing:", "Testing:     ", "Testing: ");
        }

        [Test]
        public void IgnoreAfter()
        {
            ILineParser lineParser = Data.IgnoreAfter(".");

            AssertWillParse(lineParser, "Data", "Data.Testing", "Data.", "Data.  Testing. ", "Data.   Testing");
            AssertWillParse(lineParser, "", ".", ".", "");
            AssertNoChange(lineParser, "Data", "there is no dot", "dot", ",");
        }

        [Test]
        public void IgnoreBefore()
        {
            ILineParser lineParser = Comment.IgnoreBefore(".");

            AssertWillParse(lineParser, "Data", "TEsting.Data", ".Data", "Testing    .Data");
            AssertWillParse(lineParser, "", ".", "", "Data.");
            AssertNoChange(lineParser, "Data", "there is no dot", "dot", ",");
        }

        [Test]
        public void IgnoreAll()
        {
            ILineParser lineParser = Comment.IgnoreAll();

            AssertWillParse(lineParser, null, "", "Data", "TEsting.Data", ".Data", "Testing    .Data", ".", " any line");
        }

        [Test]
        public void RemoveSpaces()
        {
            ILineParser lineParser = Data.RemoveSpaces();

            AssertWillParse(lineParser, "One Two", "One Two", "One  Two", " One  Two ", "   One Two");
            AssertWillParse(lineParser, ".", ".");
            AssertNoChange(lineParser, "One", "there is no dot", "dot", ",");
        }

        private void AssertWillParse(ILineParser parser, string expected, params string[] args)
        {
            foreach (string line in args)
            {
                Assert.That(parser.Parse(0,line), Is.EqualTo(expected), line);
            }
        }

        private void AssertNoChange(ILineParser matcher, params string[] args)
        {
            foreach (string line in args)
            {
                Assert.That(matcher.Parse(0,line), Is.EqualTo(line), line);
            }
        } 
    }
}