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
            ILineParser lineParser = Data.IgnoreAfter(".").ExcludeMarker();

            AssertWillParse(lineParser, "Data", "Data.Testing", "Data.", "Data.  Testing. ", "Data.   Testing");
            AssertWillParse(lineParser, "", ".", ".", "");
            AssertNoChange(lineParser, "Data", "there is no dot", "dot", ",");
        }

        [Test]
        public void IgnoreBefore()
        {
            ILineParser lineParser = Comment.IgnoreBefore(".").ExcludeMarker();

            AssertWillParse(lineParser, "Data", "TEsting.Data", ".Data", "Testing    .Data");
            AssertWillParse(lineParser, "", ".", "", "Data.");
            AssertNoChange(lineParser, "Data", "there is no dot", "dot", ",");
        }

        [Test]
        public void IgnoreBefore2Chars()
        {
            ILineParser lineParser = Comment.IgnoreBefore("./").ExcludeMarker();

            AssertWillParse(lineParser, "Data", "TEsting./Data", "./Data", "Testing    ./Data");
            AssertWillParse(lineParser, "", "./", "", "Data./");
            AssertNoChange(lineParser, "Data", "there is no dot", "dot", ",", ".", "/", "/.");
        }

        [Test]
        public void IgnoreAfterIncludeMarker()
        {
            ILineParser lineParser = Data.IgnoreAfter(".").IncludeMarker();

            AssertWillParse(lineParser, "Data.", "Data.Testing", "Data.", "Data.  Testing. ", "Data.   Testing");
            AssertWillParse(lineParser, ".", ".", ".");
            AssertNoChange(lineParser, "Data", "there is no dot", "dot", ",", "");
        }

        [Test]
        public void IgnoreBeforeIncludeMarker()
        {
            ILineParser lineParser = Comment.IgnoreBefore(".").IncludeMarker();

            AssertWillParse(lineParser, ".Data", "TEsting.Data", ".Data", "Testing    .Data");
            AssertWillParse(lineParser, ".", ".", "Data.");
            AssertNoChange(lineParser, "Data", "there is no dot", "dot", ",", "");
        }

        [Test]
        public void IgnoreBefore2CharsIncludeMarker()
        {
            ILineParser lineParser = Comment.IgnoreBefore("./").IncludeMarker();

            AssertWillParse(lineParser, "./Data", "TEsting./Data", "./Data", "Testing    ./Data");
            AssertWillParse(lineParser, "./", "./", "Data./");
            AssertNoChange(lineParser, "Data", "there is no dot", "dot", ",", ".", "/", "/.", "");
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

        [Test]
        public void TruncateAt()
        {
            ILineParser lineParser = Data.TruncateAtColumn(3);

            AssertWillParse(lineParser, "One", "One", "OneTwo", "One Two");
            AssertWillParse(lineParser, "On", "On");
            AssertWillParse(lineParser, "O", "O");

            AssertWillParse(lineParser, "O  ", "O  Two", "O  ");
        }

        [Test]
        public void IgnoreBeforeColumn()
        {
            ILineParser lineParser = Comment.IgnoreBefore(3);

            AssertWillParse(lineParser, "Two", "OneTwo", "On Two", "O  Two", "   Two");
            AssertWillParse(lineParser, "", "", "A", "On");
        }

        [Test]
        public void SplitAndIgnore()
        {
            ILineParser lineParser = Data.SplitOn("-").Ignore(0, 1).Join(" ");
            AssertWillParse(lineParser, "C D", "A-B-C-D", "AA-BB-C-D");
            AssertWillParse(lineParser, "C", "A-B-C", "AA-BB-C");
            AssertNoChange(lineParser, "A", "C", "C D E");
        }

        [Test]
        public void SplitAndIgnoreFollowing()
        {
            ILineParser lineParser = Data.SplitOn("-").Ignore(2).AndFollowing().Join(" ");
            AssertWillParse(lineParser, "A B", "A-B-C", "A-B-C-D", "A-B-C-D-E");
            AssertNoChange(lineParser, "A", "A-B", "C", "C D E");
        }

        [Test]
        public void SplitAndSelect()
        {
            ILineParser lineParser = Data.SplitOn("-").Select(0, 1).Join(" ");
            AssertWillParse(lineParser, "A B", "A-B-C-D", "A-B", "A-B-C");
            AssertNoChange(lineParser, "A", "C", "C D E");
        }

        [Test]
        public void SplitAndSelectFollowing()
        {
            ILineParser lineParser = Data.SplitOn("-").Select(2).AndFollowing().Join(" ");
            AssertWillParse(lineParser, "C D E", "A-B-C-D-E");
            AssertWillParse(lineParser, "C D", "A-B-C-D");
            AssertWillParse(lineParser, "C", "A-B-C");
            AssertNoChange(lineParser, "A", "A-B");
            AssertNoChange(lineParser, "C", "C D E");
        }

        [Test]
        public void SplitFindAndIgnore()
        {
            ILineParser lineParser = Data.SplitOn("-").Find("B").Ignore(0, 1).Join(" ");
            AssertWillParse(lineParser, "A D", "A-B-C-D", "A-D-B-0", "B-C-A-D");
            AssertNoChange(lineParser, "A", "C", "C D E");
            AssertNoChange(lineParser, "A-D-C-B");  // B + 1 is out of range
        }

        [Test]
        public void SplitFindAndIgnoreFollowing()
        {
            ILineParser lineParser = Data.SplitOn("-").Find("B").Ignore(1).AndFollowing().Join(" ");
            AssertWillParse(lineParser, "A B", "A-B-C", "A-B-C-D", "A-B-C-D-E");
            AssertNoChange(lineParser, "A", "A-B", "C", "C D E");
            AssertNoChange(lineParser, "A-D-C-B");  // B + 1 is out of range
        }

        [Test]
        public void SplitFindAndSelect()
        {
            ILineParser lineParser = Data.SplitOn("-").Find("B").Select(0, 1).Join(" ");
            AssertWillParse(lineParser, "B C", "A-B-C-D", "A-D-B-C", "B-C");
            AssertNoChange(lineParser, "A", "C", "C D E");
            AssertNoChange(lineParser, "A-D-C-B");  // B + 1 is out of range
        }

        [Test]
        public void SplitFindAndSelectFollowing()
        {
            ILineParser lineParser = Data.SplitOn("-").Find("B").Select(1).AndFollowing().Join(" ");
            AssertWillParse(lineParser, "C D E", "A-B-C-D-E");
            AssertWillParse(lineParser, "C D", "A-B-C-D");
            AssertWillParse(lineParser, "C", "A-B-C");
            AssertNoChange(lineParser, "A", "A-B");
            AssertNoChange(lineParser, "C", "C D E");
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