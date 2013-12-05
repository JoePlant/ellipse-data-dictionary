using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers.Lines
{
    [TestFixture]
    public class LineMatcherUnitTests : TestFixture
    {
        [Test]
        public void LineStartsWith()
        {
            ILineMatcher lineMatcher = Line.StartsWith("Testing");

            AssertDoesNotMatch(lineMatcher, null, "", "    ", "TESTING: ", "Testin", "   Testing", "testing",
                               "Ending with Testing");
            AssertMatches(lineMatcher, "Testing", "Testing  ", "Testing that this line matches", "TestingWithoutSpaces");
        }

        [Test]
        public void LineContains()
        {
            ILineMatcher lineMatcher = Line.Contains("Testing");

            AssertDoesNotMatch(lineMatcher, null, "", "    ", "TESTING: ", "Testin", "testing");
            AssertMatches(lineMatcher, "Testing", "Testing  ", "Testing that this line matches", "TestingWithoutSpaces",
                          "That Testing in the middle matches", "At the End: Testing");
        }

        [Test]
        public void LineContainsMultiplePositions()
        {
            ILineMatcher lineMatcher = new LineMatcher.LineContains("Test");
            StringReader reader = new StringReader("Test\nTest\nTest");
            AssertDoesMatch(lineMatcher, reader, 0,1,2);
            AssertDoesNotMatch(lineMatcher, reader, 3);

            Assert.That(reader.ReadLine(), Is.EqualTo("Test"));
            AssertDoesMatch(lineMatcher, reader, 0, 1);
            AssertDoesNotMatch(lineMatcher, reader, 2);

            Assert.That(reader.ReadLine(), Is.EqualTo("Test"));
            AssertDoesMatch(lineMatcher, reader, 0);
            AssertDoesNotMatch(lineMatcher, reader, 1);

            Assert.That(reader.ReadLine(), Is.EqualTo("Test"));
            AssertDoesNotMatch(lineMatcher, reader, 0);
        }

        private void AssertDoesMatch(ILineMatcher matcher, IReader reader, params int[] args)
        {
            foreach (int i in args)
            {
                int lines;
                Assert.That(matcher.Matches(reader, i, out lines), Is.True, "{0} offset {1}: read {2} lines", reader, i, lines);
            }
        }

        private void AssertDoesNotMatch(ILineMatcher matcher, IReader reader, params int[] args)
        {
            foreach (int i in args)
            {
                int lines;
                Assert.That(matcher.Matches(reader, i, out lines), Is.False, "{0} offset {1}: read {2} lines", reader, i, lines);
            }
        }

        private void AssertMatches(ILineMatcher matcher, params string[] args)
        {
            foreach (string line in args)
            {
                StringReader reader = new StringReader(line);
                int lines;
                Assert.That(matcher.Matches(reader, 0, out lines), Is.True, line);
            }
        }

        private void AssertDoesNotMatch(ILineMatcher matcher, params string[] args)
        {
            foreach (string line in args)
            {
                StringReader reader = new StringReader(line);
                int lines;
                Assert.That(matcher.Matches(reader, 0, out lines), Is.False, line);
            }
        }

    }
}