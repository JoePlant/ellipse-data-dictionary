﻿using Ellipse.DataDictionary.Readers;
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
        public void LineStartsWithIgnoreSpaces()
        {
            ILineMatcher lineMatcher = Line.StartsWithMarker("Testing");

            AssertDoesNotMatch(lineMatcher, null, "", "    ", "TESTING: ", "Testin",  "testing",
                               "Ending with Testing");
            AssertMatches(lineMatcher, "Testing", "Testing  ", "Testing that this line matches", "TestingWithoutSpaces", " Testing", "  Testing", "   Testing");
        }

        [Test]
        public void LineOr()
        {
            ILineMatcher lineMatcher = Line.Or(Line.StartsWith("AA"), Line.StartsWith("BB"));

            AssertDoesNotMatch(lineMatcher, null, "", "    ", "test", "TestAA", "TestBB");
            AssertMatches(lineMatcher, "AA", "BB","AAC", "AATest", "BBTest");
        }

        [Test]
        public void LineAnd2()
        {
            ILineMatcher lineMatcher = Line.And(Line.Contains("One"), Line.Contains("Two"));

            AssertDoesNotMatch(lineMatcher, null, "", "    ", "One", "Two", "Three", "One One", "Two Two");
            AssertMatches(lineMatcher, "OneTwo", "TwoOne", "  One  Two  ", "  Two  One  ", "One  Two", "Two  One", "One One Two", "One Two Two");
        }

        [Test]
        public void LineAnd3()
        {
            ILineMatcher lineMatcher = Line.And(Line.Contains("One"), Line.Contains("Two"), Line.Contains("Three"));

            AssertDoesNotMatch(lineMatcher, null, "", "    ", "One", "Two", "Three", "One One", "Two Two", "One Two", "One Three", "Three One");
            AssertMatches(lineMatcher, "OneTwoThree", "ThreeTwoOne", "  One  Two  Three  ", "  Three  Two  One  ", "One  Two  Three", "Three  Two  One", "One One Two Three", "One Two Two Three", "One Two Three Three");
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
        public void DoesNotContain()
        {
            ILineMatcher lineMatcher = Line.DoesNotContain("Testing");
            
            // need to use "\n" as an empty string
            AssertMatches(lineMatcher, "\n", "    ", "TESTING: ", "Testin", "testing");
            AssertDoesNotMatch(lineMatcher, "Testing", "Testing  ", "Testing that this line matches", "TestingWithoutSpaces",
                          "That Testing in the middle matches", "At the End: Testing");
        }

        [Test]
        public void LineIsEmpty()
        {
            ILineMatcher lineMatcher = Line.IsEmpty();

            AssertDoesNotMatch(lineMatcher, null, "    ", "TESTING: ", "Testin", "testing");
            AssertMatches(lineMatcher, "\n");
        }

        [Test]
        public void LineIsNotEmpty()
        {
            ILineMatcher lineMatcher = Line.IsNotEmpty();

            AssertMatches(lineMatcher, "    ", "TESTING: ", "Testin", "testing");
            AssertDoesNotMatch(lineMatcher, null, string.Empty);
        }

        [Test]
        public void LineOptional()
        {
            ILineMatcher lineMatcher = Line.Optional(Line.Contains("Testing"));

            // matches invalid lines
            AssertMatches(lineMatcher, null, "", "    ", "TESTING: ", "Testin", "testing");
            // matches valid lines
            AssertMatches(lineMatcher, "Testing", "Testing  ", "Testing that this line matches", "TestingWithoutSpaces",
                          "That Testing in the middle matches", "At the End: Testing");
        }

        [Test]
        public void LineContainsMultiplePositions()
        {
            ILineMatcher lineMatcher = new LineMatcher.LineContains("Test");
            Reader reader = Reader.CreateStringReader("Test\nTest\nTest");
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
                Reader reader = Reader.CreateStringReader(line);
                int lines;
                Assert.That(matcher.Matches(reader, 0, out lines), Is.True, "Line: '{0}'", line);
            }
        }

        private void AssertDoesNotMatch(ILineMatcher matcher, params string[] args)
        {
            foreach (string line in args)
            {
                Reader reader = Reader.CreateStringReader(line);
                int lines;
                Assert.That(matcher.Matches(reader, 0, out lines), Is.False, line);
            }
        }

    }
}