using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers.Lines
{
    [TestFixture]
    public class MultipleLineMatcherUnitTests : TestFixture
    {
        [Test]
        public void TwoLines()
        {
            ILineMatcher multiple = Line.Multiple(Line.IsEqual("One"), Line.IsEqual("Two"));

            Reader reader = Reader.CreateStringReader("One\nTwo\nThree\nStop\nFour");
            int linesRead;
            Assert.That(multiple.Matches(reader, 0, out linesRead), Is.True);
            Assert.That(linesRead, Is.EqualTo(2));
        }

        [Test]
        public void TwoLinesReversed()
        {
            ILineMatcher multiple = Line.Multiple(Line.IsEqual("One"), Line.IsEqual("Two"));

            Reader reader = Reader.CreateStringReader("Two\nOne\nThree\nStop\nFour");
            int linesRead;
            Assert.That(multiple.Matches(reader, 0, out linesRead), Is.False);
            Assert.That(linesRead, Is.EqualTo(0));
        }

        [Test]
        public void RepeatUntilWithLines()
        {
            ILineMatcher multiple = Line.Multiple(Line.Repeat(Line.Any()).Until(Line.Contains("Stop")), Line.Contains("Stop"));

            Reader reader = Reader.CreateStringReader("One\nTwo\nStop\nThree");
            int linesRead;
            Assert.That(multiple.Matches(reader, 0, out linesRead), Is.True);
            Assert.That(linesRead, Is.EqualTo(3));
        }

        [Test]
        public void NoStop()
        {
            ILineMatcher repeat = Line.Multiple(Line.Repeat(Line.Any()).Until(Line.Contains("Stop")));

            Reader reader = Reader.CreateStringReader("One\nTwo");
            int linesRead;
            Assert.That(repeat.Matches(reader, 0, out linesRead), Is.True);
            Assert.That(linesRead, Is.EqualTo(2));
        }

        [Test]
        public void OptionalPlusFollowing()
        {
            ILineMatcher repeat = Line.Multiple(Line.Optional(Line.Contains("Stop")), Line.IsEqual("One"));

            Reader reader = Reader.CreateStringReader("Stop\nOne\nTwo");
            int linesRead;
            Assert.That(repeat.Matches(reader, 0, out linesRead), Is.True);
            Assert.That(linesRead, Is.EqualTo(2));
        }

        [Test]
        public void OptionalMissingFollowing()
        {
            ILineMatcher repeat = Line.Multiple(Line.Optional(Line.Contains("Stop")), Line.IsEqual("One"));

            Reader reader = Reader.CreateStringReader("One\nTwo");
            int linesRead;
            Assert.That(repeat.Matches(reader, 0, out linesRead), Is.True);
            Assert.That(linesRead, Is.EqualTo(1));
        }
    }
}