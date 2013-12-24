using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers.Lines
{
    [TestFixture]
    public class RepeatLineMatcherUnitTests : TestFixture
    {
        [Test]
        public void RepeatLines()
        {
            ILineMatcher repeat = Line.Repeat(Line.Any()).Until(Line.Contains("Stop"));

            Reader reader = Reader.CreateStringReader("One\nTwo\nThree\nStop\nFour");
            int linesRead;
            Assert.That(repeat.Matches(reader, 0, out linesRead), Is.True);
            Assert.That(linesRead, Is.EqualTo(3));
        }

        [Test]
        public void StopAfter1Line()
        {
            ILineMatcher repeat = Line.Repeat(Line.Any()).Until(Line.Contains("Stop"));

            Reader reader = Reader.CreateStringReader("One\nStop\nTwo");
            int linesRead;
            Assert.That(repeat.Matches(reader, 0, out linesRead), Is.True);
            Assert.That(linesRead, Is.EqualTo(1));
        }

        [Test]
        public void NoStop()
        {
            ILineMatcher repeat = Line.Repeat(Line.Any()).Until(Line.Contains("Stop"));

            Reader reader = Reader.CreateStringReader("One\nTwo");
            int linesRead;
            Assert.That(repeat.Matches(reader, 0, out linesRead), Is.True);
            Assert.That(linesRead, Is.EqualTo(2));
        }

        [Test]
        public void ImmediateStop()
        {
            ILineMatcher repeat = Line.Repeat(Line.Any()).Until(Line.Contains("Stop"));

            Reader reader = Reader.CreateStringReader("Stop\nOne\nTwo");
            int linesRead;
            Assert.That(repeat.Matches(reader, 0, out linesRead), Is.False);
            Assert.That(linesRead, Is.EqualTo(0));
        }
    }
}