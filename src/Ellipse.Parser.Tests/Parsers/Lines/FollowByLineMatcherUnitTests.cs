using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers.Lines
{
    [TestFixture]
    public class FollowByLineMatcherUnitTests : TestFixture
    {
        [Test]
        public void MatchingWithFollowingLine()
        {
            ILineMatcher followedBy = Line.FollowedBy(Line.Contains("Marker"), Line.Contains("Follow"));

            StringReader reader = new StringReader("Marker\nFollow\nMore");
            int linesRead;
            Assert.That(followedBy.Matches(reader, 0, out linesRead), Is.True);
            Assert.That(linesRead, Is.EqualTo(1));
        }

        [Test]
        public void DoesNotMatchWithLateFollowing()
        {
            ILineMatcher followedBy = Line.FollowedBy(Line.Contains("Marker"), Line.Contains("Follow"));

            StringReader reader = new StringReader("Marker\nMore\nFollow");
            int linesRead;
            Assert.That(followedBy.Matches(reader, 0, out linesRead), Is.False);
            Assert.That(linesRead, Is.EqualTo(0));
        }

        [Test]
        public void MarkerWithOptionalFollowFound()
        {
            ILineMatcher followedBy = Line.FollowedBy(Line.Contains("Marker"), Line.Optional(Line.Contains("Follow")));

            StringReader reader = new StringReader("Marker\nFollow\nMore");
            int linesRead;
            Assert.That(followedBy.Matches(reader, 0, out linesRead), Is.True);
            Assert.That(linesRead, Is.EqualTo(1));
        }

        [Test]
        public void MarkerWithOptionalFollowMissing()
        {
            ILineMatcher followedBy = Line.FollowedBy(Line.Contains("Marker"), Line.Optional(Line.Contains("Follow")));

            StringReader reader = new StringReader("Marker\nMore");
            int linesRead;
            Assert.That(followedBy.Matches(reader, 0, out linesRead), Is.True);
            Assert.That(linesRead, Is.EqualTo(1));
        }
    }
}