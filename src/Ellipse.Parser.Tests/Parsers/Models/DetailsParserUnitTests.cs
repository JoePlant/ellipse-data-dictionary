using System;
using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers.Models
{
    [TestFixture]
    public class DetailsParserUnitTests : TestFixture
    {
        private const string lineFeed = "\n";

        private readonly string[] detailsText = new []
            {
                "DETAILS:",
                "   This Department of defence specific file is used to record",
                "   technical substitution(TSUB) header details.",
                "MODULE:",
            };

        private IModelParser parser;

        protected override void OnSetUp()
        {
            parser = new DetailsParser();
            base.OnSetUp();
        }

        [Test]
        public void ParseWithMissingTrailer()
        {
            string text = string.Join(lineFeed, detailsText);

            string incomplete = text.Substring(0, text.IndexOf("technical", StringComparison.InvariantCulture));

            Assert.That(incomplete, Is.Not.EqualTo(text));

            IReader reader = new StringReader(incomplete);

            Assert.That(reader.LineNumber, Is.EqualTo(1));

            Assert.That(parser.Matches(reader), Is.True);

            Model model = parser.Parse(reader);
            Assert.That(model, Is.Not.Null);
            
            Assert.That(reader.LineNumber, Is.EqualTo(4));
            Assert.That(reader.EndOfFile, Is.True);
        }

        [Test]
        public void Parse()
        {
            string text = string.Join(lineFeed, detailsText);

            IReader reader = new StringReader(text);

            Assert.That(reader.LineNumber, Is.EqualTo(1));

            Assert.That(parser.Matches(reader), Is.True);

            Model model = parser.Parse(reader);
            Assert.That(model, Is.Not.Null);

            Assert.That(model, Is.InstanceOf<StringModel>());

            Assert.That(reader.LineNumber, Is.EqualTo(4));
            Assert.That(reader.EndOfFile, Is.False);

            Assert.That(model.ToString(), Is.StringStarting("[DETAILS]"));
            Assert.That(model.ToString(), Is.Not.StringContaining("MODULE"));

            Assert.That(reader.PeekAhead(0), Is.EqualTo("MODULE:"));
        }

        [Test]
        public void ParseWithFollowingText()
        {
            string text = string.Join(lineFeed, detailsText);
            string completeText = text + lineFeed + "Following";

            Assert.That(completeText, Is.StringStarting(text));

            IReader reader = new StringReader(completeText);

            Assert.That(reader.LineNumber, Is.EqualTo(1));

            Model model = parser.Parse(reader);
            Assert.That(model, Is.Not.Null);

            Assert.That(model, Is.InstanceOf<StringModel>());

            Assert.That(reader.LineNumber, Is.EqualTo(4));
            Assert.That(reader.EndOfFile, Is.False);

            Assert.That(model.ToString(), Is.StringStarting("[DETAILS]"));
            Assert.That(model.ToString(), Is.Not.StringContaining("MODULE"));

            Assert.That(reader.PeekAhead(0), Is.EqualTo("MODULE:"));
        }

    }
}