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

            IReader reader = Reader.CreateStringReader(incomplete);

            Assert.That(reader.LineNumber, Is.EqualTo(1));

            Assert.That(parser.Matches(reader), Is.True);

            IModel model = parser.Parse(reader);
            Assert.That(model, Is.Not.Null);
            
            Assert.That(reader.LineNumber, Is.EqualTo(4));
            Assert.That(reader.EndOfFile, Is.True);
        }

        [Test]
        public void Parse()
        {
            string text = string.Join(lineFeed, detailsText);

            IReader reader = Reader.CreateStringReader(text);

            Assert.That(reader.LineNumber, Is.EqualTo(1));

            Assert.That(parser.Matches(reader), Is.True);

            IModel model = parser.Parse(reader);
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

            IReader reader = Reader.CreateStringReader(completeText);

            Assert.That(reader.LineNumber, Is.EqualTo(1));

            IModel model = parser.Parse(reader);
            Assert.That(model, Is.Not.Null);

            Assert.That(model, Is.InstanceOf<StringModel>());

            Assert.That(reader.LineNumber, Is.EqualTo(4));
            Assert.That(reader.EndOfFile, Is.False);

            Assert.That(model.ToString(), Is.StringStarting("[DETAILS]"));
            Assert.That(model.ToString(), Is.Not.StringContaining("MODULE"));

            Assert.That(reader.PeekAhead(0), Is.EqualTo("MODULE:"));
        }

        [Test]
        public void ParseAlternateFormat()
        {
            string text = string.Join(lineFeed, detailsText).Replace("DETAILS", "Details");

            Assert.That(text, Is.Not.StringContaining("DETAILS"));

            IReader reader = Reader.CreateStringReader(text);

            Assert.That(reader.LineNumber, Is.EqualTo(1));

            IModel model = parser.Parse(reader);
            Assert.That(model, Is.Not.Null);

            Assert.That(model, Is.InstanceOf<StringModel>());

            Assert.That(reader.LineNumber, Is.EqualTo(4));
            Assert.That(reader.EndOfFile, Is.False);

            Assert.That(model.ToString(), Is.StringStarting("[DETAILS]"));
            Assert.That(model.ToString(), Is.Not.StringContaining("MODULE"));

            Assert.That(reader.PeekAhead(0), Is.EqualTo("MODULE:"));
        }

        [Test]
        public void ParseEmptyData()
        {
            const string text = "DETAILS:\nMODULE:";

            IReader reader = Reader.CreateStringReader(text);

            Assert.That(reader.LineNumber, Is.EqualTo(1));

            IModel model = parser.Parse(reader);
            Assert.That(model, Is.Not.Null);

            Assert.That(model, Is.InstanceOf<StringModel>());

            Assert.That(reader.LineNumber, Is.EqualTo(2));
            Assert.That(reader.EndOfFile, Is.False);

            Assert.That(model.ToString(), Is.StringStarting("[DETAILS]"));
            Assert.That(model.ToString(), Is.Not.StringContaining("MODULE"));

            Assert.That(reader.PeekAhead(0), Is.EqualTo("MODULE:"));
        }

    }
}