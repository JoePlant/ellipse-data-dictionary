using System;
using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers.Models
{
    [TestFixture]
    public class ModuleParserUnitTests : TestFixture
    {
        private const string lineFeed = "\n";

        private readonly string[] detailsText = new []
            {
                "MODULE:",
                "   30DD"
            };

        private IModelParser parser;

        protected override void OnSetUp()
        {
            parser = new ModuleParser();
            base.OnSetUp();
        }

        [Test]
        public void ParseWithSingleLine()
        {
            string text = detailsText[0];

            Assert.That(text,Is.Not.StringContaining("30DD"));
            IReader reader = new StringReader(text);

            Assert.That(reader.LineNumber, Is.EqualTo(1));

            Assert.That(parser.Matches(reader), Is.False);

            Model model = parser.Parse(reader);
            Assert.That(model, Is.Null);
            
            Assert.That(reader.LineNumber, Is.EqualTo(1));
            Assert.That(reader.EndOfFile, Is.False);
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

            Assert.That(reader.LineNumber, Is.EqualTo(3));
            Assert.That(reader.EndOfFile, Is.True);

            Assert.That(model.ToString(), Is.StringStarting("[MODULE]"));
            Assert.That(model.ToString(), Is.StringContaining("30DD"));
        }

        [Test]
        public void ParseAlternate()
        {
            string text = string.Join(lineFeed, detailsText).Replace(":", "");

            IReader reader = new StringReader(text);

            Assert.That(reader.LineNumber, Is.EqualTo(1));

            Assert.That(parser.Matches(reader), Is.True);

            Model model = parser.Parse(reader);
            Assert.That(model, Is.Not.Null);

            Assert.That(model, Is.InstanceOf<StringModel>());

            Assert.That(reader.LineNumber, Is.EqualTo(3));
            Assert.That(reader.EndOfFile, Is.True);

            Assert.That(model.ToString(), Is.StringStarting("[MODULE]"));
            Assert.That(model.ToString(), Is.StringContaining("30DD"));
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

            Assert.That(reader.LineNumber, Is.EqualTo(3));
            Assert.That(reader.EndOfFile, Is.False);

            Assert.That(model.ToString(), Is.StringStarting("[MODULE]"));
            Assert.That(model.ToString(), Is.StringContaining("30DD"));

            Assert.That(reader.PeekAhead(0), Is.EqualTo("Following"));
        }

        [Test]
        public void SimpleLineWithExtra2Lines()
        {
            string text = string.Join(lineFeed, detailsText) + lineFeed + lineFeed + "Next";

            IReader reader = new StringReader(text);
            Assert.That(parser.Matches(reader), Is.True);
            Model model = parser.Parse(reader);
            Assert.That(model, Is.Not.Null);
            Assert.That(model, Is.InstanceOf<StringModel>());

            Assert.That(model.ToString(), Is.StringStarting("[MODULE]"));
            Assert.That(model.ToString(), Is.StringContaining("30DD"));

            Assert.That(reader.PeekNext(), Is.EqualTo("Next"));
        }

    }
}