using System;
using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers.Models
{
    [TestFixture]
    public class AccessInformationParserUnitTests : TestFixture
    {
        private const string lineFeed = "\n";

        private string NextLine
        {
            get { return detailsText[detailsText.Length - 1]; }
        }

        private int NumberOfLines
        {
            get { return detailsText.Length; }
        }

        private readonly string[] detailsText = new[]
            {
                "ACCESS INFORMATION:",
                "   The file is keyed on a combination of:",
                "   .  TSUB Number",
                "   .  TSUB Status",
                "   .  Line Number",
                "   Alternate keys are provided for:",
                "   .  Stock Code",
                "   .  TSUB Number",
                "   .  TSUB Status",
                "   .  Line Number",
                "TECHNICAL INFORMATION:"
            };

        private IModelParser parser;

        protected override void OnSetUp()
        {
            parser = new AccessInformationParser();
            base.OnSetUp();
        }

        [Test]
        public void ParseWithSingleLine()
        {
            string text = detailsText[0];

            Assert.That(text,Is.Not.StringContaining("key"));
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

            Assert.That(reader.LineNumber, Is.EqualTo(NumberOfLines));
            Assert.That(reader.EndOfFile, Is.False);
            Assert.That(reader.PeekNext(), Is.EqualTo(NextLine));

            Assert.That(model.ToString(), Is.StringStarting("[AccessInformation]"));
            Assert.That(model.ToString(), Is.StringContaining("TSUB"));
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

            Assert.That(reader.LineNumber, Is.EqualTo(NumberOfLines));
            Assert.That(reader.EndOfFile, Is.False);

            Assert.That(model.ToString(), Is.StringStarting("[AccessInformation]"));
            Assert.That(model.ToString(), Is.StringContaining("TSUB"));

            Assert.That(reader.PeekAhead(0), Is.EqualTo(NextLine));
        }
    }
}