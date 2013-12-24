using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers.Models
{
    [TestFixture]
    public class TechnicalInformationParserUnitTests : TestFixture
    {
        private const string lineFeed = "\n";

        private const string name = "[TechnicalInformation]";
        private const string dataPhrase = "Random";

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
                "TECHNICAL INFORMATION:",
                "   STABILITY:",
                "      Relatively stable,will infrequently be updated after initial of",
                "      existing TSUB tables.",
                "   SIZING:",
                "      The number of existing TSUB tables line items are approx, 36000.",
                "   REFERENCE FREQUENCY:",
                "      Referenced frequently in the requisitioning and procurement",
                "      processess.",
                "   KEY GROWTH:",
                "      Random",
                "",
                "01  DDF136-RECORD."
            };

        private IModelParser parser;

        protected override void OnSetUp()
        {
            parser = new TechnicalInformationParser();
            base.OnSetUp();
        }

        [Test]
        public void ParseWithSingleLine()
        {
            string text = detailsText[0];

            Assert.That(text,Is.Not.StringContaining("TSUB"));
            IReader reader = Reader.CreateStringReader(text);

            Assert.That(reader.LineNumber, Is.EqualTo(1));

            Assert.That(parser.Matches(reader), Is.False);

            IModel model = parser.Parse(reader);
            Assert.That(model, Is.Null);
            
            Assert.That(reader.LineNumber, Is.EqualTo(1));
            Assert.That(reader.EndOfFile, Is.False);
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

            Assert.That(reader.LineNumber, Is.EqualTo(NumberOfLines));
            Assert.That(reader.EndOfFile, Is.False);
            Assert.That(reader.PeekNext(), Is.EqualTo(NextLine));

            Assert.That(model.ToString(), Is.StringStarting(name));
            Assert.That(model.ToString(), Is.StringContaining(dataPhrase));
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

            Assert.That(reader.LineNumber, Is.EqualTo(NumberOfLines));
            Assert.That(reader.EndOfFile, Is.False);

            Assert.That(model.ToString(), Is.StringStarting(name));
            Assert.That(model.ToString(), Is.StringContaining(dataPhrase));

            Assert.That(reader.PeekAhead(0), Is.EqualTo(NextLine));
        }
    }
}