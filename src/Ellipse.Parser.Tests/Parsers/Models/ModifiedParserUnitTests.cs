using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers.Models
{
    [TestFixture]
    public class ModifiedParserUnitTests : TestFixture
    {
        [Test]
        public void SimpleLine()
        {
            const string text = "Modified        : 26-May-95 15:32:46\nNext";
            IModelParser parser = new ModifiedParser();

            IReader reader = Reader.CreateStringReader(text);
            Assert.That(parser.Matches(reader), Is.True);
            IModel model =  parser.Parse(reader);
            Assert.That(model, Is.Not.Null);
            Assert.That(model, Is.InstanceOf<StringModel>());

            Assert.That(model.ToString(), Is.EqualTo("[Modified] 26-May-95 15:32:46"));

            Assert.That(reader.PeekNext(), Is.EqualTo("Next"));
        }

        [Test]
        public void SimpleLineWithExtraLine()
        {
            const string text = "Modified        : 26-May-95 15:32:46\n\nNext";
            IModelParser parser = new ModifiedParser();

            IReader reader = Reader.CreateStringReader(text);
            Assert.That(parser.Matches(reader), Is.True);
            IModel model =  parser.Parse(reader);
            Assert.That(model, Is.Not.Null);
            Assert.That(model, Is.InstanceOf<StringModel>());

            Assert.That(model.ToString(), Is.EqualTo("[Modified] 26-May-95 15:32:46"));
            Assert.That(reader.PeekNext(), Is.EqualTo("Next"));
        }

        [Test]
        public void SimpleLineWith2ExtraLines()
        {
            const string text = "Modified        : 26-May-95 15:32:46\n\n\nNext";
            IModelParser parser = new ModifiedParser();

            IReader reader = Reader.CreateStringReader(text);
            Assert.That(parser.Matches(reader), Is.True);
            IModel model = parser.Parse(reader);
            Assert.That(model, Is.Not.Null);
            Assert.That(model, Is.InstanceOf<StringModel>());

            Assert.That(model.ToString(), Is.EqualTo("[Modified] 26-May-95 15:32:46"));
            Assert.That(reader.PeekNext(), Is.EqualTo("Next"));
        }

    }
}