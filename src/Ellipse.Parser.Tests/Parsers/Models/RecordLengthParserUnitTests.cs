using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers.Models
{
    [TestFixture]
    public class RecordLengthParserUnitTests : TestFixture
    {
        [Test]
        public void SimpleLine()
        {
            const string text = "Record length   : 112 bytes";
            IModelParser parser = new RecordLengthParser();

            IReader reader = Reader.CreateStringReader(text);
            Assert.That(parser.Matches(reader), Is.True);
            IModel model = parser.Parse(reader);
            Assert.That(model, Is.Not.Null);
            Assert.That(model, Is.InstanceOf<StringModel>());

            Assert.That(model.ToString(), Is.EqualTo("[RecordLength] 112"));
        }
    }
}