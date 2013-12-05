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
            const string text = "Modified        : 26-May-95 15:32:46";
            IModelParser parser = new ModifiedParser();

            IReader reader = new StringReader(text);
            Assert.That(parser.Matches(reader), Is.True);
            Model model = parser.Parse(reader);
            Assert.That(model, Is.Not.Null);
            Assert.That(model, Is.InstanceOf<StringModel>());

            Assert.That(model.ToString(), Is.EqualTo("[Modified] 26-May-95 15:32:46"));
        }
    }
}