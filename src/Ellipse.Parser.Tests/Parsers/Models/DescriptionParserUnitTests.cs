using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers.Models
{
    [TestFixture]
    public class DescriptionParserUnitTests : TestFixture
    {
        [Test]
        public void SimpleLine()
        {
            const string text = "Description     : Technical Substitution Header File";
            IModelParser parser = new DescriptionParser();

            IReader reader = new StringReader(text);
            Assert.That(parser.Matches(reader), Is.True);
            Model model = parser.Parse(reader);
            Assert.That(model, Is.Not.Null);
            Assert.That(model, Is.InstanceOf<StringModel>());

            Assert.That(model.ToString(), Is.EqualTo("[Description] Technical Substitution Header File"));
        }
    }
}