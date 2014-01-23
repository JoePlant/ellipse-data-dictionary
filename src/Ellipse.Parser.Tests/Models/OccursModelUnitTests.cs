using System.Collections.Generic;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Models
{
    [TestFixture]
    public class OccursModelUnitTests : TestFixture
    {
        [Test]
        public void NAME_OCCURS_12_INDEXED_BY_NAME_IDX()
        {
            IModel model = OccursModel.Factory("Occurs", "NAME OCCURS 12 INDEXED BY NAME-IDX", null);
            IDictionary<string, string> dictionary = model.GetModelParts();
            Assert.That(dictionary.ContainsKey("name"));
            Assert.That(dictionary.ContainsKey("occurs"));
            Assert.That(dictionary.ContainsKey("indexed-by"));
            Assert.That(dictionary.ContainsKey("text"));

            Assert.That(dictionary["name"], Is.EqualTo("NAME"), "name");
            Assert.That(dictionary["occurs"], Is.EqualTo("12"), "occurs");
            Assert.That(dictionary["indexed-by"], Is.EqualTo("NAME-IDX"), "indexed-by");
            Assert.That(dictionary["text"], Is.EqualTo("NAME OCCURS 12 INDEXED BY NAME-IDX"), "text");
            Assert.That(dictionary.Keys.Count, Is.EqualTo(4));
        }
    }
}