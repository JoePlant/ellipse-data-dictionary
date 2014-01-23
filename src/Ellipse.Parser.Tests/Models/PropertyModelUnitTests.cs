using System.Collections.Generic;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Models
{
    [TestFixture]
    public class PropertyModelUnitTests : TestFixture
    {
        [Test]
        public void Property_NAME()
        {
            IModel model = PropertyModel.Factory("Property", "NAME", null);
            IDictionary<string, string> dictionary = model.GetModelParts();
            Assert.That(dictionary.ContainsKey("name"));
            Assert.That(dictionary.ContainsKey("text"));

            Assert.That(dictionary["name"], Is.EqualTo("NAME"), "name");
            Assert.That(dictionary["text"], Is.EqualTo("NAME"), "text");
            Assert.That(dictionary.Keys.Count, Is.EqualTo(2));
        }
    }
}