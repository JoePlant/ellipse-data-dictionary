using System;
using System.Collections.Generic;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Models
{
    [TestFixture]
    public class RedefinesModelUnitTests : TestFixture
    {
        [Test]
        public void REDEFINES_WithNoSpace()
        {
            IModel model = RedefinesModel.Factory("Redefines", "REDEFINES", null);
            IDictionary<string, string> dictionary = model.GetModelParts();
            Assert.That(dictionary.ContainsKey("data"));
            Assert.That(dictionary.Keys.Count, Is.EqualTo(1));
        }

        [Test]
        public void NAME_REDEFINES_TARGET()
        {
            IModel model = RedefinesModel.Factory("Redefines", "NAME REDEFINES TARGET", null);
            IDictionary<string, string> dictionary = model.GetModelParts();
            Assert.That(dictionary.ContainsKey("name"));
            Assert.That(dictionary.ContainsKey("target"));
            Assert.That(dictionary.ContainsKey("text"));

            Assert.That(dictionary["name"], Is.EqualTo("NAME"), "name");
            Assert.That(dictionary["target"], Is.EqualTo("TARGET"), "target");
            Assert.That(dictionary["text"], Is.EqualTo("NAME REDEFINES TARGET"), "text");
            Assert.That(dictionary.Keys.Count, Is.EqualTo(3));
        }
    }
}