using System;
using System.Collections.Generic;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Models
{
    [TestFixture]
    public class DataTypeModelUnitTests : TestFixture
    {
        [Test]
        public void DataWithNoSpace()
        {
            CobolModel model = DataTypeModel.Factory("DataType", "PIC", null);
            IDictionary<string, string> dictionary = model.GetModelParts();
            Assert.That(dictionary.ContainsKey("data"));
            Assert.That(dictionary.Keys.Count, Is.EqualTo(1));
        }

        [Test]
        public void Name_PIC_X1()
        {
            CobolModel model = DataTypeModel.Factory("DataType", "NAME PIC X(1)", null);
            IDictionary<string, string> dictionary = model.GetModelParts();
            Assert.That(dictionary.ContainsKey("name"));
            Assert.That(dictionary.ContainsKey("data"));
            Assert.That(dictionary.ContainsKey("text"));

            Assert.That(dictionary["name"], Is.EqualTo("NAME"), "name");
            Assert.That(dictionary["data"], Is.EqualTo("X(1)"), "data");
            Assert.That(dictionary["text"], Is.EqualTo("NAME PIC X(1)"), "text");
            Assert.That(dictionary.Keys.Count, Is.EqualTo(3));
        }

        [Test]
        public void Implied_PIC_X1()
        {
            CobolModel model = DataTypeModel.Factory("DataType", "PIC X(1)", null);
            IDictionary<string, string> dictionary = model.GetModelParts();
            Assert.That(dictionary.ContainsKey("name"));
            Assert.That(dictionary.ContainsKey("data"));
            Assert.That(dictionary.ContainsKey("text"));

            Assert.That(dictionary["name"], Is.EqualTo("{implied}"), "name");
            Assert.That(dictionary["data"], Is.EqualTo("X(1)"), "data");
            Assert.That(dictionary["text"], Is.EqualTo("PIC X(1)"), "text");
            Assert.That(dictionary.Keys.Count, Is.EqualTo(3));
        }
    }
}