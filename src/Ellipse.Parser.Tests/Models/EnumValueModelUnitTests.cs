using System;
using System.Collections.Generic;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Models
{
    [TestFixture]
    public class EnumValueModelUnitTests : TestFixture
    {
        [Test]
        public void ValueWithNoSpace()
        {
            IModel model = EnumValueModel.Factory("EnumValue", "VALUE", null);
            IDictionary<string, string> dictionary = model.GetModelParts();
            Assert.That(dictionary.ContainsKey("data"));
            Assert.That(dictionary.Keys.Count, Is.EqualTo(1));
        }

        [Test]
        public void NAME_VALUE_A()
        {
            IModel model = EnumValueModel.Factory("EnumValue", "NAME VALUE 'A'", null);
            IDictionary<string, string> dictionary = model.GetModelParts();
            Assert.That(dictionary.ContainsKey("name"));
            Assert.That(dictionary.ContainsKey("value"));
            Assert.That(dictionary.ContainsKey("text"));

            Assert.That(dictionary["name"], Is.EqualTo("NAME"), "name");
            Assert.That(dictionary["value"], Is.EqualTo("'A'"), "value");
            Assert.That(dictionary["text"], Is.EqualTo("NAME VALUE 'A'"), "text");
            Assert.That(dictionary.Keys.Count, Is.EqualTo(3));
        }

        [Test]
        public void NAME_VALUE_Space()
        {
            IModel model = EnumValueModel.Factory("EnumValue", "NAME VALUE ' '", null);
            IDictionary<string, string> dictionary = model.GetModelParts();
            Assert.That(dictionary.ContainsKey("name"));
            Assert.That(dictionary.ContainsKey("value"));
            Assert.That(dictionary.ContainsKey("text"));

            Assert.That(dictionary["name"], Is.EqualTo("NAME"), "name");
            Assert.That(dictionary["value"], Is.EqualTo("' '"), "value");
            Assert.That(dictionary["text"], Is.EqualTo("NAME VALUE ' '"), "text");
            Assert.That(dictionary.Keys.Count, Is.EqualTo(3));
        }

        [Test]
        public void NAME_VALUE_ASpace()
        {
            IModel model = EnumValueModel.Factory("EnumValue", "NAME VALUE 'A '", null);
            IDictionary<string, string> dictionary = model.GetModelParts();
            Assert.That(dictionary.ContainsKey("name"));
            Assert.That(dictionary.ContainsKey("value"));
            Assert.That(dictionary.ContainsKey("text"));

            Assert.That(dictionary["name"], Is.EqualTo("NAME"), "name");
            Assert.That(dictionary["value"], Is.EqualTo("'A '"), "value");
            Assert.That(dictionary["text"], Is.EqualTo("NAME VALUE 'A '"), "text");
            Assert.That(dictionary.Keys.Count, Is.EqualTo(3));
        }

        [Test]
        public void NAME_VALUE_A_B()
        {
            IModel model = EnumValueModel.Factory("EnumValue", "NAME VALUE 'A' 'B'", null);
            IDictionary<string, string> dictionary = model.GetModelParts();
            Assert.That(dictionary.ContainsKey("name"));
            Assert.That(dictionary.ContainsKey("values"));
            Assert.That(dictionary.ContainsKey("text"));

            Assert.That(dictionary["name"], Is.EqualTo("NAME"), "name");
            Assert.That(dictionary["values"], Is.EqualTo("'A' 'B'"), "value");
            Assert.That(dictionary["text"], Is.EqualTo("NAME VALUE 'A' 'B'"), "text");
            Assert.That(dictionary.Keys.Count, Is.EqualTo(3));
        }

        [Test]
        public void NAME_VALUE_1_THRU_9()
        {
            IModel model = EnumValueModel.Factory("EnumValue", "NAME VALUE 1 THRU 9", null);
            IDictionary<string, string> dictionary = model.GetModelParts();
            Assert.That(dictionary.ContainsKey("name"));
            Assert.That(dictionary.ContainsKey("values"));
            Assert.That(dictionary.ContainsKey("text"));

            Assert.That(dictionary["name"], Is.EqualTo("NAME"), "name");
            Assert.That(dictionary["values"], Is.EqualTo("1 THRU 9"), "values");
            Assert.That(dictionary["text"], Is.EqualTo("NAME VALUE 1 THRU 9"), "text");
            Assert.That(dictionary.Keys.Count, Is.EqualTo(3));
        }

        [Test]
        public void NAME_VALUE_1()
        {
            IModel model = EnumValueModel.Factory("EnumValue", "NAME VALUE 1", null);
            IDictionary<string, string> dictionary = model.GetModelParts();
            Assert.That(dictionary.ContainsKey("name"));
            Assert.That(dictionary.ContainsKey("value"));
            Assert.That(dictionary.ContainsKey("text"));

            Assert.That(dictionary["name"], Is.EqualTo("NAME"), "name");
            Assert.That(dictionary["value"], Is.EqualTo("1"), "value");
            Assert.That(dictionary["text"], Is.EqualTo("NAME VALUE 1"), "text");
            Assert.That(dictionary.Keys.Count, Is.EqualTo(3));
        }

        [Test]
        public void NAME_VALUE_1_2()
        {
            IModel model = EnumValueModel.Factory("EnumValue", "NAME VALUE 1 2", null);
            IDictionary<string, string> dictionary = model.GetModelParts();
            Assert.That(dictionary.ContainsKey("name"));
            Assert.That(dictionary.ContainsKey("values"));
            Assert.That(dictionary.ContainsKey("text"));

            Assert.That(dictionary["name"], Is.EqualTo("NAME"), "name");
            Assert.That(dictionary["values"], Is.EqualTo("1 2"), "values");
            Assert.That(dictionary["text"], Is.EqualTo("NAME VALUE 1 2"), "text");
            Assert.That(dictionary.Keys.Count, Is.EqualTo(3));
        }
    }
}