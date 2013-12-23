using System;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Models
{
    [TestFixture]
    public class HierarchyModelUnitTests : TestFixture
    {
        [Test]
        public void ModelToString()
        {
            HierarchyModel model = new HierarchyModel(new CobolModel("Class", "CLASS001"));
            Assert.That(model.ToString(), Is.EqualTo(model.Model.ToString()));

            Assert.That(model.ToString(), Is.StringContaining("Class"));
            Assert.That(model.ToString(), Is.StringContaining("CLASS001"));
        }

        [Test]
        public void GetModelRoot()
        {
            HierarchyModel model = new HierarchyModel(new CobolModel("Class", "CLASS001"));
            Model result = model.GetModel("");
            Assert.That(result, Is.EqualTo(model.Model));
        }

        [Test]
        public void GetModel1()
        {
            HierarchyModel model = new HierarchyModel(new CobolModel("Class", "CLASS001"));
            Model result = model.GetModel("1");
            Assert.That(result, Is.EqualTo(model.Model));
            Assert.That(model.GetModel("1.1"), Is.EqualTo(null));
            Assert.That(model.GetModel("1.2"), Is.EqualTo(null));
        }

        [Test]
        public void GetModel11()
        {
            HierarchyModel model = new HierarchyModel(new CobolModel("Class", "CLASS001"), new Model[] { new CobolModel("Property", "PROP001"), });
            Assert.That(model.GetModel("1"), Is.EqualTo(model.Model));
            Assert.That(model.GetModel("1.0"), Is.EqualTo(null));
            Assert.That(model.GetModel("1.1"), Is.EqualTo(model.ChildModels[0]));
            Assert.That(model.GetModel("1.2"), Is.EqualTo(null));
        }

        [Test]
        public void GetModel12()
        {
            HierarchyModel model = new HierarchyModel(new CobolModel("Class", "CLASS001"), new Model[] { new CobolModel("Property", "PROP001"), new CobolModel("Property", "PROP002"),  });
            Assert.That(model.GetModel("1"), Is.EqualTo(model.Model));
            Assert.That(model.GetModel("1.0"), Is.EqualTo(null));
            Assert.That(model.GetModel("1.1"), Is.EqualTo(model.ChildModels[0]));
            Assert.That(model.GetModel("1.2"), Is.EqualTo(model.ChildModels[1]));
            Assert.That(model.GetModel("1.3"), Is.Null);
            Assert.That(model.GetModel("2.1"), Is.Null);
        }


        [Test]
        public void GetModel121()
        {
            HierarchyModel model = new HierarchyModel(new CobolModel("Class", "CLASS001"),
                                                      new Model[]
                                                          {
                                                              new HierarchyModel(new CobolModel("Property", "PROP001"), new Model[]
                                                                  {
                                                                      new CobolModel("DataType", "DATA001"), 
                                                                  }),
                                                              new CobolModel("Property", "PROP002"),
                                                          });
            Assert.That(model.GetModel("1"), Is.EqualTo(model.Model));
            Assert.That(model.GetModel("1.0"), Is.EqualTo(null));
            Assert.That(model.GetModel("1.1"), Is.EqualTo(((HierarchyModel) model.ChildModels[0]).Model));
            Assert.That(model.GetModel("1.1.1"), Is.EqualTo(((HierarchyModel)model.ChildModels[0]).ChildModels[0]));
            Assert.That(model.GetModel("1.1.2"), Is.EqualTo(null));
            Assert.That(model.GetModel("1.2"), Is.EqualTo(model.ChildModels[1]));
            Assert.That(model.GetModel("1.2.2"), Is.EqualTo(null));
            Assert.That(model.GetModel("1.3"), Is.Null);
            Assert.That(model.GetModel("2.1"), Is.Null);
        }

    }
}