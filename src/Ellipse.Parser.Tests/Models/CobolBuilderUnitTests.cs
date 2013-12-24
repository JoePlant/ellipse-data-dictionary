using System;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Models
{
    [TestFixture]
    public class CobolBuilderUnitTests : TestFixture
    {
        [Test]
        public void BuildClass()
        {
            IModel model = Build.Class("CLASS001").Model();

            AssertCobolModel(model, "1", "Class", "CLASS001");
        }

        [Test]
        public void BuildClassWithProperty()
        {
            IModel model = Build
                .Class("MSF001")
                .With(
                    Build.Property("PROP001")
                )
                .Model();

            AssertCobolModel(model, "1", "Class", "MSF001");
            AssertCobolModel(model, "1.1", "Property", "PROP001");
        }

        [Test]
        public void BuildClassWithPropertyAndDataType()
        {
            IModel model = Build
                .Class("MSF001")
                .With(
                    Build.Property("PROP001")
                    .With(
                        Build.DataType("DATATYPE001")
                    )
                )
                .Model();

            AssertCobolModel(model, "1", "Class", "MSF001");
            AssertCobolModel(model, "1.1", "Property", "PROP001");
            AssertCobolModel(model, "1.1.1", "DataType", "DATATYPE001");
        }

        [Test]
        public void SameModelConstructed()
        {
            IModel simpleModel = Build
                .Class("MSF006-RECORD")
                .With(
                    Build.Property("KEY-006", "comment")
                         .WithDataType("DSTRCT-CODE PIC X(4)", "comment")
                         .WithDataType("ACCOUNT-CODE PIC X(24)", "comment")
                         .With(
                             Build.Property("CONTROL-ID", "comment")
                                  .With(
                                      Build.DataType("CONTROL-TYPE PIC X(1)", "comment")
                                           .WithValue("MIMS-CONTROL VALUE 'M'", "comment")
                                           .WithValue("INTER-DSTRCT-CTL VALUE 'I'", "comment")
                                           .WithValue("SUBLEDGER-CTL VALUE 'S'", "comment")
                                           .WithValue("TABLE-DSTRCT-CTL VALUE 'T'", "comment")
                                 )
                        )

                ).Model();

            IModel verboseModel = Build
                .Class("MSF006-RECORD")
                .With(
                    Build.Property("KEY-006", "comment")
                         .With(Build.DataType("DSTRCT-CODE PIC X(4)", "comment"))
                         .With(Build.DataType("ACCOUNT-CODE PIC X(24)", "comment"))
                         .With(
                             Build.Property("CONTROL-ID", "comment")
                                  .With(
                                      Build.DataType("CONTROL-TYPE PIC X(1)", "comment")
                                           .WithValue("MIMS-CONTROL VALUE 'M'", "comment")
                                           .WithValue("INTER-DSTRCT-CTL VALUE 'I'", "comment")
                                           .WithValue("SUBLEDGER-CTL VALUE 'S'", "comment")
                                           .WithValue("TABLE-DSTRCT-CTL VALUE 'T'", "comment")
                                 )
                        )

                ).Model();

            string simple = new ModelFormatter(simpleModel) {IncludeModelPaths = true}.Render();
            string verbose = new ModelFormatter(verboseModel) { IncludeModelPaths = true }.Render();

            Assert.That(simple, Is.EqualTo(verbose));
        }

        private void AssertCobolModel(IModel model, string path, string name, string value)
        {
            Assert.That(model, Is.Not.Null, "Model is null");
            IModel iModel = model as IModel;
            Assert.That(iModel, Is.Not.Null, "Not iModel");

            if (iModel != null)
            {
                IModel selected = iModel.GetModel(path);
                Assert.That(selected, Is.Not.Null, "Selected: {0}", path);
                CobolModel cobolModel = selected as CobolModel;

                Assert.That(cobolModel, Is.Not.Null, "Selected: {0} is {1}", path, cobolModel);
                if (cobolModel != null)
                {
                    Assert.That(cobolModel.Name, Is.EqualTo(name), path);
                    Assert.That(cobolModel.Data, Is.EqualTo(value), path);
                }
            }

        }
    }
}