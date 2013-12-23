using NUnit.Framework;
namespace Ellipse.DataDictionary.Models
{
    [TestFixture]
    public class ModelFormatterUnitTests : TestFixture
    {
        [Test]
        public void Model()
        {
            ModelFormatter formatter = new ModelFormatter(new Model());

            Assert.That(formatter.Render(), Is.EqualTo("[Model]"));
        }

        [Test]
        public void ModelWithPath()
        {
            ModelFormatter formatter = new ModelFormatter(new Model()) {IncludeModelPaths = true};
            Assert.That(formatter.Render(), Is.EqualTo("1: [Model]"));
        }

        [Test]
        public void StringModel()
        {
            ModelFormatter formatter = new ModelFormatter(new StringModel("Name", "Data"));

            Assert.That(formatter.Render(), Is.EqualTo("[Name] Data"));
        }

        [Test]
        public void StringModelWithPath()
        {
            ModelFormatter formatter = new ModelFormatter(new StringModel("Name", "Data")) { IncludeModelPaths = true };

            Assert.That(formatter.Render(), Is.EqualTo("1: [Name] Data"));
        }

        [Test]
        public void CobolModel()
        {
            ModelFormatter formatter = new ModelFormatter(new CobolModel("Name", "Data"));

            Assert.That(formatter.Render(), Is.EqualTo("[Name] Data"));
        }

        [Test]
        public void CobolModelWithPath()
        {
            ModelFormatter formatter = new ModelFormatter(new CobolModel("Name", "Data")) { IncludeModelPaths = true };

            Assert.That(formatter.Render(), Is.EqualTo("1: [Name] Data"));
        }

        [Test]
        public void CobolModelWithComment()
        {
            ModelFormatter formatter = new ModelFormatter(new CobolModel("Name", "Data", "Comment"));

            Assert.That(formatter.Render(), Is.EqualTo("[Name] Data /*...*/"));
        }

        [Test]
        public void IgnoreModel()
        {
            ModelFormatter formatter = new ModelFormatter(new IgnoreModel("This is some data"));

            Assert.That(formatter.Render(), Is.EqualTo("[Ignore] This is some data"));
        }

        [Test]
        public void PageHeader()
        {
            ModelFormatter formatter = new ModelFormatter(new PageHeaderModel( new [] {"One", "Two", "Three"}));

            Assert.That(formatter.Render(), Is.EqualTo("[Page] One\r\n[Page] Two\r\n[Page] Three\r\n"));
        }

        [Test]
        public void ClassWithProperty()
        {
            Model model = Build.Class("Class001").WithProperty("PROP001", "Comment").Model();
            ModelFormatter formatter = new ModelFormatter(model);

            Assert.That(formatter.Render(), Is.EqualTo("[Class] Class001\r\n  [Property] PROP001 /*...*/"));
        }

        [Test]
        public void ClassWithPropertyWithPath()
        {
            Model model = Build.Class("Class001").WithProperty("PROP001", "Comment").Model();
            ModelFormatter formatter = new ModelFormatter(model) {IncludeModelPaths = true};
            
            Assert.That(formatter.Render(), Is.EqualTo("1: [Class] Class001\r\n1.1: [Property] PROP001 /*...*/"));
        }

        [Test]
        public void ClassWith2Properties()
        {
            Model model = Build.Class("Class001").WithProperty("PROP001", "Comment").WithProperty("PROP002", "Comment").Model();
            ModelFormatter formatter = new ModelFormatter(model);

            Assert.That(formatter.Render(), Is.EqualTo("[Class] Class001\r\n  [Property] PROP001 /*...*/\r\n  [Property] PROP002 /*...*/"));
        }

        [Test]
        public void ClassWith2PropertiesWithPath()
        {
            Model model = Build.Class("Class001").WithProperty("PROP001", "Comment").WithProperty("PROP002", "Comment").Model();
            ModelFormatter formatter = new ModelFormatter(model){IncludeModelPaths = true};

            Assert.That(formatter.Render(), Is.EqualTo("1: [Class] Class001\r\n1.1: [Property] PROP001 /*...*/\r\n1.2: [Property] PROP002 /*...*/"));
        }

        [Test]
        public void ClassWith2DeepProperties()
        {
            Model model = Build.Class("Class001")
                               .With(Build.Property("PROP001", "Comment")
                                          .WithProperty("PROP002", "Comment"))
                               .Model();
            ModelFormatter formatter = new ModelFormatter(model);

            Assert.That(formatter.Render(), Is.EqualTo("[Class] Class001\r\n  [Property] PROP001 /*...*/\r\n    [Property] PROP002 /*...*/"));
        }

        [Test]
        public void ClassWith2DeepPropertiesWithPath()
        {
            Model model = Build.Class("Class001")
                               .With(Build.Property("PROP001", "Comment")
                                          .WithProperty("PROP002", "Comment"))
                               .Model();
            ModelFormatter formatter = new ModelFormatter(model){IncludeModelPaths = true};

            Assert.That(formatter.Render(), Is.EqualTo("1: [Class] Class001\r\n1.1: [Property] PROP001 /*...*/\r\n1.1.1: [Property] PROP002 /*...*/"));
        }

        [Test]
        public void ClassWith2BranchesOfProperties()
        {
            Model model = Build
                .Class("Class001").With(
                    Build.Property("PROP00A", "Comment").WithProperty("PROP0A1", "Comment").WithProperty("PROP0A2", "Comment")
                )
                .With(
                    Build.Property("PROP00B", "Comment").WithProperty("PROP0B1", "Comment").WithProperty("PROP0B2", "Comment")
                ).Model();
            ModelFormatter formatter = new ModelFormatter(model);

            const string expected = "[Class] Class001\r\n" +
                                    "  [Property] PROP00A /*...*/\r\n" +
                                    "    [Property] PROP0A1 /*...*/\r\n" +
                                    "    [Property] PROP0A2 /*...*/\r\n" +
                                    "  [Property] PROP00B /*...*/\r\n" +
                                    "    [Property] PROP0B1 /*...*/\r\n" +
                                    "    [Property] PROP0B2 /*...*/" +
                                    "";

            Assert.That(formatter.Render(), Is.EqualTo(expected));
        }

        [Test]
        public void ClassWith2BranchesOfPropertiesWithPath()
        {
            Model model = Build
                .Class("Class001").With(
                    Build.Property("PROP00A", "Comment").WithProperty("PROP0A1", "Comment").WithProperty("PROP0A2", "Comment")
                )
                .With(
                    Build.Property("PROP00B", "Comment").WithProperty("PROP0B1", "Comment").WithProperty("PROP0B2", "Comment")
                ).Model();
            ModelFormatter formatter = new ModelFormatter(model){IncludeModelPaths = true};

            const string expected = "1: [Class] Class001\r\n" +
                                    "1.1: [Property] PROP00A /*...*/\r\n" +
                                    "1.1.1: [Property] PROP0A1 /*...*/\r\n" +
                                    "1.1.2: [Property] PROP0A2 /*...*/\r\n" +
                                    "1.2: [Property] PROP00B /*...*/\r\n" +
                                    "1.2.1: [Property] PROP0B1 /*...*/\r\n" +
                                    "1.2.2: [Property] PROP0B2 /*...*/" +
                                    "";

            Assert.That(formatter.Render(), Is.EqualTo(expected));
        }
    }
}