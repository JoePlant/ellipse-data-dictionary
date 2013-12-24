using NUnit.Framework;
namespace Ellipse.DataDictionary.Models
{
    [TestFixture]
    [Ignore("Under construction")]
    public class ModelXmlFormatterUnitTests : TestFixture
    {
        [Test]
        public void StringModel()
        {
            ModelXmlFormatter formatter = new ModelXmlFormatter(new StringModel("Name", "Data"));

            AssertXmlIsSame(formatter.Render(), "<String name='Name' data='Data' path='1' />");
        }

        [Test]
        public void CobolModel()
        {
            ModelXmlFormatter formatter = new ModelXmlFormatter(new CobolModel("Name", "Data"));

            AssertXmlIsSame(formatter.Render(), "<Cobol name='Name' data='Data' path='1' />");
        }

        [Test]
        public void CobolModelWithComment()
        {
            ModelXmlFormatter formatter = new ModelXmlFormatter(new CobolModel("Name", "Data", "Comment"));

            AssertXmlIsSame(formatter.Render(), "<Cobol name='Name' data='Data' comment='Comment' />");
        }

        [Test]
        public void IgnoreModel()
        {
            ModelXmlFormatter formatter = new ModelXmlFormatter(new IgnoreModel("This is some data"));

            AssertXmlIsSame(formatter.Render(), "<Ignore data='This is some data' />");
        }

        [Test]
        public void PageHeader()
        {
            ModelXmlFormatter formatter = new ModelXmlFormatter(new PageHeaderModel(new[] { "One", "Two", "Three" }));

            AssertXmlIsSame(formatter.Render(), "<Page lines='One\r\nTwo\r\nThree\r\n' />");
        }

        [Test]
        public void ClassWithProperty()
        {
            IModel model = Build.Class("Class001").WithProperty("PROP001", "Comment").Model();
            ModelXmlFormatter formatter = new ModelXmlFormatter(model);

            AssertXmlIsSame(formatter.Render(), "<Class data='Class001' path='1'><Property data='PROP001' comment='Comment' path='1.1'/></Class001>");
        }

        [Test]
        public void ClassWith2Properties()
        {
            IModel model = Build.Class("Class001").WithProperty("PROP001", "Comment").WithProperty("PROP002", "Comment").Model();
            ModelXmlFormatter formatter = new ModelXmlFormatter(model);

            AssertXmlIsSame(formatter.Render(), "[Class] Class001\r\n  [Property] PROP001 /*...*/\r\n  [Property] PROP002 /*...*/");
        }

        [Test]
        public void ClassWith2DeepProperties()
        {
            IModel model = Build.Class("Class001")
                               .With(Build.Property("PROP001", "Comment")
                                          .WithProperty("PROP002", "Comment"))
                               .Model();
            ModelXmlFormatter formatter = new ModelXmlFormatter(model);

            AssertXmlIsSame(formatter.Render(),
                            "[Class] Class001\r\n  [Property] PROP001 /*...*/\r\n    [Property] PROP002 /*...*/");
        }

        [Test]
        public void ClassWith2BranchesOfPropertiesWithPath()
        {
            IModel model = Build
                .Class("Class001").With(
                    Build.Property("PROP00A", "Comment").WithProperty("PROP0A1", "Comment").WithProperty("PROP0A2", "Comment")
                )
                .With(
                    Build.Property("PROP00B", "Comment").WithProperty("PROP0B1", "Comment").WithProperty("PROP0B2", "Comment")
                ).Model();
            ModelXmlFormatter formatter = new ModelXmlFormatter(model);

            const string expected = "1: [Class] Class001\r\n" +
                                    "1.1: [Property] PROP00A /*...*/\r\n" +
                                    "1.1.1: [Property] PROP0A1 /*...*/\r\n" +
                                    "1.1.2: [Property] PROP0A2 /*...*/\r\n" +
                                    "1.2: [Property] PROP00B /*...*/\r\n" +
                                    "1.2.1: [Property] PROP0B1 /*...*/\r\n" +
                                    "1.2.2: [Property] PROP0B2 /*...*/" +
                                    "";

            AssertXmlIsSame(formatter.Render(), expected);
        }

        private void AssertXmlIsSame(string actual, string expected)
        {
            Assert.That(actual, Is.EqualTo(expected));
        }
    }
}