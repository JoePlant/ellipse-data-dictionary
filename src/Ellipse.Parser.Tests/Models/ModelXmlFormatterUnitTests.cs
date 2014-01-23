using System.Xml;
using NUnit.Framework;
namespace Ellipse.DataDictionary.Models
{
    [TestFixture]
    public class ModelXmlFormatterUnitTests : TestFixture
    {
        [Test]
        public void StringModel()
        {
            ModelXmlFormatter formatter = new ModelXmlFormatter(new StringModel("Name", "Data"));

            AssertXmlIsSame(formatter.Render(), "<Name data='Data' path='1' />");
        }

        [Test]
        public void CobolModel()
        {
            ModelXmlFormatter formatter = new ModelXmlFormatter(new CobolModel("Name", "Data"));

            AssertXmlIsSame(formatter.Render(), "<Name data='Data' path='1' />");
        }

        [Test]
        public void CobolModelWithComment()
        {
            ModelXmlFormatter formatter = new ModelXmlFormatter(new CobolModel("Name", "Data", "Comment"));

            AssertXmlIsSame(formatter.Render(), "<Name data='Data' comment='Comment' path='1' />");
        }

        [Test]
        public void DataTypeModel()
        {
            ModelXmlFormatter formatter = new ModelXmlFormatter(Models.DataTypeModel.Factory("DataType", "NAME-01 PIC X(1)", null));

            AssertXmlIsSame(formatter.Render(), "<DataType name='NAME-01' type='X(1)' text='NAME-01 PIC X(1)' path='1' />");
        }


        [Test]
        public void IgnoreModel()
        {
            ModelXmlFormatter formatter = new ModelXmlFormatter(new IgnoreModel("This is some data"));

            AssertXmlIsSame(formatter.Render(), "<Ignore line='This is some data' path='1' />");
        }

        [Test]
        public void PageHeader()
        {
            ModelXmlFormatter formatter = new ModelXmlFormatter(new PageHeaderModel(new[] { "One", "Two", "Three" }));

            AssertXmlIsSame(formatter.Render(), "<Page lines='One\r\nTwo\r\nThree' path='1'/>");
        }

        [Test]
        public void ClassWithProperty()
        {
            IModel model = Build.Class("Class001").WithProperty("PROP001", "Comment").Model();
            ModelXmlFormatter formatter = new ModelXmlFormatter(model);

            AssertXmlIsSame(formatter.Render(), "<Class data='Class001'  path='1'>" +
                                                "  <Property name='PROP001' text='PROP001' comment='Comment' path='1.1'/>" +
                                                "</Class>");
        }

        [Test]
        public void ClassWith2Properties()
        {
            IModel model = Build.Class("Class001").WithProperty("PROP001", "Comment").WithProperty("PROP002", "Comment").Model();
            ModelXmlFormatter formatter = new ModelXmlFormatter(model);

            AssertXmlIsSame(formatter.Render(), "<Class data='Class001' path='1'>" +
                                                "  <Property name='PROP001' text='PROP001' comment='Comment' path='1.1' />" +
                                                "  <Property name='PROP002' text='PROP002' comment='Comment' path='1.2' />" +
                                                "</Class>");
        }

        [Test]
        public void ClassWith2DeepProperties()
        {
            IModel model = Build.Class("Class001")
                               .With(Build.Property("PROP001", "Comment")
                                          .WithProperty("PROP002", "Comment"))
                               .Model();
            ModelXmlFormatter formatter = new ModelXmlFormatter(model);

            AssertXmlIsSame(formatter.Render(), "<Class data='Class001'  path='1'>" +
                                                "  <Property name='PROP001' text='PROP001' comment='Comment' path='1.1'>" +
                                                "    <Property name='PROP002' text='PROP002' comment='Comment' path='1.1.1' />" +
                                                "  </Property>" +
                                                "</Class>");
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

            const string expected = "<Class data='Class001' path='1'>" +
                                    "  <Property name='PROP00A' text='PROP00A' comment='Comment' path='1.1'>" +
                                    "    <Property name='PROP0A1' text='PROP0A1' comment='Comment' path='1.1.1' />" +
                                    "    <Property name='PROP0A2' text='PROP0A2' comment='Comment' path='1.1.2' />" +
                                    "  </Property>" +
                                    "  <Property name='PROP00B' text='PROP00B' comment='Comment' path='1.2'>" +
                                    "    <Property name='PROP0B1' text='PROP0B1' comment='Comment' path='1.2.1' />" +
                                    "    <Property name='PROP0B2' text='PROP0B2' comment='Comment' path='1.2.2' />" +
                                    "  </Property>" +
                                    "</Class>";

            AssertXmlIsSame(formatter.Render(), expected);
        }

        private void AssertXmlIsSame(string actual, string expected)
        {
            XmlDocument actualDoc = new XmlDocument();
            actualDoc.LoadXml(actual);

            XmlDocument expectedDoc = new XmlDocument();
            expectedDoc.LoadXml(expected);
            Assert.That(actualDoc.OuterXml, Is.EqualTo(expectedDoc.OuterXml));
        }
    }
}