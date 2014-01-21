using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Parsers.Lines;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers
{
    [TestFixture]
    public class ImpliedModelParserUnitTests : ParserTestFixture<EmptyParser>
    {
        private class TestParser : ImpliedModelParser
        {
            public TestParser() : base("Test", Line.Contains("TEST "), Data.IgnoreAfter("TEST ").ExcludeMarker().Trim(), Data.IgnoreBefore("TEST ").IncludeMarker().Trim())
            {
            }
        }

        [Test]
        public void Matches()
        {
            IImpliedModelParser modelParser = new TestParser();
            IModel model = new CobolModel("Parent", "NAME SHOULD SEPARATE TEST DATA", "This is test data");
            Assert.That(modelParser.Matches(model), Is.True);
        }

        [Test]
        public void Parses()
        {
            IImpliedModelParser modelParser = new TestParser();
            IModel model = new CobolModel("Parent", "NAME SHOULD SEPARATE TEST DATA", "This is test data");
            Assert.That(modelParser.Matches(model), Is.True);

            IModel child = modelParser.Parse(model);
            Assert.That(child, Is.Not.Null);
            Assert.That(child, Is.Not.SameAs(model));

            IModel expected = new HierarchyModel(new CobolModel("Parent", "NAME SHOULD SEPARATE", "This is test data"), new IModel[]
                {
                    new CobolModel("Test", "TEST DATA", "Implied")
                });

            AssertModelIsSame(child, expected, true);
        }

    }
}