using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Parsers.Lines;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers
{
    [TestFixture]
    public class HierarchyImpliedModelParserUnitTests : ParserTestFixture<EmptyParser>
    {
        private class TestParser : ImpliedModelParser
        {
            public TestParser()
                : base(
                "Test", 
                Line.Contains(" TEST "), 
                Data.SplitOn(" ").Find("TEST").Ignore(0).AndFollowing().Join(" "),
                Data.SplitOn(" ").Find("TEST").Select(0).AndFollowing().Join(" "),
                CobolModel.Factory,
                CobolModel.Factory
                )
            {
            }
        }

        private class DataParser : ImpliedModelParser
        {
            public DataParser()
                : base(
                "Data",
                Line.Contains("DATA"),
                Data.SplitOn(" ").Find("DATA").Ignore(0, 1).Join(" "),
                Data.SplitOn(" ").Find("DATA").Select(1).Join(" "),
                CobolModel.Factory,
                CobolModel.Factory
                )
            {
            }
        }

        [Test]
        public void SingleTestParser()
        {
            IImpliedModelParser modelParser = new TestParser();
            IModel model = new CobolModel("Parent", "NAME SHOULD SEPARATE TEST DATA 1", "This is test data");
            Assert.That(modelParser.Matches(model), Is.True);

            IModel child = modelParser.Parse(model);
            Assert.That(child, Is.Not.Null);
            Assert.That(child, Is.Not.SameAs(model));

            IModel expected = new HierarchyModel(new CobolModel("Parent", "NAME SHOULD SEPARATE", "This is test data"), new IModel[]
                {
                    new CobolModel("Test", "TEST DATA 1", "Implied")
                });

            AssertModelIsSame(child, expected, true);
        }

        [Test]
        public void SingleDataParser()
        {
            IImpliedModelParser modelParser = new DataParser();
            IModel model = new CobolModel("Parent", "NAME SHOULD SEPARATE TEST DATA 1", "This is test data");
            Assert.That(modelParser.Matches(model), Is.True);

            IModel child = modelParser.Parse(model);
            Assert.That(child, Is.Not.Null);
            Assert.That(child, Is.Not.SameAs(model));

            IModel expected = new HierarchyModel(new CobolModel("Parent", "NAME SHOULD SEPARATE TEST", "This is test data"), new IModel[]
                {
                    new CobolModel("Data", "1", "Implied")
                });

            AssertModelIsSame(child, expected, true);
        }

        [Test]
        public void BothTestAndDataParser()
        {
            IImpliedModelParser modelParser = new HierarchicalImpliedModelParser(new TestParser(), new DataParser());
            IModel model = new CobolModel("Parent", "NAME SHOULD SEPARATE TEST DATA 1", "This is test data");
            Assert.That(modelParser.Matches(model), Is.True);

            IModel child = modelParser.Parse(model);
            Assert.That(child, Is.Not.Null);
            Assert.That(child, Is.Not.SameAs(model));

            IModel expected = new HierarchyModel(new CobolModel("Parent", "NAME SHOULD SEPARATE", "This is test data"), new IModel[]
                {
                    new HierarchyModel( new CobolModel("Test", "TEST", "Implied"), new IModel[]
                        {
                            new CobolModel("Data", "1", "Implied"), 
                        })
                });

            AssertModelIsSame(child, expected, true);
        }

    }
}