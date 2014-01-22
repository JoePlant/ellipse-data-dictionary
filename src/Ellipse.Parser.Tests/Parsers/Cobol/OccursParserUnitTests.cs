using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    [TestFixture]
    public class OccursParserUnitTests : ParserTestFixture<OccursParser>
    {
        [Test]
        public void MultiLine05()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.Occurs.MultiLine05);
            
            IDataParser parser = CreateDataParser(reader);

            AssertParsedUsingXml(parser, new CobolModel("Occurs", "ITEM-DDS135 OCCURS 60 INDEXED BY DDS135-1-IDX", "[ 86] Stock Item"));
        }

        [Test]
        public void MultiLine07()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.Occurs.MultiLine07);

            IDataParser parser = CreateDataParser(reader);

            AssertParsedUsingXml(parser, new CobolModel("Occurs", "LO-LEVELS OCCURS 5 INDEXED BY LO-LEVELS-IDX", "[ 179] Geographical location levels data"));
        }

        [Test]
        public void MultiLine09()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.Occurs.MultiLine09);

            IDataParser parser = CreateDataParser(reader);

            AssertParsedUsingXml(parser, new CobolModel("Occurs", "EX-CNT-DATA OCCURS 4 INDEXED BY EX-CNT-DATA-IDX", "[ 11] Expedite Count Data"));
        }

        [Test]
        public void ImpliedMultiLine05()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.Occurs.ImpliedMultiLine05);

            IDataParser parser = CreateDataParser(reader, OccursParser.HierarchyParser(3));

            IModel model =
                Build.Occurs("ERROR-CODE OCCURS 10 INDEXED BY ERROR-CODE-IDX",
                             "[ 137] ERROR CODE DB")
                     .With(
                         Build.DataType("PIC X(4)", "Implied")
                              .With(
                                  Build.EnumValue("ERROR VALUE '0001' THRU '9999' 'A000' THRU 'Z999'", "Error"))
                    )
                     .Model();

            AssertParsedUsingXml(parser, model);
        }

        [Test]
        public void ImpliedMultiLine07()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.Occurs.ImpliedMultiLine07);

            IDataParser parser = CreateDataParser(reader);

            IModel model =
                Build.Occurs("CURR-ACCUM-A OCCURS 10 INDEXED BY CURR-ACC-A-IDX",
                             "[ 183] Current Period Accumulation Identifiers DB")
                     .WithDataType("PIC X(1)", "Implied")
                     .Model();

            AssertParsedUsingXml(parser, model);
        }

        [Test]
        public void ImpliedMultiLine11()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.Occurs.ImpliedMultiLine11);

            IDataParser parser = CreateDataParser(reader);

            IModel model =
                Build.Occurs("STOCK-SECTION OCCURS 10 INDEXED BY STOCK-SECTION-IDX",
                             "[ 28] Internal Stock Sections (Positional) DB")
                     .WithDataType("PIC X(2)", "Implied")
                     .Model();

            AssertParsedUsingXml(parser, model);
        }

        /// <test>
        ///                 09  STOCK-SECTION-N.                        [  28] Group item for stock-section-n.
        ///                     11  STOCK-SECTION PIC X(2) OCCURS 10    [  28] Internal Stock Sections (Positional)     DB
        ///                                 INDEXED BY
        ///                                 STOCK-SECTION-IDX.
        /// </test>
        [Test]
        public void PropertyWithImpliedMultiline11()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.Occurs.PropertyWithImpliedMultiLine11);

            IDataParser parser = CreateDataParser(reader, PropertyParser.HierarchyParser(5));

            IModel model =
                Build.Property("STOCK-SECTION-N", "[ 28] Group item for stock-section-n.")
                     .With(
                         Build.Occurs("STOCK-SECTION OCCURS 10 INDEXED BY STOCK-SECTION-IDX",
                                      "[ 28] Internal Stock Sections (Positional) DB")
                              .WithDataType("PIC X(2)", "Implied")
                    )
                     .Model();

            AssertParsedUsingXml(parser, model);
        }

        [Test]
        public void ImpliedMultiLine11B()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.Occurs.ImpliedMultiLines11B);

            IDataParser parser = CreateDataParser(reader);

            IModel model =
                Build.Occurs("W000-DATA OCCURS 3 INDEXED BY W000-DATA-IDX",
                             "[ 16] Ninety five bytes 0f work data")
                     .WithDataType("PIC X(95)", "Implied")
                     .Model();
            AssertParsedUsingXml(parser, model);
        }

        

        [Test]
        public void ClassCases()
        {
            AssertDoesNotParse(ExampleStrings.Class.AllCases());
        }

        [Test]
        public void PropertyCases()
        {
            AssertDoesNotParse(ExampleStrings.Property.SimpleCases());
        }

        [Test]
        public void DataTypeCases()
        {
            AssertDoesNotParse(ExampleStrings.DataType.AllCases());
        }

        [Test]
        public void EnumDataTypeCases()
        {
            AssertDoesNotParse(ExampleStrings.EnumValue.AllCases());
        }

        [Test]
        public void RedefinesDataTypeCases()
        {
            AssertDoesNotParse(ExampleStrings.Redefines.AllCases());
        }

        [Test]
        public void SameInstance()
        {
            AssertSameParser(() => OccursParser.HierarchyParser(3));
        }
    }
}