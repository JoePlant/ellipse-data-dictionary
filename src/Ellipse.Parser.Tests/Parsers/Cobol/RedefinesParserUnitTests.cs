using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    [TestFixture]
    public class RedefinesParserUnitTests : ParserTestFixture<RedefinesParser>
    {
        [Test]
        public void SingleLine03()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.Redefines.SingleLine03);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Redefines", "DOS-PREF-RPT REDEFINES DOS-PREF-GRP", "[ 5] DOS Preferred Report"));
        }

        [Test]
        public void MultiLine05()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.Redefines.MultiLine05);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Redefines", "WH-TABLE-CODE REDEFINES TABLE-CODE", "[ 5] Warehouse Table File Code DATASET (MSF010) ERROR\n(0041)"));
        }

        [Test]
        public void MultiLine07()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.Redefines.MultiLine07);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Redefines", "MSF062-DATA-1-062-CC REDEFINES MSF062-DATA-1-062-AU", "[ 5] Reference data 1"));
        }

        [Test]
        public void SingleLine09()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.Redefines.SingleLine09);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Redefines", "ACCTYRMN REDEFINES CONTROL-REC-NO-9", "[ 7] Accounting year and month"));
        }

        [Test]
        public void SingleLine09Trimmed()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.Redefines.SingleLine09.Substring(2));
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Redefines", "ACCTYRMN REDEFINES CONTROL-REC-NO-9", "[ 7] Accounting year and month"));
        }

        [Test]
        public void MultiLine11()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.Redefines.MultiLine11);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Redefines", "MSF061-DATA-1-061-CB REDEFINES MSF061-DATA-1-061-1A", "[ 5] Reference data 1"));
        }


        [Test]
        public void MultiLine13()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.Redefines.MultiLine13);
            IDataParser parser = CreateDataParser(reader);
            AssertParsed(parser, new CobolModel("Redefines", "MSF062-PORT-ELE-RC REDEFINES MSF062-IREQ-ITEM-RC", "[ 46] Portion Number and Element Number"));
        }

        [Test]
        public void Level03CompositeA()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.Redefines.Level03CompositeA);
            IDataParser parser = CreateDataParser(reader, RedefinesParser.HierarchyParser(2));

            IModel model =
                Build.Redefines("DC0028-REC REDEFINES DO-REC",
                                "[ 11] District Cntrl Last Internal Invoice ST,ID1:DC,ID2:0028")
                     .With(
                         Build.Property("LST-INT-INV-NO", "[ 11] Last Internal Invoice Number Allocated DB")
                              .With(
                                  Build.DataType("LST-INT-INV-NO-9 PIC 9(9)",
                                                 "[ 11] Last Internal Invoice Number Allocated"))
                    )
                     .With(
                         Build.DataType("FILLER PIC X(255)")
                    )
                     .Model();
            AssertParsed(parser, model);
        }

        [Test]
        public void Level03CompositeB()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.Redefines.Level03CompositeB);
            IDataParser parser = CreateDataParser(reader, RedefinesParser.HierarchyParser(2));

            IModel model =
                Build.Redefines("DC0027-REC REDEFINES DO-REC",
                                "[ 11] District Cntrl Last Picking Slip ST,ID1:DC,ID2:0027")
                     .With(
                         Build.Property("LST-CON-PICK-NO", "[ 11] Last Consolidate Picking Slip Number DB")
                              .With(
                                  Build.DataType("LST-CON-PICK-NO-9 PIC 9(6)",
                                                 "[ 11] Last Consolidate Picking Slip Number"))
                    )
                     .With(
                         Build.DataType("FILLER PIC X(258)")
                    )
                     .Model();
            AssertParsed(parser, model);
        }

        [Test]
        public void Level07Composite()
        {
            Reader reader = Reader.CreateStringReader(ExampleStrings.Redefines.Level07Composite);
            IDataParser parser = CreateDataParser(reader, RedefinesParser.HierarchyParser(4));

            IModel model =
                Build.Redefines("W000-DATA-RT REDEFINES W000-DATA-GRP",
                                "[ 16] Work file data")
                     .With(
                         Build.Property("W000-REDEF-REC", "[ 16] Work file data")
                              .With(
                                  Build.DataType("W000-DATA PIC X(95) OCCURS 3 INDEXED BY W000-DATA-IDX",
                                                 "[ 16] Ninety five bytes 0f work data"))
                    )
                     .Model();
            AssertParsed(parser, model);
        }

        [Test]
        public void ClassCases()
        {
            AssertDoesNotParse(ExampleStrings.Class.AllCases());
        }

        [Test]
        public void DataTypeCases()
        {
            AssertDoesNotParse(ExampleStrings.DataType.AllCases());
        }

        [Test]
        public void EnumValueCases()
        {
            AssertDoesNotParse(ExampleStrings.EnumValue.AllCases());
        }

        [Test]
        public void PropertyCases()
        {
            AssertDoesNotParse(ExampleStrings.Property.SimpleCases());
        }
    }
}