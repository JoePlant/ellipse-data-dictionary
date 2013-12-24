using System;
using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Parsers;
using Ellipse.DataDictionary.Parsers.Cobol;
using Ellipse.DataDictionary.Parsers.Models;
using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary.System
{
    [TestFixture]
    public class CobolHierarchyUnitTests : ParserTestFixture<EmptyParser>
    {

        [Test]
        public void ParseMSF003_RECORD()
        {
            FileReader reader = new FileReader(@".\Resources\Cobol\MSF003-RECORD.rpt");
            IDataParser dataParser = CreateDataParser(reader, CobolParser.CobolHierarchy());
            dataParser.Corrections = null;
            dataParser.OnMissingParser = line =>
                {
                    throw new InvalidOperationException("No Parser for: " + reader);
                };
            dataParser.Parse();

            IModel expected =
                Build.Class("MSF003-RECORD")
                     .With(
                         Build.Property("KEY-003", "comment")
                              .WithDataType("ATTR-NAME-LONG PIC X(64)", "comment")
                    )
                     .With(
                         Build.Property("AIX1-KEY-003", "comment")
                              .WithDataType("ATTR-NAME-SHRT PIC X(14)", "comment")
                    )
                     .WithDataType("AGE-METHOD PIC X(2)", "comment")
                     .Model();
               
            AssertParsed(dataParser, expected);
        }


        [Test]
        public void ParseMSF004_RECORD()
        {
            FileReader reader = new FileReader(@".\Resources\Cobol\MSF004-RECORD.rpt");
            IDataParser dataParser = CreateDataParser(reader, CobolParser.CobolHierarchy());
            dataParser.Corrections = null;
            dataParser.OnMissingParser = line =>
            {
                throw new InvalidOperationException("No Parser for: " + reader);
            };
            dataParser.Parse();

            IModel expected =
                Build.Class("MSF004-RECORD")
                     .With(
                         Build.Property("KEY-004", "comment")
                              .WithDataType("DSTRCT-CODE PIC X(4)", "comment")
                              .WithDataType("FULL-PERIOD PIC X(6)", "comment")
                    )
                     .With(
                         Build.Property("END-DATE", "comment")
                              .WithDataType("END-DATE-9 PIC 9(8)", "comment")
                    )
                     .With(
                         Build.Property("GAP-END-DATE", "comment")
                              .WithDataType("GAP-END-DATE-9 PIC 9(8)", "comment")
                    )
                     .WithDataType("TAX-PERIOD-CLOSED PIC X(1)", "comment")
                     .Model();

            AssertParsed(dataParser, expected);
        }

        [Test]
        public void ParseMSF005_RECORD()
        {
            FileReader reader = new FileReader(@".\Resources\Cobol\MSF005-RECORD.rpt");
            IDataParser dataParser = CreateDataParser(reader, CobolParser.CobolHierarchy());
            dataParser.Corrections = null;
            dataParser.OnMissingParser = line =>
            {
                throw new InvalidOperationException("No Parser for: " + reader);
            };
            dataParser.Parse();
            IModel expected =
                Build.Class("MSF005-RECORD")
                     .With(
                         Build.Property("KEY-005", "comment")
                              .WithDataType("DSTRCT-CODE PIC X(4)", "comment")
                              .WithDataType("USER-TYPE PIC X(2)", "comment")
                              .WithDataType("TLX-SEQ-NO PIC X(12)", "comment")
                              .With(
                                  Build.Property("LINE-NO", "comment")
                                       .WithDataType("LINE-NO-9 PIC 9(4)", "comment")
                             )
                    )
                     .WithDataType("TLX-TEXT PIC X(72)", "comment")
                     .Model();
           
            AssertParsed(dataParser, expected);
        }

        [Test]
        public void ParseMSF006_RECORD()
        {
            FileReader reader = new FileReader(@".\Resources\Cobol\MSF006-RECORD.rpt");
            IDataParser dataParser = CreateDataParser(reader, CobolParser.CobolHierarchy());
            dataParser.Corrections = null;
            dataParser.OnMissingParser = line =>
                {
                    throw new InvalidOperationException("No Parser for: " + reader);
                };
            dataParser.Parse();

            IModel expected = Build
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
                                  .With(
                                      Build.Property("CONTROL-NUMBER", "comment")
                                           .With(
                                               Build.Property("INT-DSTRCT", "comment")
                                                    .WithDataType("SUBLEDGER-TYPE PIC X(2)", "comment")
                                                    .WithDataType("FILLER PIC X(2)")
                                          )
                                 )
                        )
                         .With(
                             Build.DataType("INVENT-CAT PIC X(2)", "comment")
                        )
                )
                .With(
                    Build.Property("AIX1-KEY-006", "comment")
                         .WithDataType("DSTRCT-CODE-2 PIC X(4)", "comment")
                         .WithDataType("CONTROL-ID-2 PIC X(5)", "comment")
                         .WithDataType("INVENT-CAT-2 PIC X(2)", "comment")
                         .WithDataType("ACCOUNT-CODE-2 PIC X(24)", "comment")
                )
                .With(
                    Build.DataType("SL-ACCT-TY-IND PIC X(2)", "comment")
                         .WithValue("PO-NO-ITEM VALUE 'PO'", "comment")
                         .WithValue("INVOICE-NUM VALUE 'IN'", "comment")
                )
                .With(
                    Build.DataType("TARGT-ACCT-CDE PIC X(24)", "comment")
                )
                .With(
                    Build.DataType("TARGT-OFF-ACCT PIC X(24)", "comment")
                )
                .With(
                    Build.DataType("TARGT-AUTO-SW PIC X(1)", "comment")
                         .WithValue("TARGT-AUTOGEN VALUE 'Y'", "comment")
                         .WithValue("TARGT-NO-AUTOGEN VALUE 'N'", "comment")
                )
                .With(
                    Build.DataType("TARGT-OFF-SW PIC X(1)", "comment")
                         .WithValue("TARGT-OFF-DIST VALUE 'Y'", "comment")
                         .WithValue("TARGT-NO-OFF-DIST VALUE 'N'", "comment")
                )
                .Model();

            AssertParsed(dataParser, expected);
        }

        [Test]
        public void ParseMSW000_RECORD()
        {
            FileReader reader = new FileReader(@".\Resources\Cobol\MSW000-RECORD.rpt");
            IDataParser dataParser = CreateDataParser(reader, CobolParser.CobolHierarchy(), new IgnoreParser());
            dataParser.Corrections = null;
            dataParser.OnMissingParser = line =>
                {
                    throw new InvalidOperationException("No Parser for: " + reader);
                };
            dataParser.Parse();
            IModel expected =
                Build.Class("MSW000-RECORD")
                     .With(
                         Build.Property("KEY-W000", "comment")
                              .WithDataType("USERNO PIC X(4)", "comment")
                              .WithDataType("W000-KEY PIC X(10)", "comment")
                    )
                     .With(
                         Build.DataType("W000-FILE-FLAGS PIC X(1)", "comment")
                    )
                     .With(
                         Build.Property("W000-FILE-DATA", "comment")
                              .With(
                                  Build.Property("W000-DATA-REC", "comment")
                                       .With(
                                           Build.Property("W000-DATA-GRP", "comment")
                                                .WithDataType("W000-DATA-1 PIC X(95)", "comment")
                                                .WithDataType("W000-DATA-2 PIC X(95)", "comment")
                                                .WithDataType("W000-DATA-3 PIC X(95)", "comment")
                                      )
                                       .With(
                                           Build.Redefines("W000-DATA-RT REDEFINES W000-DATA-GRP", "comment")
                                                .With(
                                                    Build.Property("W000-REDEF-REC", "comment")
                                                         .WithDataType(
                                                             "W000-DATA PIC X(95) OCCURS 3 INDEXED BY W000-DATA-IDX",
                                                             "comment")
                                               )
                                      )
                             )
                    )
                     .WithDataType("FILLER PIC X(4)")
                     .Model();

            AssertParsed(dataParser, expected);
        }

    }
}