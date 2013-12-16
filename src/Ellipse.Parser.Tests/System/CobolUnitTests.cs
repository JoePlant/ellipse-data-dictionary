using System;
using System.Collections.Generic;
using System.Text;
using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Parsers;
using Ellipse.DataDictionary.Parsers.Cobol;
using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary.System
{
    [TestFixture]
    public class CobolUnitTests : TestFixture
    {
        private readonly IModelParser[] parsers = new IModelParser[] {
                    new ClassParser(), 
                    new PropertyParser(), 
             
                    new EnumValueParser(), 
                    new EnumParser(),
                    new DataTypeParser(), 
                    new CommentParser()
                };


        [Test]
        public void ParseMSF003_RECORD()
        {
            FileReader reader = new FileReader(@".\Resources\Cobol\MSF003-RECORD.rpt");
            IDataParser dataParser = new DataParser(reader, parsers);
            dataParser.Corrections = null;
            dataParser.OnMissingParser = line =>
            {
                throw new InvalidOperationException("No Parser for: " + reader);
            };
            dataParser.Parse();

            List<Model> expected = new List<Model>
                {
                    new CobolModel("Class", "MSF003-RECORD"),
                    
                    new CobolModel("Property", "KEY-003", "comment"),
                    new CobolModel("DataType", "ATTR-NAME-LONG PIC X(64)", "comment"),
                    
                    new CobolModel("Property", "AIX1-KEY-003", "comment"),
                    new CobolModel("DataType", "ATTR-NAME-SHRT PIC X(14)", "comment"),
                    
                    new CobolModel("DataType", "AGE-METHOD PIC X(2)", "comment"),
                };
            AssertModel(dataParser.Results, expected);
        }

        
        [Test]
        public void ParseMSF004_RECORD()
        {
            FileReader reader = new FileReader(@".\Resources\Cobol\MSF004-RECORD.rpt");
            IDataParser dataParser = new DataParser(reader, parsers);
            dataParser.Corrections = null;
            dataParser.OnMissingParser = line =>
            {
                throw new InvalidOperationException("No Parser for: " + reader);
            };
            dataParser.Parse();

            List<Model> expected = new List<Model>
                {
                    new CobolModel("Class", "MSF004-RECORD"),
                    
                    new CobolModel("Property", "KEY-004", "comment"),
                    new CobolModel("DataType", "DSTRCT-CODE PIC X(4)", "comment"),
                    new CobolModel("DataType", "FULL-PERIOD PIC X(6)", "comment"),
                    
                    new CobolModel("Property", "END-DATE", "comment"),
                    new CobolModel("DataType", "END-DATE-9 PIC 9(8)", "comment"),
                    
                    new CobolModel("Property", "GAP-END-DATE", "comment"),
                    new CobolModel("DataType", "GAP-END-DATE-9 PIC 9(8)", "comment"),
                    
                    new CobolModel("DataType", "TAX-PERIOD-CLOSED PIC X(1)", "comment"),
                };
            AssertModel(dataParser.Results, expected);
        }

        [Test]
        public void ParseMSF005_RECORD()
        {
            FileReader reader = new FileReader(@".\Resources\Cobol\MSF005-RECORD.rpt");
            IDataParser dataParser = new DataParser(reader, parsers);
            dataParser.Corrections = null;
            dataParser.OnMissingParser = line =>
            {
                throw new InvalidOperationException("No Parser for: " + reader);
            };
            dataParser.Parse();

            List<Model> expected = new List<Model>
                {
                    new CobolModel("Class", "MSF005-RECORD"),
                    
                    new CobolModel("Property", "KEY-005", "comment"),
                    new CobolModel("DataType", "DSTRCT-CODE PIC X(4)", "comment"),
                    new CobolModel("DataType", "USER-TYPE PIC X(2)", "comment"),
                    new CobolModel("DataType", "TLX-SEQ-NO PIC X(12)", "comment"),
                    
                    new CobolModel("Property", "LINE-NO", "comment"),
                    new CobolModel("DataType", "LINE-NO-9 PIC 9(4)", "comment"),
                    
                    new CobolModel("DataType", "TLX-TEXT PIC X(72)", "comment"),
                };
            AssertModel(dataParser.Results, expected);
        }

        [Test]
        public void ParseMSF006_RECORD()
        {
            FileReader reader = new FileReader(@".\Resources\Cobol\MSF006-RECORD.rpt");
            IDataParser dataParser = new DataParser(reader, parsers);
            dataParser.Corrections = null;
            dataParser.OnMissingParser = line =>
                {
                    throw new InvalidOperationException("No Parser for: " + reader);
                };
            dataParser.Parse();

            List<Model> expected = new List<Model>
                {
                    new CobolModel("Class", "MSF006-RECORD"),

                    new CobolModel("Property", "KEY-006", "comment"),
                    new CobolModel("DataType", "DSTRCT-CODE PIC X(4)", "comment"),
                    new CobolModel("DataType", "ACCOUNT-CODE PIC X(24)", "comment"),

                    new CobolModel("Property", "CONTROL-ID", "comment"),
                    new CobolModel("DataType", "CONTROL-TYPE PIC X(1)", "comment"),
                    new CobolModel("EnumValue", "MIMS-CONTROL VALUE 'M'", "comment"),
                    new CobolModel("EnumValue", "INTER-DSTRCT-CTL VALUE 'I'", "comment"),
                    new CobolModel("EnumValue", "SUBLEDGER-CTL VALUE 'S'", "comment"),

                    new CobolModel("EnumValue", "TABLE-DSTRCT-CTL VALUE 'T'", "comment"),

                    new CobolModel("Property", "CONTROL-NUMBER", "comment"),
                    new CobolModel("Property", "INT-DSTRCT", "comment"),
                    new CobolModel("DataType", "SUBLEDGER-TYPE PIC X(2)", "comment"),
                    new CobolModel("DataType", "FILLER PIC X(2)"),
                    new CobolModel("DataType", "INVENT-CAT PIC X(2)", "comment"),

                    new CobolModel("Property", "AIX1-KEY-006", "comment"),
                    new CobolModel("DataType", "DSTRCT-CODE-2 PIC X(4)", "comment"),
                    new CobolModel("DataType", "CONTROL-ID-2 PIC X(5)", "comment"),
                    new CobolModel("DataType", "INVENT-CAT-2 PIC X(2)", "comment"),
                    new CobolModel("DataType", "ACCOUNT-CODE-2 PIC X(24)", "comment"),

                    new CobolModel("DataType", "SL-ACCT-TY-IND PIC X(2)", "comment"),
                    new CobolModel("EnumValue", "PO-NO-ITEM VALUE 'PO'", "comment"),
                    new CobolModel("EnumValue", "INVOICE-NUM VALUE 'IN'", "comment"),

                     new CobolModel("DataType", "TARGT-ACCT-CDE PIC X(24)", "comment"),
                    new CobolModel("DataType", "TARGT-OFF-ACCT PIC X(24)", "comment"),

                    new CobolModel("DataType", "TARGT-AUTO-SW PIC X(1)", "comment"),
                    new CobolModel("EnumValue", "TARGT-AUTOGEN VALUE 'Y'", "comment"),
                    new CobolModel("EnumValue", "TARGT-NO-AUTOGEN VALUE 'N'", "comment"),

                    new CobolModel("DataType", "TARGT-OFF-SW PIC X(1)", "comment"),
                    new CobolModel("EnumValue", "TARGT-OFF-DIST VALUE 'Y'", "comment"),
                    new CobolModel("EnumValue", "TARGT-NO-OFF-DIST VALUE 'N'", "comment"),
                };
            AssertModel(dataParser.Results, expected);
        }


        private void AssertModel(IList<Model> results, IList<Model> expected)
        {
            string actual = BuildStringModel(results);
            string expect = BuildStringModel(expected);

            Assert.That(actual, Is.EqualTo(expect));
        }

        private static string BuildStringModel(IList<Model> modelList)
        {
            StringBuilder builder = new StringBuilder();
            int index = 0;
            foreach (Model model in modelList)
            {
                index++;
                builder.AppendFormat("{0}: {1}\n", index, model);
            }

            builder.AppendFormat("Elements: {0}", modelList.Count);

            return builder.ToString();
        }
    }
}