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
        [Test]
        public void ParseMSF004_RECORD()
        {
            FileReader reader = new FileReader(@".\Resources\Cobol\MSF004-RECORD.rpt");
            IModelParser[] parsers = new IModelParser[] {
                    new ClassParser(), 
                    new PropertyParser(), 
             
                    new EnumValueParser(), 
                    new EnumParser(),
                    new DataTypeParser(), 
                    new CommentParser(),
                };
            IDataParser dataParser = new DataParser(reader, parsers);
            dataParser.Corrections = null;
            dataParser.OnMissingParser = (line) =>
            {
                throw new InvalidOperationException("No Parser for: " + reader);
            };
            dataParser.Parse();

            List<Model> expected = new List<Model>
                {
                    new CobolModel("Class", "MSF004-RECORD"),
                    
                    new CobolModel("Property", "KEY-004", "some comment"),
                    new CobolModel("DataType", "DSTRCT-CODE PIC X(4)", "some comment"),
                    new CobolModel("DataType", "FULL-PERIOD PIC X(6)", "some comment"),
                    
                    new CobolModel("Property", "END-DATE", "some comment"),
                    new CobolModel("DataType", "END-DATE-9 PIC 9(8)", "some comment"),
                    
                    new CobolModel("Property", "GAP-END-DATE", "some comment"),
                    new CobolModel("DataType", "GAP-END-DATE-9 PIC 9(8)", "some comment"),
                    
                    new CobolModel("Property", "TAX-PERIOD-CLOSED PIC X(1)", "some comment"),
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
            builder.AppendFormat("Elements: {0}\n", modelList.Count);
            int index = 0;
            foreach (Model model in modelList)
            {
                index++;
                builder.AppendFormat("{0}: {1}\n", index, model);
            }
            return builder.ToString();
        }
    }
}