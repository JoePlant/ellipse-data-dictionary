using System;
using Ellipse.DataDictionary.Parsers;
using Ellipse.DataDictionary.Parsers.Models;
using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary
{
    [TestFixture]
    public class DataParserUnitTests : TestFixture
    {
        [Test]
        public void SystemTests()
        {
            IReader reader = new FileReader(@".\Resources\DataDictionary\datadict.rpt");
            IModelParser[] parsers = new IModelParser[] {
                    new AccessInformationParser(),
                    new DescriptionParser(),
                    new DetailsParser(),
                    new ModifiedParser(),
                    new ModuleParser(),
                    new PageHeaderParser(),
                    new RecordLengthParser(),
                    new RecordParser(),
                    new TechnicalInformationParser(),
                    new CobolParser(), 
                    //new EndOfFileParser()
                };
            IDataParser dataParser = new DataParser(reader, parsers);
            dataParser.OnMissingParser = (line) => {
                                                       throw new InvalidOperationException("No Parser for: " + reader);
            };
            dataParser.Parse();
        }
    }
}