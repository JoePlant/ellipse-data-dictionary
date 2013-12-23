using System;
using System.Collections.Generic;
using Ellipse.DataDictionary.Parsers;
using Ellipse.DataDictionary.Parsers.Cobol;
using Ellipse.DataDictionary.Parsers.Models;
using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary
{
    [TestFixture]
    public class DataParserUnitTests : TestFixture
    {
        private readonly Dictionary<string, string> corrections = new Dictionary<string, string>
            {
                {
                    "ACESS INFORMATION:",
                    "ACCESS INFORMATION:"
                },
                {
                    "ACCESS INFO:",
                    "ACCESS INFORMATION:"
                },
                {
                    "Record     :  MSF731-RECORD / MIMS",
                    "Record          :  MSF731-RECORD / MIMS"
                },
                {
                    "Record      : MSFX39-RECORD / MIMS",
                    "Record          :  MSFX39-RECORD / MIMS"
                },
                {
                    "Description:Resource Requirement File",
                    "Description     : Resource Requirement File"
                },                
                {
                    "Description : Entity to Contractor Cross Reference",
                    "Description     : Entity to Contractor Cross Reference"
                },
                {
                    "Record Length   :263 Bytes",
                    "Record length   : 263 bytes"
                },
                {
                    "Record Length : 78 Bytes",
                    "Record length   : 78 bytes"
                },     
                {
                    "DETAIL:",
                    "DETAILS:"
                },
                {
                    " DETAILs:",
                    "DETAILS:"
                },
                {
                    "DETAILs:",
                    "DETAILS:"
                },
                {
                    "1",
                    ""
                }

            };

        [Test]
        public void SystemBlockTests()
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
                    new CobolBlockParser(), 
                };
            IDataParser dataParser = new DataParser(reader, parsers);
            dataParser.Corrections = corrections;
            dataParser.OnMissingParser = (line) => {
                                                       throw new InvalidOperationException("No Parser for: " + reader);
            };
            dataParser.Parse();
        }

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
                    new IgnoreParser(), 
                };
            IDataParser dataParser = new DataParser(reader, parsers);
            dataParser.Corrections = corrections;
            dataParser.OnMissingParser = (line) =>
            {
                throw new InvalidOperationException("No Parser for: " + reader);
            };
            dataParser.Parse();
        }

        [Test]
        public void HierarchySystemTests()
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
                    CobolParser.CobolHierarchy(), 
                    new IgnoreParser(), 
                };
            IDataParser dataParser = new DataParser(reader, parsers);
            dataParser.Corrections = corrections;
            dataParser.OnMissingParser = (line) =>
            {
                throw new InvalidOperationException("No Parser for: " + reader);
            };
            dataParser.Parse();
        }
    }
}