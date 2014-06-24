using System;
using System.Collections.Generic;
using System.IO;
using Ellipse.DataDictionary.Models;
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
        public void HeaderSystemTests()
        {
            IReader reader = new FileReader(@".\Resources\DataDictionary\datadict.rpt");
            IModelParser[] parsers = new IModelParser[] {
                    new PageHeaderParser(),
                    new AccessInformationParser(),
                    new DescriptionParser(),
                    new DetailsParser(),
                    new ModifiedParser(),
                    new ModuleParser(),
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

            string typename = GetType().Name;
            string directory = Path.Combine(".", typename);

            if (!Directory.Exists(directory))
            {
                Directory.CreateDirectory(directory);
            }

            foreach (IModel model in dataParser.Results)
            {
                HierarchyModel hiearchyModel = model as HierarchyModel;
                if (hiearchyModel != null)
                {
                    ClassModel classModel = hiearchyModel.Model as ClassModel;
                    if (classModel != null)
                    {
                        ModelXmlFormatter formatter = new ModelXmlFormatter(model);
                        string xml = formatter.Render();
                        string objectName = classModel.Data;

                        string fileName = directory + @"\" + objectName + ".xml";
                        if (File.Exists(fileName))
                        {
                            File.Delete(fileName);
                        }

                        Assert.That(File.Exists(fileName), Is.False);
                        using (StreamWriter writer = new StreamWriter(fileName))
                        {
                            writer.Write(xml);
                        }
                    }
                }
            }
        }
    }
}