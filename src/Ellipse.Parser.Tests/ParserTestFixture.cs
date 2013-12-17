using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text;
using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Parsers;
using Ellipse.DataDictionary.Readers;
using NUnit.Framework;
using StringReader = Ellipse.DataDictionary.Readers.StringReader;

namespace Ellipse.DataDictionary
{
    [TestFixture]
    public abstract class ParserTestFixture<T> : TestFixture where T : IModelParser, new()
    {
        protected IDataParser CreateDataParser(IReader reader, params IModelParser[] optional)
        {
            List<IModelParser> parsers = new List<IModelParser> {new T()};
            parsers.AddRange(optional);
            IDataParser dataParser = new DataParser(reader, parsers.ToArray());
            dataParser.OnMissingParser = (line) =>
            {
                throw new InvalidOperationException("No Parser for: " + reader);
            };
            return dataParser;
        }

        protected void AssertDoesNotParse(string[] cases)
        {
            foreach (string line in cases)
            {
                bool isMissing = false;
                StringReader reader = new StringReader(line);
                IDataParser dataParser = new DataParser(reader, new IModelParser[] {new T()});
                dataParser.OnMissingParser = s => isMissing = true;
                dataParser.Parse();

                Assert.That(dataParser.Results, Is.Empty, "Results should be empty: {0}", line);
                Assert.That(isMissing, Is.True, "Missing Parser: {0}", line);
            }
        }

        protected void AssertParsed(IDataParser dataParser, params Model[] expectedModel)
        {
            dataParser.Parse();
            string actual = BuildStringModel(dataParser.Results);
            string expect = BuildStringModel(expectedModel);

            Assert.That(actual, Is.EqualTo(expect));
        }

        protected static string BuildStringModel(IList<Model> modelList)
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

        protected void ParseFile(string fileName, params IModelParser[] additionalParsers)
        {
            string methodName = new StackTrace().GetFrame(1).GetMethod().Name.Replace("_", "-");
            FileInfo file = new FileInfo(fileName);
            Assert.That(file.Exists, Is.True, "File doesn't exist {0}", fileName);

            FileReader reader = new FileReader(fileName);
            List<IModelParser> list = new List<IModelParser> {new T()};
            if (additionalParsers.Length > 0)
            {
                list.AddRange(additionalParsers);
            }
            IDataParser dataParser = new DataParser(reader, list.ToArray());
            dataParser.OnMissingParser = s =>
                {
                    Assert.Fail("Unable to Parse: {0}", reader);
                    return false;
                };
            dataParser.Parse();

            Assert.That(dataParser.Results, Is.Not.Empty, "Results should not be empty");
            Assert.That(dataParser.Results[0], Is.TypeOf<CobolModel>(), "Expected a Cobol Model");

            CobolModel classModel = dataParser.Results[0] as CobolModel;
            Assert.That(classModel, Is.Not.Null, "Class Model not found: {0}", dataParser.Results[0]);
            if (classModel != null)
            {
                Assert.That(file.Name, Is.StringContaining(classModel.Data), "Class name doesn't match the filename");
                Assert.That(classModel.Data, Is.EqualTo(methodName), "Class name doesn't match the method name");
            }
        }
    }
}