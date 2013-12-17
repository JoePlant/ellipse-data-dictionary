using System;
using System.Collections.Generic;
using System.Text;
using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Parsers;
using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary
{
    [TestFixture]
    public abstract class ParserTestFixture<T> : TestFixture where T : IModelParser, new()
    {
        protected T CreateParser()
        {
            return new T();
        }

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

        protected void ParseFile(string fileName)
        {
            FileReader reader = new FileReader(fileName);
            IDataParser dataParser = new DataParser(reader, new IModelParser[] { new T() });
            dataParser.OnMissingParser = s =>
                {
                    Assert.Fail("Unable to Parse: {0}", reader);
                    return false;
                };
            dataParser.Parse();

            Assert.That(dataParser.Results, Is.Not.Empty, "Results should not be empty");
        }
    }
}