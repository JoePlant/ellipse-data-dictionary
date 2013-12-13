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

        protected void AssertResults(IDataParser dataParser, params Model[] expectedModel)
        {
            string actual = BuildStringModel(dataParser.Results);
            string expect = BuildStringModel(expectedModel);

            Assert.That(actual, Is.EqualTo(expect));
        }

        protected static string BuildStringModel(IList<Model> modelList)
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