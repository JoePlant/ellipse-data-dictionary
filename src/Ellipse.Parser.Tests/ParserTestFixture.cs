using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
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
        private readonly IModelParser modelParser; 

        protected ParserTestFixture() : this(() => new T())
        {
        }

        protected ParserTestFixture(Func<IModelParser> parserFunc)
        {
            modelParser = parserFunc();
        }

        protected static IDataParser CreateDataParser(IReader reader, IModelParser modelParser, params IModelParser[] optionalParsers)
        {
            List<IModelParser> parsers = new List<IModelParser> { modelParser };
            parsers.AddRange(optionalParsers);
            IDataParser dataParser = new DataParser(reader, parsers.ToArray());
            dataParser.OnMissingParser = line =>
            {
                throw new InvalidOperationException("No Parser for: " + reader);
            };
            return dataParser;
        }

        protected IDataParser CreateDataParser(IReader reader, IModelParser[] optional)
        {
            return CreateDataParser(reader, new T(), optional);
        }

        protected IDataParser CreateDataParser(IReader reader)
        {
            return CreateDataParser(reader, new T(), new IModelParser[0]);
        }

        protected void AssertDoesNotParse(string[] cases)
        {
            AssertDoesNotParse(cases, modelParser);
        }

        protected static void AssertDoesNotParse(string[] cases, Func<IModelParser> parserFunc )
        {
            AssertDoesNotParse(cases, parserFunc());
        }

        protected static void AssertDoesNotParse(string[] cases, IModelParser modelParser)
        {
            foreach (string line in cases)
            {
                bool isMissing = false;
                Reader reader = Reader.CreateStringReader(line);
                IDataParser dataParser = new DataParser(reader, new[] { modelParser });
                dataParser.OnMissingParser = s => isMissing = true;
                dataParser.Parse();

                Assert.That(dataParser.Results, Is.Empty, "Results should be empty: {0}", line);
                Assert.That(isMissing, Is.True, "It should not parse line: {0}", line);
            }
        }

        protected void AssertParsed(IDataParser dataParser, IModel[] expectedModel)
        {
            dataParser.Parse();
            string actual = BuildStringModel(dataParser.Results);
            string expect = BuildStringModel(expectedModel);

            Debug.WriteLine(actual);
            Assert.That(actual, Is.EqualTo(expect));
        }

        protected void AssertParsed(IDataParser dataParser, IModel expectedModel)
        {
            dataParser.Parse();
            Assert.That(dataParser.Results.Count, Is.EqualTo(1), "Only one model allowed.");

            AssertModelIsSame(dataParser.Results[0], expectedModel);
        }

        protected static string BuildStringModel(IList<IModel> modelList)
        {
            StringBuilder builder = new StringBuilder();
            
            int index = 0;
            foreach (IModel model in modelList)
            {
                index++;
                builder.AppendFormat("{0}: {1}", index, model);
                builder.AppendLine();
            }
            if (modelList.Count > 1)
            {
                builder.AppendFormat("Elements: {0}", modelList.Count);
            }
            return builder.ToString();
        }

        protected void AssertModelIsSame(IModel actualModel, IModel expectedModel)
        {
            string actual = new ModelFormatter(actualModel){IncludeModelPaths = true}.Render();
            string expected = new ModelFormatter(expectedModel) { IncludeModelPaths = true }.Render();

            Assert.That(actual, Is.EqualTo(expected), "Actual: \r\n{0}\r\nExpected:\r\n{1}",actual, expected);
        }


        protected HierarchyModel ParseHierarchicalFile(string fileName)
        {
            string methodName = new StackTrace().GetFrame(1).GetMethod().Name.Replace("_", "-");
            FileInfo file = new FileInfo(fileName);
            Assert.That(file.Exists, Is.True, "File doesn't exist {0}", fileName);

            FileReader reader = new FileReader(fileName);
            List<IModelParser> list = new List<IModelParser> { modelParser };
        
            IDataParser dataParser = new DataParser(reader, list.ToArray());
            dataParser.OnMissingParser = s =>
            {
                Assert.Fail("Unable to Parse: {0}", reader);
                return false;
            };
            dataParser.Parse();

            Assert.That(dataParser.Results, Is.Not.Empty, "Results should not be empty");
            Assert.That(dataParser.Results[0], Is.TypeOf<HierarchyModel>(), "Expected a Cobol Model");

            HierarchyModel hierarchyModel = dataParser.Results[0] as HierarchyModel;
            Assert.That(hierarchyModel, Is.Not.Null, "Class Model not found: {0}", dataParser.Results[0]);
            if (hierarchyModel != null)
            {
                CobolModel model = (CobolModel)hierarchyModel.Model;
                Assert.That(file.Name, Is.StringContaining(model.Data), "Class name doesn't match the filename");
                Assert.That(model.Data, Is.EqualTo(methodName), "Class name doesn't match the method name");
            }
            return hierarchyModel;
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