using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Parsers.Lines;
using Ellipse.DataDictionary.Readers;

namespace Ellipse.DataDictionary.Parsers
{
    public abstract class ImpliedModelParser : IImpliedModelParser, ILineMatcher
    {
        private readonly string name;
        private readonly ILineMatcher lineMatcher;
        private readonly ILineParser modelParser;
        private readonly ILineParser impliedParser;

        protected ImpliedModelParser(string name, ILineMatcher lineMatcher, ILineParser modelParser, ILineParser impliedParser)
        {
            this.name = name;
            this.lineMatcher = lineMatcher;
            this.modelParser = modelParser;
            this.impliedParser = impliedParser;
        }

        public bool Matches(IModel model)
        {
            CobolModel cobolModel = model as CobolModel;
            string data = cobolModel != null ? cobolModel.Data : null;
            if (!string.IsNullOrEmpty(data))
            {
                IReader reader = Reader.CreateStringReader(data);
                int linesRead;
                return Matches(reader, 0, out linesRead);
            }
            return false;
        }
        
        public bool Matches(IReader reader, int offset, out int linesRead)
        {
            return lineMatcher.Matches(reader, 0, out linesRead);
        }

        public IModel Parse(IModel model)
        {
            CobolModel cobolModel = model as CobolModel;
            string data = cobolModel != null ? cobolModel.Data : null;
            if (!string.IsNullOrEmpty(data))
            {
                IReader reader = Reader.CreateStringReader(data);
                int linesRead;
                if (lineMatcher.Matches(reader, 0, out linesRead))
                {
                    string modelData = modelParser.Parse(0, data);
                    string impliedData = impliedParser.Parse(0, data);
                    if (!string.IsNullOrEmpty(impliedData))
                    {
                        return new HierarchyModel(new CobolModel(cobolModel.Name, modelData, cobolModel.Comment),
                                                  new IModel[]
                                                      {
                                                          new CobolModel(name, impliedData, "Implied")
                                                      });
                    }
                }

            }

            return model;
        }

    }
}