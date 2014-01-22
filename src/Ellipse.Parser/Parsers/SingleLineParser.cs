using System;
using System.Collections.Generic;
using System.Linq;
using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Parsers.Lines;
using Ellipse.DataDictionary.Readers;

namespace Ellipse.DataDictionary.Parsers
{
    public abstract class SingleLineParser : IModelParser 
    {
        private readonly string name;
        private readonly ILineMatcher lineMatcher;
        private readonly ILineParser dataParser;
        private readonly ILineParser commentParser;
        private readonly IImpliedModelParser[] impliedModelParsers;
        private readonly ModelFactoryDelegate modelFactory;

        protected SingleLineParser(string name, ILineMatcher lineMatcher, ILineParser dataParser)
            : this(name, lineMatcher, dataParser, Comment.IgnoreAll(), null, StringModel.Factory)
        {
        }

        protected SingleLineParser(string name, ILineMatcher lineMatcher, ILineParser dataParser, ILineParser commentParser)
            : this(name, lineMatcher, dataParser, commentParser, null, StringModel.Factory)
        {
        }

        protected SingleLineParser(string name, ILineMatcher lineMatcher, ILineParser dataParser, ILineParser commentParser, IImpliedModelParser[] impliedModelParsers, ModelFactoryDelegate modelFactory)
        {
            this.name = name;
            this.lineMatcher = lineMatcher;
            this.dataParser = dataParser;
            this.commentParser = commentParser;
            this.impliedModelParsers = impliedModelParsers;
            this.modelFactory = modelFactory;
        }

        public bool Matches(IReader reader)
        {
            int lines;
            return lineMatcher.Matches(reader, 0, out lines);
        }
        
        public IModel Parse(IReader reader)
        {
            int numLines;
            if (lineMatcher.Matches(reader, 0, out numLines))
            {
                string[] lines = reader.ReadLines(numLines);
                int lineNo = 0;
                List<string> data = new List<string>();
                List<string> comment = new List<string>();
                foreach (string line in lines)
                {
                    string dataLine = dataParser.Parse(lineNo, line);
                    if (!string.IsNullOrEmpty(dataLine))
                    {
                        data.Add(dataLine);
                    }

                    string commentLine = commentParser.Parse(lineNo, line);
                    if (!string.IsNullOrEmpty(commentLine))
                    {
                        comment.Add(commentLine);
                    }
                    lineNo++;
                }

                IModel model = modelFactory(name, string.Join(" ", data), string.Join("\n", comment));

                if (impliedModelParsers != null)
                {
                    return impliedModelParsers.Aggregate(model, (current, impliedParser) => impliedParser.Matches(current) ? impliedParser.Parse(current) : current);
                }
                return model;
            }
            return null;
        }
        
    }
}