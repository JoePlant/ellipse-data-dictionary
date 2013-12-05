using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Parsers.Lines;
using Ellipse.DataDictionary.Readers;

namespace Ellipse.DataDictionary.Parsers
{
    public abstract class SingleLineParser : IModelParser 
    {
        private readonly string name;
        private readonly ILineMatcher lineMatcher;
        private readonly ILineParser lineParser;

        protected SingleLineParser(string name, ILineMatcher lineMatcher, ILineParser lineParser)
        {
            this.name = name;
            this.lineMatcher = lineMatcher;
            this.lineParser = lineParser;
        }

        public bool Matches(IReader reader)
        {
            int lines;
            return lineMatcher.Matches(reader, 0, out lines);
        }
        
        public Model Parse(IReader reader)
        {
            string line = reader.ReadLine();
            string data = lineParser.Parse(line);
            return new StringModel(name, data);
        }
        
    }
}