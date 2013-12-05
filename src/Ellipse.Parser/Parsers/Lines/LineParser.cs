using System;
using System.Collections.Generic;
using System.Linq;

namespace Ellipse.DataDictionary.Parsers.Lines
{
    public class LineParser : ILineParser
    {
        private readonly List<Func<string, string>>  parseList; 
       
        public LineParser()
        {
            parseList = new List<Func<string, string>>();
        }

        public string Parse(string line)
        {
            return parseList.Aggregate(line, (current, func) => func(current));
        }

        public ILineParser IgnoreStart(string start)
        {
            parseList.Add(
                    line => (line != null && line.StartsWith(start)) 
                            ? line.Substring(start.Length) 
                            : line
                         );
            return this;
        }

        public ILineParser IgnoreEnd(string end)
        {
            parseList.Add(
                line => (line != null && line.EndsWith(end))
                            ? line.Substring(0, line.Length - end.Length)
                            : line
             );
            return this;
        }

        public ILineParser Trim()
        {
            parseList.Add(line => !string.IsNullOrEmpty(line) ? line.Trim() : line);
            return this;
        }
    }
}