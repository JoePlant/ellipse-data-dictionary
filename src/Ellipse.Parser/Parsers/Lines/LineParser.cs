using System;
using System.Collections.Generic;
using System.Linq;

namespace Ellipse.DataDictionary.Parsers.Lines
{
    public class LineParser : ILineParser
    {
        private readonly List<Func<int, string, string>> standardParseList;
        private readonly Dictionary<int, ILineParser> lineDictionary; 
       
        public LineParser()
        {
            standardParseList = new List<Func<int, string, string>>();
            lineDictionary = new Dictionary<int, ILineParser>();
        }
        
        public string Parse(int lineNo, string line)
        {
            ILineParser lineParser;
            if (lineDictionary.TryGetValue(lineNo, out lineParser))
            {
                return lineParser.Parse(lineNo, line);
            }

            return standardParseList.Aggregate(line, (current, func) => func(lineNo, current));
        }

        public ILineParser IgnoreStart(string start)
        {
            standardParseList.Add(
                    (i, line) => (line != null && line.StartsWith(start)) 
                            ? line.Substring(start.Length) 
                            : line
                         );
            return this;
        }

        public ILineParser IgnoreEnd(string end)
        {
            standardParseList.Add(
                (i, line) => (line != null && line.EndsWith(end))
                            ? line.Substring(0, line.Length - end.Length)
                            : line
             );
            return this;
        }

        public ILineParser IgnoreAfter(string marker)
        {
            standardParseList.Add(
                (i, line) => (line != null && line.Contains(marker))
                            ? line.Substring(0, line.IndexOf(marker, StringComparison.InvariantCulture))
                            : line
                );
            return this;
        }

        public ILineParser IgnoreBefore(string marker)
        {
            standardParseList.Add(
                (i, line) => (line != null && line.Contains(marker))
                            ? line.Substring(line.IndexOf(marker, StringComparison.InvariantCulture)+1)
                            : line
                );
            return this;
        }

        public ILineParser IgnoreBefore(int columnNo)
        {
            standardParseList.Add(
                (i, line) => (line != null && line.Length >= columnNo)
                            ? line.Substring(columnNo)
                            : ""
                );
            return this;
        }
        public ILineParser Trim()
        {
            standardParseList.Add(
                (i, line) => !string.IsNullOrEmpty(line)
                            ? line.Trim()
                            : line
                );
            return this;
        }

        public ILineParser RemoveSpaces()
        {
            standardParseList.Add(
                (i, line) => line != null && line.Contains(" ")
                            ? string.Join(" ", line.Split(new []{" "}, StringSplitOptions.RemoveEmptyEntries))
                            : line
                );
            return this;
        }

        public ILineParser IgnoreAll()
        {
            standardParseList.Add(
                (i,line) => null
                );
            return this;
        }

        public ILineParser OnLineNumber(int lineNo, ILineParser lineParser)
        {
            lineDictionary.Add(lineNo, lineParser);
            return this;
        }

        public ILineParser TruncateAt(int columnNo)
        {
            standardParseList.Add(
                (i, line) => line != null && line.Length > columnNo
                            ? line.Substring(0, columnNo)
                            : line
                );
            return this;
        }
    }
}