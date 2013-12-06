using System;
using System.Linq;

namespace Ellipse.DataDictionary.Readers
{
    public class ModifyReader : IReader
    {
        private readonly IReader reader;
        private readonly Func<string, string> modifyFunc;

        public ModifyReader(IReader reader, Func<string, string> modifyFunc)
        {
            this.reader = reader;
            this.modifyFunc = modifyFunc;
        }

        protected IReader Reader
        {
            get { return reader; }
        }

        public string PeekAhead(int plusLines)
        {
            string line = reader.PeekAhead(plusLines);
            return modifyFunc(line);
        }

        public string PeekNext()
        {
            string line = reader.PeekNext();
            return modifyFunc(line);
        }

        public string[] ReadLines(int numLines)
        {
            string[] lines = reader.ReadLines(numLines);

            return lines.Select(line => modifyFunc(line)).ToArray();
        }

        public string ReadLine()
        {
            string line = reader.ReadLine();
            return modifyFunc(line);
        }

        public int LineNumber { get { return reader.LineNumber; }}
        public bool EndOfFile { get { return reader.EndOfFile; }}
        public bool IsEndOfFile(int offset)
        {
            return reader.IsEndOfFile(offset);
        }
    }
}