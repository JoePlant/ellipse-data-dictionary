using System.Collections.Generic;
using System.IO;

namespace Ellipse.DataDictionary.Readers
{
    public class Reader : IReader
    {
        private readonly List<string> lines = new List<string>();
        private int lineNo;
        private readonly int maxLines;

        public Reader(TextReader reader)
        {
            using (reader)
            {
                string line = reader.ReadLine();

                while (line != null)
                {
                    lines.Add(line);
                    line = reader.ReadLine();
                }
            }

            maxLines = lines.Count;
            lineNo = 0;
        }

        public bool IsEndOfFile(int offset)
        {
            int required = lineNo + offset;
            return required >= maxLines;
        }

        public string PeekAhead(int plusLines)
        {
            int required = lineNo + plusLines;
            return required < maxLines ? lines[required] : null;
        }

        public string[] ReadLines(int numLines)
        {
            List<string> result = new List<string>();
            for (int i = 0; i < numLines; i++)
            {
                if (!EndOfFile)
                {
                    result.Add(ReadLine());
                }
            }
            return result.ToArray();
        }

        public string ReadLine()
        {
            string result = null;
            if (!EndOfFile)
            {
                result = lines[lineNo];
                lineNo++;
            }
            return result;
        }

        public int LineNumber
        {
            get { return lineNo + 1; }
        }

        public bool EndOfFile
        {
            get { return lineNo >= maxLines; }
        }

        public override string ToString()
        {
            if (EndOfFile)
            {
                return string.Format("End of file: {0} {1}", maxLines, maxLines == 1 ? "line" : "lines");
            }
            return string.Format("Line: {0} '{1}'", LineNumber, lines[lineNo]);
        }

        public string PeekNext()
        {
            return PeekAhead(0);
        }
    }
}