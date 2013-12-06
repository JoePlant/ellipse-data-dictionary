using System;
using System.Collections.Generic;

namespace Ellipse.DataDictionary.Readers
{
    public class LookupReader : ModifyReader
    {
        public LookupReader(IReader reader, IDictionary<string, string> corrections)
            : base(reader, (line) => Correct(corrections, line))
        {
        }

        private static string Correct(IDictionary<string, string> corrections, string line)
        {
            string corrected;
            return corrections.TryGetValue(line, out corrected) ? corrected : line;
        }
    }
}