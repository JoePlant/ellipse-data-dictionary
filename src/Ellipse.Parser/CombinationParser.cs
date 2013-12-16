using System;
using System.Linq;
using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Parsers;
using Ellipse.DataDictionary.Readers;

namespace Ellipse.DataDictionary
{
    public class CombinationParser : IModelParser
    {
        private readonly IModelParser[] parsers;

        public CombinationParser(params IModelParser[] parsers)
        {
            this.parsers = parsers;
            if (parsers.Length == 0)
            {
                throw new ArgumentException("No parsers specified");
            }
        }

        public bool Matches(IReader reader)
        {
            IModelParser parser = parsers.FirstOrDefault(p => p.Matches(reader));
            return parser != null;
        }

        public Model Parse(IReader reader)
        {
            IModelParser parser = parsers.FirstOrDefault(p => p.Matches(reader));
            return parser != null ? parser.Parse(reader) : null;
        }
    }
}