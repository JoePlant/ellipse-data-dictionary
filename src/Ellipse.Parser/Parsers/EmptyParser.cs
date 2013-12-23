using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Readers;

namespace Ellipse.DataDictionary.Parsers
{
    public class EmptyParser : IModelParser
    {
        public bool Matches(IReader reader)
        {
            return false;
        }

        public Model Parse(IReader reader)
        {
            return null;
        }
    }
}