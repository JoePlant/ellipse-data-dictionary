using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Readers;

namespace Ellipse.DataDictionary.Parsers
{
    public interface IModelParser
    {
        bool Matches(IReader reader);

        IModel Parse(IReader reader);
    }
}