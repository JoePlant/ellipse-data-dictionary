using Ellipse.DataDictionary.Models;

namespace Ellipse.DataDictionary.Parsers
{
    public interface IImpliedModelParser 
    {
        bool Matches(IModel model);
        IModel Parse(IModel model);
    }
}