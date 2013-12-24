
using System.Collections.Generic;

namespace Ellipse.DataDictionary.Models
{
    public interface IModel
    {
        string Name { get; }
        IModel GetModel(string path);
        IDictionary<string, string> GetModelParts();
    }
}