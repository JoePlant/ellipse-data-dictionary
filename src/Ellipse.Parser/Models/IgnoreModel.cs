using System.Collections.Generic;

namespace Ellipse.DataDictionary.Models
{
    public class IgnoreModel : IModel
    {
        public IgnoreModel(string line)
        {
            Line = line;
        }

        public string Line { get; private set; }

        public override string ToString()
        {
            return "[Ignore] " + Line;
        }

        public string Name
        {
            get { return "Ignore"; }
        }

        public IModel GetModel(string path)
        {
            throw new System.NotImplementedException();
        }

        public IDictionary<string, string> GetModelParts()
        {
            throw new System.NotImplementedException();
        }
    }
}