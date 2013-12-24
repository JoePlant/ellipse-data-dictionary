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
            if (path == "" || path == "1")
            {
                return this;
            }
            return null;
        }

        public IDictionary<string, string> GetModelParts()
        {
            return new Dictionary<string, string>
                {
                    {"line", Line}
                };
        }
    }
}