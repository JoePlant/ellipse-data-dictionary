using System.Collections.Generic;

namespace Ellipse.DataDictionary.Models
{
    public class StringModel : IModel
    {
        public StringModel(string name, string data)
        {
            Name = name;
            Data = data;
        }

        public string Name { get; private set; }

        public IModel GetModel(string path)
        {
            if (path == "" || path == "1")
            {
                return this;
            }
            return null;
        }

        public virtual IDictionary<string, string> GetModelParts()
        {
            return new Dictionary<string, string>
                {
                    {"data", Data}
                };
        }

        public string Data { get; private set; }

        public override string ToString()
        {
            return string.Format("[{0}] {1}", Name, Data);
        }
    }
}