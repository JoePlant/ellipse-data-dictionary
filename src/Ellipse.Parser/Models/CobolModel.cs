
using System.Collections.Generic;

namespace Ellipse.DataDictionary.Models
{
    public class CobolModel : StringModel
    {
        public CobolModel(string name, string data) : base(name, data)
        {
        } 

        public CobolModel(string name, string data, string comment) : base(name, data)
        {
            Comment = comment;
        }

        public string Comment { get; private set; }

        public override string ToString()
        {
            string comment = string.IsNullOrEmpty(Comment) ? "" : " /*...*/";
            return base.ToString() + comment;
        }

        public override IDictionary<string, string> GetModelParts()
        {
            return new Dictionary<string, string>
                {
                    {"data", Data},
                    {"comment", Comment}
                };
        }
    }
}