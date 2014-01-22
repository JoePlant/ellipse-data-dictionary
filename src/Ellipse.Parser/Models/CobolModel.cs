
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
            Dictionary<string, string> dictionary = new Dictionary<string, string>();
            dictionary["data"] = Data;
            if (!string.IsNullOrEmpty(Comment))
            {
                dictionary["comment"] = Comment;
            }
            return dictionary;
        }

        public new static IModel Factory(string name, string data, string comment)
        {
            return new CobolModel(name, data, comment);
        }
    }
}