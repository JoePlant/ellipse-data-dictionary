using System.Collections.Generic;

namespace Ellipse.DataDictionary.Models
{
    public class PropertyModel : CobolModel
    {
        public PropertyModel(string name, string data, string comment)
            : base(name, data, comment)
        {
        }

        public new static IModel Factory(string name, string data, string comment)
        {
            return new PropertyModel(name, data, comment);
        }

        public override IDictionary<string, string> GetModelParts()
        {
            IDictionary<string, string> dictionary = new Dictionary<string, string>();

            dictionary["name"] = Data;
            dictionary["text"] = Data;
            if (!string.IsNullOrEmpty(Comment))
            {
                dictionary["comment"] = Comment;
            }
            return dictionary;
        }
    }
}