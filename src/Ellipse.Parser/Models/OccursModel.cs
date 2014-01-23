using System;
using System.Collections.Generic;

namespace Ellipse.DataDictionary.Models
{
    public class OccursModel : CobolModel
    {
        public OccursModel(string name, string data, string comment)
            : base(name, data, comment)
        {
        }

        public new static IModel Factory(string name, string data, string comment)
        {
            return new OccursModel(name, data, comment);
        }

        public override IDictionary<string, string> GetModelParts()
        {
            string[] parts = Data.Split(new[] {' '}, StringSplitOptions.None);

            IDictionary<string, string> dictionary = new Dictionary<string, string>();

            switch (parts.Length)
            {
                case 6:
                    {
                        if (parts[1] == "OCCURS")
                        {
                            dictionary["name"] = parts[0];
                            dictionary["occurs"] = parts[2];
                            dictionary["indexed-by"] = parts[5];
                        }
                        else
                        {
                            return base.GetModelParts();
                        }
                        break;
                    }
                default:
                    {
                        return base.GetModelParts();
                    }
            }

            dictionary["text"] = Data;
            if (!string.IsNullOrEmpty(Comment))
            {
                dictionary["comment"] = Comment;
            }
            return dictionary;
        }
    }
}