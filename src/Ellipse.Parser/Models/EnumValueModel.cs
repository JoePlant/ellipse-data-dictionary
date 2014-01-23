using System;
using System.Collections.Generic;

namespace Ellipse.DataDictionary.Models
{
    public class EnumValueModel : CobolModel
    {
        public EnumValueModel(string name, string data, string comment)
            : base(name, data, comment)
        {
        }

        public new static IModel Factory(string name, string data, string comment)
        {
            return new EnumValueModel(name, data, comment);
        }

        public override IDictionary<string, string> GetModelParts()
        {
            string[] parts = Data.Split(new[] {' '}, StringSplitOptions.None);
            bool singleValue = Data.Contains("'") &&
                               (((Data.Split(new[] {"'"}, StringSplitOptions.None).Length + 1)/2) == 2);

            IDictionary<string, string> dictionary = new Dictionary<string, string>();

            switch (parts.Length)
            {
                case 0:
                case 1:
                case 2:
                    {
                        return base.GetModelParts();
                    }
                case 3:
                    {
                        if (parts[1] == "VALUE")
                        {
                            AddName(dictionary, parts);
                            AddValueFollowing(dictionary, parts, "value");
                        }
                        else
                        {
                            return base.GetModelParts();
                        }
                        break;
                    }
                default:
                    {
                        if ((parts[1] == "VALUE") ) 
                        {
                            AddName(dictionary, parts);
                            AddValueFollowing(dictionary, parts, singleValue ? "value" : "values");
                        }
                        else
                        {
                            return base.GetModelParts();
                        }
                        break;
                    }
               }

            dictionary["text"] = Data;
            if (!string.IsNullOrEmpty(Comment))
            {
                dictionary["comment"] = Comment;
            }
            return dictionary;
        }

        private static void AddName(IDictionary<string, string> dictionary, string[] parts)
        {
            dictionary["name"] = parts[0];
        }

        private static void AddValueFollowing(IDictionary<string, string> dictionary, string[] parts, string key)
        {
            List<string> values = new List<string>();
            for (int i = 2; i < parts.Length; i++)
            {
                values.Add(parts[i]);
            }
            dictionary[key] = string.Join(" ", values);
        }
    }
}