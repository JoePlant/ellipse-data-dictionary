using System;
using System.Collections.Generic;

namespace Ellipse.DataDictionary.Models
{
    public class DataTypeModel : CobolModel
    {
        public DataTypeModel(string name, string data, string comment)
            : base(name, data, comment)
        {
        }

        public new static IModel Factory(string name, string data, string comment)
        {
            return new DataTypeModel(name, data, comment);
        }

        public override IDictionary<string, string> GetModelParts()
        {
            string[] parts = Data.Split(new[] {' '}, StringSplitOptions.None);

            IDictionary<string, string> dictionary = new Dictionary<string, string>();

            dictionary["name"] = "{implied}";
            List<string> dataParts = new List<string>();
            switch (parts.Length)
            {
                case 1:
                    {
                        return base.GetModelParts();
                    }
                case 2:
                    {
                        if (parts[0] == "PIC")
                        {
                            dictionary["type"] = parts[1];
                        }
                        else
                        {
                            return base.GetModelParts();
                        }
                        break;
                    }
                default:
                    {
                        if (parts[1] == "PIC")
                        {
                            dictionary["name"] = parts[0];
                            for (int i = 2; i < parts.Length; i++)
                            {
                                dataParts.Add(parts[i]);
                            }
                            dictionary["type"] = string.Join(" ", dataParts);
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
    }
}