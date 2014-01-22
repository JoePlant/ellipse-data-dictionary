using System;
using System.Collections.Generic;

namespace Ellipse.DataDictionary.Models
{
    public class RedefinesModel : CobolModel
    {
        public RedefinesModel(string name, string data, string comment)
            : base(name, data, comment)
        {
        }

        public new static IModel Factory(string name, string data, string comment)
        {
            return new RedefinesModel(name, data, comment);
        }

        public override IDictionary<string, string> GetModelParts()
        {
            string[] parts = Data.Split(new[] {' '}, StringSplitOptions.None);

            IDictionary<string, string> dictionary = new Dictionary<string, string>();

            switch (parts.Length)
            {
                case 3:
                    {
                        if (parts[1] == "REDEFINES")
                        {
                            dictionary["name"] = parts[0];
                            dictionary["target"] = parts[2];
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