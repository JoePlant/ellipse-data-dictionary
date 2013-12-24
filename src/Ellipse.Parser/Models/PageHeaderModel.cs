using System.Collections.Generic;
using System.Text;

namespace Ellipse.DataDictionary.Models
{
    public class PageHeaderModel : IModel
    {
        private readonly string[] lines;

        public PageHeaderModel(string[] lines)
        {
            this.lines = lines;
        }

        public override string ToString()
        {
            StringBuilder builder = new StringBuilder();
            foreach (string line in lines)
            {
                builder.Append("[Page] ");
                builder.AppendLine(line);
            }
            return builder.ToString();
        }

        public string Name
        {
            get { return "Page"; }
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
                    {"lines", string.Join("\r\n", lines)}
                };
        }
    }
}
