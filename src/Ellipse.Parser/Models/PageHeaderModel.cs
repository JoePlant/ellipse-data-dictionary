using System.Text;

namespace Ellipse.DataDictionary.Models
{
    public class PageHeaderModel : Model
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
    }
}