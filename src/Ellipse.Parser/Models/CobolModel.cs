namespace Ellipse.DataDictionary.Models
{
    public class CobolModel : StringModel, IModel
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

        public Model GetModel(string path)
        {
            if (path == "" || path == "1")
            {
                return this;
            }
            return null;
        }
    }
}