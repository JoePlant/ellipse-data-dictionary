namespace Ellipse.DataDictionary.Models
{
    public class StringModel : Model
    {
        public StringModel(string name, string data)
        {
            Name = name;
            Data = data;
        }

        public string Name { get; private set; }

        public string Data { get; private set; }

        public override string ToString()
        {
            return string.Format("[{0}] {1}", Name, Data);
        }
    }
}