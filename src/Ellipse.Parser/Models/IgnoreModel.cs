namespace Ellipse.DataDictionary.Models
{
    public class IgnoreModel : Model
    {
        public IgnoreModel(string line)
        {
            Line = line;
        }

        public string Line { get; private set; }

        public override string ToString()
        {
            return "[Ignore] " + Line;
        }
    }
}