using System.IO;

namespace Ellipse.DataDictionary.Readers
{
    public class FileReader : Reader
    {
        public FileReader(string fileName) : base(new StreamReader(fileName))
        {
        }
    }
}