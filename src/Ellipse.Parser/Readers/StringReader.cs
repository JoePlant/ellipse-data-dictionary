using System.Collections.Generic;

namespace Ellipse.DataDictionary.Readers
{
    public class StringReader : Reader
    {
        public StringReader(string text) : base (new System.IO.StringReader(string.IsNullOrEmpty(text) ? "" : text))
        {
        }
    }
}