using System;

namespace Ellipse.DataDictionary
{
    public interface IDataParser
    {
        void Parse();
        Func<string, bool> OnMissingParser { set; }
    }
}