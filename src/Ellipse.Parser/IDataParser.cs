using System;
using System.Collections.Generic;

namespace Ellipse.DataDictionary
{
    public interface IDataParser
    {
        void Parse();
        Func<string, bool> OnMissingParser { set; }
        IDictionary<string, string> Corrections { set; }
    }
}