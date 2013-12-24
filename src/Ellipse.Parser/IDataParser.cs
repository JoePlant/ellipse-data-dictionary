using System;
using System.Collections.Generic;
using Ellipse.DataDictionary.Models;

namespace Ellipse.DataDictionary
{
    public interface IDataParser
    {
        void Parse();
        Func<string, bool> OnMissingParser { set; }
        IDictionary<string, string> Corrections { set; }
        IList<IModel> Results { get; }
    }
}