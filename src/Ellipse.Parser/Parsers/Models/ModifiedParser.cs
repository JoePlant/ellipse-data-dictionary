using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Models
{
    public class ModifiedParser : BlockParser
    {
        private const string prefix = "Modified        :";
        private const string name = "Modified";

        private static readonly ILineMatcher[] ModifiedLine = new[]
            {
                Line.StartsWith(prefix),
                Line.Optional(Line.Repeat(Line.IsEmpty()).Until(Line.IsNotEmpty()))
            };


        public ModifiedParser() : base(ModifiedLine)
        {
        }

        protected override Model CreateModel(string[] lines)
        {
            if (lines.Length > 0)
            {
                return new StringModel("Modified", Data.IgnoreStart(prefix).Trim().Parse(1,lines[0]));
            }
            return null;
        }
    }
}