using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Models
{
    public class CobolBlockParser : BlockParser
    {
        private static readonly ILineMatcher[] CobolBlock = new[]
            {
                Line.StartsWith("01  "),
                Line.Repeat(Line.Any()).Until(Line.IsEmpty()),
            };

        public CobolBlockParser() : base(CobolBlock)
        {
        }

        protected override IModel CreateModel(string[] lines)
        {
            return new StringModel("Cobol", string.Join("\n", lines));
        }
    }
}