using System;
using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Models
{
    public class DetailsParser : BlockParser
    {
        private static readonly ILineMatcher[] DetailBlock = new[]
            {
                Line.IsEqual("DETAILS:"),
                Line.Repeat(Line.Any()).Until(Line.IsEqual("MODULE:")),
            };

        public DetailsParser() : base(DetailBlock)
        {
        }

        protected override Model CreateModel(string[] lines)
        {
            return new StringModel("DETAILS", string.Join(Environment.NewLine, lines));
        }
    }
}