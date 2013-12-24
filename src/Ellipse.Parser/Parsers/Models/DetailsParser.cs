using System;
using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Models
{
    public class DetailsParser : BlockParser
    {
        private static readonly ILineMatcher[] DetailBlock = new[]
            {
                Line.Or(Line.StartsWith("DETAILS"), Line.StartsWith("Details")),
                Line.Optional(Line.Repeat(Line.Any()).Until(Line.StartsWith("MODULE"))),
            };

        public DetailsParser() : base(DetailBlock)
        {
        }

        protected override IModel CreateModel(string[] lines)
        {
            return new StringModel("DETAILS", string.Join(Environment.NewLine, lines));
        }
    }
}