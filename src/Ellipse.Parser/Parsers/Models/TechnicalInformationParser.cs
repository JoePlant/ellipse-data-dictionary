using System;
using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Models
{
    public class TechnicalInformationParser : BlockParser
    {
        private static readonly ILineMatcher[] DetailBlock = new[]
            {
                Line.StartsWith("TECHNICAL INFORMATION"),
                Line.Repeat(Line.Any()).Until(Line.StartsWith("01  ")),
            };

        public TechnicalInformationParser() : base(DetailBlock)
        {
        }

        protected override Model CreateModel(string[] lines)
        {
            return new StringModel("TechnicalInformation", string.Join(Environment.NewLine, lines));
        }
    }
}