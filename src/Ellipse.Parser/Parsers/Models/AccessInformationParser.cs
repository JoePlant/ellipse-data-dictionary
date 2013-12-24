using System;
using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Models
{
    public class AccessInformationParser : BlockParser
    {
        private static readonly ILineMatcher[] DetailBlock = new[]
            {
                Line.StartsWith("ACCESS INFORMATION"),
                Line.Optional(
                    Line.Repeat(
                        Line.Any())
                    .Until(Line.StartsWith("TECHNICAL INFORMATION"))),
            };

        public AccessInformationParser() : base(DetailBlock)
        {
        }

        protected override IModel CreateModel(string[] lines)
        {
            return new StringModel("AccessInformation", string.Join(Environment.NewLine, lines));
        }
    }
}