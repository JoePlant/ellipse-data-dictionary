using System;
using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Models
{
    public class ModuleParser : BlockParser
    {
        private static readonly ILineMatcher[] ModuleBlock = new[]
            {
                Line.StartsWith("MODULE"),
                Line.Any(),
                Line.Optional(Line.Repeat(Line.IsEmpty()).Until(Line.IsNotEmpty())),
            };

        public ModuleParser() : base(ModuleBlock)
        {
        }

        protected override Model CreateModel(string[] lines)
        {
            return new StringModel("MODULE", string.Join(Environment.NewLine, lines));
        }
    }
}