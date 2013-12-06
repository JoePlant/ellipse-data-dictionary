﻿using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Models
{
    public class CobolParser : BlockParser
    {
        private static readonly ILineMatcher[] CobolBlock = new[]
            {
                Line.StartsWith("01  "),
                Line.Repeat(Line.Any()).Until(Line.IsEmpty()),
            };

        public CobolParser() : base(CobolBlock)
        {
        }

        protected override Model CreateModel(string[] lines)
        {
            return new StringModel("Cobol", string.Join("\n", lines));
        }
    }
}