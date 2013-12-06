using System;
using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Models
{
    public class EndOfFileParser : BlockParser
    {
        private static readonly ILineMatcher[] EndOfFile = new[]
            {
                Line.IsEmpty(),
                Line.Repeat(Line.IsEmpty()),
            };

        public EndOfFileParser() : base(EndOfFile)
        {
        }

        protected override Model CreateModel(string[] lines)
        {
            return new IgnoreModel(string.Join("\n", lines));
        }
    }
}