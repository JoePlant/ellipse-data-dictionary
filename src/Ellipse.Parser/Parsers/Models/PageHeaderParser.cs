using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Parsers.Lines;

namespace Ellipse.DataDictionary.Parsers.Models
{
    public class PageHeaderParser : BlockParser
    {
        private static readonly ILineMatcher[] PageHeader = new []
            {
                Line.Repeat(Line.IsEmpty()).Until(Line.Contains("Page")),
                Line.Contains("  Page  "),
                Line.IsEmpty(),
                Line.StartsWith("Dictionary file : "),
                Line.IsEmpty()
            };

        public PageHeaderParser() : base(PageHeader)
        {
        }

        protected override Model CreateModel(string[] lines)
        {
            return new PageHeaderModel(lines);
        }
    }
}