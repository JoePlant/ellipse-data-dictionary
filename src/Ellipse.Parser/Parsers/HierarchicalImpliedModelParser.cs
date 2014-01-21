using Ellipse.DataDictionary.Models;

namespace Ellipse.DataDictionary.Parsers
{
    public class HierarchicalImpliedModelParser : IImpliedModelParser
    {
        private readonly IImpliedModelParser parentParser;
        private readonly IImpliedModelParser childParser;

        public HierarchicalImpliedModelParser(IImpliedModelParser parentParser, IImpliedModelParser childParser)
        {
            this.parentParser = parentParser;
            this.childParser = childParser;
        }

        public bool Matches(IModel model)
        {
            return parentParser.Matches(model) 
                && childParser.Matches(model);
        }

        public IModel Parse(IModel model)
        {
            IModel parsed = parentParser.Parse(model);
            HierarchyModel hParsed = parsed as HierarchyModel;
            if (hParsed != null)
            {
                IModel childModel = hParsed.ChildModels.Length > 0 ? hParsed.ChildModels[0] : null;
                if (childModel != null)
                {
                    IModel child = childParser.Parse(childModel);
                    return new HierarchyModel(hParsed.Model, new[] {child});
                }
            }
            return model;
        }
    }
}