using System.Collections.Generic;
using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Readers;

namespace Ellipse.DataDictionary.Parsers
{
    public class HierarchyParser : IModelParser
    {
        private readonly IModelParser modelParser;
        private readonly IModelParser childrenParser;

        public HierarchyParser(IModelParser modelParser, params IModelParser[] childrenParser)
        {
            this.modelParser = modelParser;
            this.childrenParser = new CombinationParser(childrenParser);
        }

        public bool Matches(IReader reader)
        {
            if (modelParser.Matches(reader))
            {
                return true;
            }
            return false;
        }

        public Model Parse(IReader reader)
        {
            Model currentModel = modelParser.Parse(reader);
            Model childModel = childrenParser.Parse(reader);
            
            List<Model> children = new List<Model>();
            while (childModel != null)
            {
                children.Add(childModel);
                childModel = childrenParser.Parse(reader);
            }
            return new HierarchyModel(currentModel, children.ToArray());
        }
    }
}