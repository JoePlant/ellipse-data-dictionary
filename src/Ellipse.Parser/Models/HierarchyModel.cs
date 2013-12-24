

using System.Collections.Generic;

namespace Ellipse.DataDictionary.Models
{
    public class HierarchyModel : IModel
    {
        public HierarchyModel(IModel model)
            : this(model, new IModel[0])
        {
        }

        public HierarchyModel(IModel model, IModel[] childModels)
        {
            Model = model;
            ChildModels = childModels;
        }

        public string Name { get { return Model.Name; } }

        public IModel GetModel(string path)
        {
            if ((path == string.Empty) || (path == "1"))
            {
                return Model;
            }

            string[] parts = path.Split('.');
            if (parts[0] == "1")
            {
                if (parts.Length >= 2)
                {
                    int position = int.Parse(parts[1]);
                    if (position > 0 && position <= ChildModels.Length)
                    {
                        IModel model = ChildModels[position - 1] as IModel;
                        if (model != null)
                        {
                            parts[1] = "1";
                            return model.GetModel(string.Join(".", parts, 1, parts.Length - 1));
                        }
                    }
                }
            }
            return null;
        }

        public IDictionary<string, string> GetModelParts()
        {
            throw new System.NotImplementedException();
        }

        public IModel Model { get; private set; }
        public IModel[] ChildModels { get; private set; }

        public override string ToString()
        {
            return new ModelFormatter(this).Render();
        }
    }
}