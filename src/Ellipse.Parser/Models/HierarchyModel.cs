using System.Text;

namespace Ellipse.DataDictionary.Models
{
    public class HierarchyModel : Model, IModel
    {
        public HierarchyModel(Model model)
            : this(model, new Model[0])
        {
        }

        public HierarchyModel(Model model, Model[] childModels)
        {
            Model = model;
            ChildModels = childModels;
        }

        public Model GetModel(string path)
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

        public Model Model { get; private set; }
        public Model[] ChildModels { get; private set; }

        public override string ToString()
        {
            return new ModelFormatter(this).Render();
        }
    }
}