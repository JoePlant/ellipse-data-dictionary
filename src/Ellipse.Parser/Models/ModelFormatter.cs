using System;
using System.Text;

namespace Ellipse.DataDictionary.Models
{
    public class ModelFormatter
    {
        private readonly Model model;

        public ModelFormatter(Model model)
        {
            this.model = model;
            IncludeModelPaths = false;
        }

        public bool IncludeModelPaths { private get; set; }

        public string Render()
        {
            StringBuilder builder = new StringBuilder();
            Render(builder, model, 0, "1");
            return builder.ToString();
        }

        private void Render(StringBuilder builder, Model renderModel, int depth, string path)
        {
            HierarchyModel hierarchy = renderModel as HierarchyModel;
            if (hierarchy != null)
            {
                RenderHierarchy(builder, hierarchy, depth, path);
            }
            else
            {
                RenderModel(builder, renderModel, depth, path);
            }
        }

        private void RenderModel(StringBuilder builder, Model renderModel, int depth, string path)
        {
            if (renderModel is HierarchyModel) throw new ArgumentException("Should not be a hierarchy model");
            if (IncludeModelPaths)
            {
                builder.AppendFormat("{0}: ", path);
            }
            else
            {
                builder.AppendFormat("{0}", new string(' ', 2 * depth));
            }
            builder.Append(renderModel);
        }

        private void RenderHierarchy(StringBuilder builder, HierarchyModel hierarchy, int depth, string path)
        {
            RenderModel(builder, hierarchy.Model, depth, path);

            int position = 0;
            foreach (var childModel in hierarchy.ChildModels)
            {
                position++;
                builder.AppendLine();
                Render(builder, childModel, depth + 1, path + "." + position);
            }
        }
    }
} 