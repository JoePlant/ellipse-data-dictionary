using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Xml;

namespace Ellipse.DataDictionary.Models
{
    public class ModelXmlFormatter
    {
        private readonly IModel formatModel;

        public ModelXmlFormatter(IModel formatModel)
        {
            this.formatModel = formatModel;
        }

        public string Render()
        {
            StringWriter writer = new StringWriter();
            XmlTextWriter textWriter = new XmlTextWriter(writer);
            textWriter.WriteStartDocument();
            Render(textWriter, formatModel, 0, "1");
            textWriter.WriteEndDocument();
            return writer.ToString();
        }

        private void Render(XmlTextWriter xmlWriter, IModel renderModel, int depth, string path)
        {
            HierarchyModel hierarchy = renderModel as HierarchyModel;
            if (hierarchy != null)
            {
                RenderHierarchy(xmlWriter, hierarchy, depth, path);
            }
            else
            {
                RenderModel(xmlWriter, renderModel, depth, path);
            }
        }

        private void RenderModel(XmlTextWriter xmlWriter, IModel renderModel, int depth, string path)
        {
            if (renderModel is HierarchyModel) throw new ArgumentException("Should not be a hierarchy model");

            IModel model = renderModel as IModel;

            if (model != null)
            {
                xmlWriter.WriteStartElement(model.Name);

                IDictionary<string, string> parts = model.GetModelParts();
                foreach (KeyValuePair<string, string> part in parts)
                {
                    xmlWriter.WriteAttributeString(part.Key, part.Value);
                }
                xmlWriter.WriteAttributeString("path", path);
                xmlWriter.WriteEndElement();
            }
        }

        private void RenderHierarchy(XmlTextWriter xmlWriter, HierarchyModel hierarchy, int depth, string path)
        {
            RenderModel(xmlWriter, hierarchy.Model, depth, path);

            int position = 0;
            foreach (var childModel in hierarchy.ChildModels)
            {
                position++;
                //xmlWriter.AppendLine();
                Render(xmlWriter, childModel, depth + 1, path + "." + position);
            }
        }
    }
} 