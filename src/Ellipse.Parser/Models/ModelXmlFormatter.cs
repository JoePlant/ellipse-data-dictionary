using System;
using System.Collections.Generic;
using System.IO;
using System.Xml;

namespace Ellipse.DataDictionary.Models
{
    public class ModelXmlFormatter : IModelFormatter
    {
        private readonly IModel formatModel;

        public ModelXmlFormatter(IModel formatModel)
        {
            this.formatModel = formatModel;
        }

        public string Render()
        {
            StringWriter writer = new StringWriter();
            using (XmlWriter xmlWriter = XmlWriter.Create(writer,
                                                          new XmlWriterSettings
                                                              {
                                                                  Indent = true,
                                                                  OmitXmlDeclaration = true,
                                                                  NewLineOnAttributes = true
                                                              }))
            {
                Render(xmlWriter, formatModel, 0, "1");
                xmlWriter.Flush();
            }
            return writer.ToString();
        }

        private void Render(XmlWriter xmlWriter, IModel renderModel, int depth, string path)
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

        private void RenderModel(XmlWriter xmlWriter, IModel model, int depth, string path)
        {
            if (model is HierarchyModel) throw new ArgumentException("Should not be a hierarchy model");
            
            xmlWriter.WriteStartElement(model.Name);

            IDictionary<string, string> parts = model.GetModelParts();
            foreach (KeyValuePair<string, string> part in parts)
            {
                xmlWriter.WriteAttributeString(part.Key, part.Value);
            }
            xmlWriter.WriteAttributeString("path", path);
            xmlWriter.WriteEndElement();
        }

        private void RenderHierarchy(XmlWriter xmlWriter, HierarchyModel hierarchy, int depth, string path)
        {
            xmlWriter.WriteStartElement(hierarchy.Model.Name);

            IDictionary<string, string> parts = hierarchy.Model.GetModelParts();
            foreach (KeyValuePair<string, string> part in parts)
            {
                xmlWriter.WriteAttributeString(part.Key, part.Value);
            }
            xmlWriter.WriteAttributeString("path", path);

            int position = 0;
            foreach (var childModel in hierarchy.ChildModels)
            {
                position++;
                //xmlWriter.AppendLine();
                Render(xmlWriter, childModel, depth + 1, path + "." + position);
            }
            xmlWriter.WriteEndElement();
        }
    }
} 