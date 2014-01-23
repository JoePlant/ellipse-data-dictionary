using System;
using System.Collections.Generic;

namespace Ellipse.DataDictionary.Models
{
    public interface IBuilder
    {
        IModel Model();

        IBuilder With(IBuilder builder);

        IBuilder WithProperty(string name, string comment = null);

        IBuilder WithDataType(string name, string comment = null);

        IBuilder WithValue(string name, string comment = null);
    }

    public class Build
    {
        public static IBuilder Property(string name, string comment=null)
        {
            return new ModelBuilder(PropertyModel.Factory("Property", name, comment));
        }

        public static IBuilder DataType(string name, string comment = null)
        {
            return new ModelBuilder(DataTypeModel.Factory("DataType", name, comment));
        }

        public static IBuilder EnumValue(string name, string comment = null)
        {
            return new ModelBuilder(EnumValueModel.Factory("EnumValue", name, comment));
        }

        public static IBuilder Class(string name, string comment = null)
        {
            return new ModelBuilder(new CobolModel("Class", name, comment));
        }

        public static IBuilder Redefines(string name, string comment = null)
        {
            return new ModelBuilder(RedefinesModel.Factory("Redefines", name, comment));
        }

        public static IBuilder Occurs(string name, string comment = null)
        {
            return new ModelBuilder(new CobolModel("Occurs", name, comment));
        }

        private class ModelBuilder : IBuilder
        {
            private readonly CobolModel model;
            private readonly List<IBuilder> children = new List<IBuilder>();

            public ModelBuilder(IModel model)
            {
                this.model = model as CobolModel;
                if (this.model == null)
                {
                    throw new ArgumentException("model is not the correct type: " + model.GetType());
                }
            }

            public IModel Model()
            {
                List<IModel> childModels = new List<IModel>();
                foreach (IBuilder builder in children)
                {
                    IModel child = builder.Model();
                    if (child != null)
                    {
                        childModels.Add(child);
                    }
                }

                return childModels.Count > 0 
                    ? (IModel) new HierarchyModel(model, childModels.ToArray()) 
                    : model;
            }

            public IBuilder With(IBuilder builder)
            {
                children.Add(builder);
                return this;
            }

            public IBuilder WithProperty(string name, string comment = null)
            {
                children.Add(Property(name, comment));
                return this;
            }

            public IBuilder WithDataType(string name, string comment = null)
            {
                children.Add(DataType(name, comment));
                return this;
            }

            public IBuilder WithValue(string name, string comment = null)
            {
                children.Add(EnumValue(name, comment));
                return this;
            }
        }
    }
}