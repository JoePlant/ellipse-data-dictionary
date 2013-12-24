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
            return new ModelBuilder(new CobolModel("Property", name, comment));
        }

        public static IBuilder DataType(string name, string comment = null)
        {
            return new ModelBuilder(new CobolModel("DataType", name, comment));
        }

        public static IBuilder EnumValue(string name, string comment = null)
        {
            return new ModelBuilder(new CobolModel("EnumValue", name, comment));
        }

        public static IBuilder Class(string name, string comment = null)
        {
            return new ModelBuilder(new CobolModel("Class", name, comment));
        }

        public static IBuilder Redefines(string name, string comment = null)
        {
            return new ModelBuilder(new CobolModel("Redefines", name, comment));
        }

        private class ModelBuilder : IBuilder
        {
            private readonly CobolModel model;
            private readonly List<IBuilder> children = new List<IBuilder>();

            public ModelBuilder(CobolModel model)
            {
                this.model = model;
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