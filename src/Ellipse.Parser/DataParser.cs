using System;
using System.Collections.Generic;
using System.Linq;
using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Parsers;
using Ellipse.DataDictionary.Readers;

namespace Ellipse.DataDictionary
{
    /// <summary>
    ///     Data Parser
    /// </summary>
    public class DataParser : IDataParser
    {
        private readonly IReader reader;
        private readonly IModelParser[] parsers;
        private readonly List<Model> results = new List<Model>();

 

        /// <summary>
        /// Initializes a new instance of the <see cref="DataParser"/> class.
        /// </summary>
        /// <param name="reader">The reader.</param>
        /// <param name="parsers">The parsers.</param>
        public DataParser(IReader reader, IModelParser[] parsers)
        {
            this.reader = reader;
            this.parsers = parsers;
        }

        /// <summary>
        /// Parses the data
        /// </summary>
        public void Parse()
        {
            while (!reader.EndOfFile)
            {
                IReader currentReader = reader;
                IModelParser parser = FindParser(reader, parsers);

                if (parser == null)
                {
                    currentReader = new TrimReader(reader);
                    parser = FindParser(currentReader, parsers);
                }

                if (parser == null)
                {
                    if (Corrections != null)
                    {
                        currentReader = new LookupReader(reader, Corrections);
                        parser = FindParser(currentReader, parsers);
                    }
                }

                if (parser == null)
                {
                    if (OnMissingParser != null)
                    {
                        if (OnMissingParser(reader.PeekNext()))
                        {
                            reader.ReadLine();
                        }
                    }
                }

                if (parser != null)
                {
                    int lineNo = currentReader.LineNumber;
                    Model model = parser.Parse(currentReader);
                    if (model != null)
                    {
                        results.Add(model);
                    }
                    else if (lineNo == currentReader.LineNumber)
                    {
                        currentReader.ReadLine();
                    }
                }
            }
        }

        public Func<string, bool> OnMissingParser { private get; set; }

        public IDictionary<string,string> Corrections { private get; set; }

        private static IModelParser FindParser(IReader reader, IEnumerable<IModelParser> parsers)
        {
            return parsers.FirstOrDefault(parser => parser.Matches(reader));
        }
    }
}