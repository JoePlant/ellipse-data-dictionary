using System;
using Ellipse.DataDictionary.Models;
using Ellipse.DataDictionary.Readers;
using NUnit.Framework;

namespace Ellipse.DataDictionary.Parsers.Models
{
    [TestFixture]
    public class PageHeaderParserUnitTests : TestFixture
    {
        private const string lineFeed = "\n";

        private readonly string[] pageHeader = new string[]
                {
                    "",
                    "",
                    "   Created in 3.052h from 3.052g on 09-Nov-01 by deborahb                   18th February, 2004 20:17:25                  Page    1",
                    "",
                    "Dictionary file : /ELLIPSE/EL5.2.3_VER/DATDCT/DATA.DCT",
                    ""
                };

        private IModelParser parser;

        protected override void OnSetUp()
        {
            parser = new PageHeaderParser();
            base.OnSetUp();
        }

        [Test]
        public void ParseIncompleteHeader()
        {
            string text = string.Join(lineFeed, pageHeader);

            string incomplete = text.Substring(0, text.IndexOf("Dictionary", StringComparison.InvariantCulture));

            Assert.That(incomplete, Is.Not.EqualTo(text));

            IReader reader = Reader.CreateStringReader(incomplete);

            Assert.That(reader.LineNumber, Is.EqualTo(1));

            Assert.That(parser.Matches(reader), Is.False);

            IModel model = parser.Parse(reader);
            Assert.That(model, Is.Null);
            
            Assert.That(reader.LineNumber, Is.EqualTo(1));
            Assert.That(reader.EndOfFile, Is.False);
        }

        [Test]
        public void ParseHeaderExactly()
        {
            string text = string.Join(lineFeed, pageHeader);

            IReader reader = Reader.CreateStringReader(text + "\n");

            Assert.That(reader.LineNumber, Is.EqualTo(1));

            Assert.That(parser.Matches(reader), Is.True);

            IModel model = parser.Parse(reader);
            Assert.That(model, Is.Not.Null);

            Assert.That(model, Is.InstanceOf<PageHeaderModel>());

            Assert.That(reader.LineNumber, Is.EqualTo(7));
            Assert.That(reader.EndOfFile, Is.True);
        }

        [Test]
        public void ParseHeaderWithFollowing()
        {
            string text = string.Join(lineFeed, pageHeader);
            string completeText = text + lineFeed + "Following";

            Assert.That(completeText, Is.StringStarting(text));

            IReader reader = Reader.CreateStringReader(completeText);

            Assert.That(reader.LineNumber, Is.EqualTo(1));

            Assert.That(parser.Matches(reader), Is.True);

            IModel model = parser.Parse(reader);
            Assert.That(model, Is.Not.Null);

            Assert.That(model, Is.InstanceOf<PageHeaderModel>());

            Assert.That(reader.LineNumber, Is.EqualTo(7));
            Assert.That(reader.EndOfFile, Is.False);

            Assert.That(reader.PeekAhead(0), Is.EqualTo("Following"));
        }

    }
}