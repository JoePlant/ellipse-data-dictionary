using System;
using Ellipse.DataDictionary.Parsers.Cobol;
using NUnit.Framework;

namespace Ellipse.DataDictionary.System
{
    [TestFixture]
    public class CobolParserSystemTests : ParserTestFixture<CobolParser>
    {
        [Test]
        public void MSF000_Record()
        {
            ParseFile(@".\Resources\Cobol\MSF000-RECORD.rpt");
        }

        [Test]
        public void MSF001_Record()
        {
            ParseFile(@".\Resources\Cobol\MSF001-RECORD.rpt");
        }

        [Test]
        public void MSF002_Record()
        {
            ParseFile(@".\Resources\Cobol\MSF002-RECORD.rpt");
        }

        [Test]
        public void MSF003_Record()
        {
            ParseFile(@".\Resources\Cobol\MSF003-RECORD.rpt");
        }

        [Test]
        public void MSF004_Record()
        {
            ParseFile(@".\Resources\Cobol\MSF004-RECORD.rpt");
        }

        [Test]
        public void MSF005_Record()
        {
            ParseFile(@".\Resources\Cobol\MSF005-RECORD.rpt");
        }

        [Test]
        public void MSF006_Record()
        {
            ParseFile(@".\Resources\Cobol\MSF006-RECORD.rpt");
        }
    }
}