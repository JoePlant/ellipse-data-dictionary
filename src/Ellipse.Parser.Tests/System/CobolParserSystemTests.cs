using System;
using Ellipse.DataDictionary.Parsers.Cobol;
using NUnit.Framework;

namespace Ellipse.DataDictionary.System
{
    [TestFixture]
    public class CobolParserSystemTests : ParserTestFixture<CobolParser>
    {
        [Test]
        public void MSF000_RECORD()
        {
            ParseFile(@".\Resources\Cobol\MSF000-RECORD.rpt");
        }

        [Test]
        public void MSF001_RECORD()
        {
            ParseFile(@".\Resources\Cobol\MSF001-RECORD.rpt");
        }

        [Test]
        public void MSF002_RECORD()
        {
            ParseFile(@".\Resources\Cobol\MSF002-RECORD.rpt");
        }

        [Test]
        public void MSF003_RECORD()
        {
            ParseFile(@".\Resources\Cobol\MSF003-RECORD.rpt");
        }

        [Test]
        public void MSF004_RECORD()
        {
            ParseFile(@".\Resources\Cobol\MSF004-RECORD.rpt");
        }

        [Test]
        public void MSF005_RECORD()
        {
            ParseFile(@".\Resources\Cobol\MSF005-RECORD.rpt");
        }

        [Test]
        public void MSF006_RECORD()
        {
            ParseFile(@".\Resources\Cobol\MSF006-RECORD.rpt");
        }

        [Test]
        public void MSF007_RECORD()
        {
            ParseFile(@".\Resources\Cobol\MSF007-RECORD.rpt");
        }

        [Test]
        public void MSF008_RECORD()
        {
            ParseFile(@".\Resources\Cobol\MSF008-RECORD.rpt");
        }

        [Test]
        public void MSF010_RECORD()
        {
            ParseFile(@".\Resources\Cobol\MSF010-RECORD.rpt");
        }

        [Test]
        public void MSF011_RECORD()
        {
            ParseFile(@".\Resources\Cobol\MSF011-RECORD.rpt");
        }

        [Test]
        public void MSF012_RECORD()
        {
            ParseFile(@".\Resources\Cobol\MSF012-RECORD.rpt");
        }

        [Test]
        public void MSF013_RECORD()
        {
            ParseFile(@".\Resources\Cobol\MSF013-RECORD.rpt");
        }

        [Test]
        public void MSF014_RECORD()
        {
            ParseFile(@".\Resources\Cobol\MSF014-RECORD.rpt");
        }

        [Test]
        public void MSF015_RECORD()
        {
            ParseFile(@".\Resources\Cobol\MSF015-RECORD.rpt");
        }

        [Test]
        public void MSF016_RECORD()
        {
            ParseFile(@".\Resources\Cobol\MSF016-RECORD.rpt");
        }

        [Test]
        public void MSF017_RECORD()
        {
            ParseFile(@".\Resources\Cobol\MSF017-RECORD.rpt");
        }

        [Test]
        public void MSF018_RECORD()
        {
            ParseFile(@".\Resources\Cobol\MSF018-RECORD.rpt");
        }

        [Test]
        public void MSF019_RECORD()
        {
            ParseFile(@".\Resources\Cobol\MSF019-RECORD.rpt");
        }

        [Test]
        public void MSF01A_RECORD()
        {
            ParseFile(@".\Resources\Cobol\MSF01A-RECORD.rpt");
        }

        [Test]
        public void MSF01B_RECORD()
        {
            ParseFile(@".\Resources\Cobol\MSF01B-RECORD.rpt");
        }

    }
}