using NUnit.Framework;

namespace Ellipse.DataDictionary.Readers
{
    [TestFixture]
    public class StringReaderUnitTests : TestFixture
    {
        [Test]
        public void PeekEmptyString()
        {
            IReader reader = new StringReader("");
            Assert.That(reader.PeekAhead(0), Is.EqualTo(null));
            Assert.That(reader.PeekAhead(+1), Is.Null);
            Assert.That(reader.LineNumber, Is.EqualTo(1));
            Assert.That(reader.EndOfFile, Is.True);
        }

        [Test]
        public void PeekString()
        {
            IReader reader = new StringReader("One");
            Assert.That(reader.PeekAhead(0), Is.EqualTo("One"));
            Assert.That(reader.PeekAhead(+1), Is.Null);
            Assert.That(reader.LineNumber, Is.EqualTo(1));
            Assert.That(reader.EndOfFile, Is.False);
            Assert.That(reader.ReadLine(), Is.EqualTo("One"));
            Assert.That(reader.EndOfFile, Is.True);
        }

        [Test]
        public void IsEndOfFileSingleLine()
        {
            IReader reader = new StringReader("One");
            Assert.That(reader.PeekAhead(0), Is.EqualTo("One"));
            Assert.That(reader.PeekAhead(+1), Is.Null);
            Assert.That(reader.IsEndOfFile(0), Is.False);
            Assert.That(reader.IsEndOfFile(1), Is.True);
            Assert.That(reader.IsEndOfFile(2), Is.True);
            Assert.That(reader.EndOfFile, Is.False);

            Assert.That(reader.ReadLine(), Is.EqualTo("One"));
            Assert.That(reader.IsEndOfFile(0), Is.True);
            Assert.That(reader.PeekAhead(0), Is.Null);
            Assert.That(reader.PeekAhead(1), Is.Null);
            Assert.That(reader.IsEndOfFile(0), Is.True);
            Assert.That(reader.IsEndOfFile(1), Is.True);
            Assert.That(reader.IsEndOfFile(2), Is.True);
            Assert.That(reader.EndOfFile, Is.True);
        }

        [Test]
        public void IsEndOfFileMulitpleLines()
        {
            IReader reader = new StringReader("One\r\nTwo");
            Assert.That(reader.PeekAhead(0), Is.EqualTo("One"));
            Assert.That(reader.PeekAhead(+1), Is.EqualTo("Two"));
            Assert.That(reader.PeekAhead(+2), Is.Null);
            Assert.That(reader.LineNumber, Is.EqualTo(1));
            Assert.That(reader.IsEndOfFile(0), Is.False);
            Assert.That(reader.IsEndOfFile(1), Is.False);
            Assert.That(reader.IsEndOfFile(2), Is.True);
            Assert.That(reader.EndOfFile, Is.False);

            Assert.That(reader.ReadLine(), Is.EqualTo("One"));
            Assert.That(reader.IsEndOfFile(0), Is.False);
            Assert.That(reader.IsEndOfFile(1), Is.True);
            Assert.That(reader.IsEndOfFile(2), Is.True);
            Assert.That(reader.EndOfFile, Is.False);

            Assert.That(reader.ReadLine(), Is.EqualTo("Two"));
            Assert.That(reader.IsEndOfFile(0), Is.True);
            Assert.That(reader.IsEndOfFile(1), Is.True);
            Assert.That(reader.IsEndOfFile(2), Is.True);
            Assert.That(reader.EndOfFile, Is.True);
        }


        [Test]
        public void ReadEmptyString()
        {
            IReader reader = new StringReader("");
            Assert.That(reader.LineNumber, Is.EqualTo(1));
            Assert.That(reader.ReadLines(1), Is.EqualTo(new string[] {}));
            Assert.That(reader.PeekAhead(0), Is.Null);
            Assert.That(reader.LineNumber, Is.EqualTo(1));

            Assert.That(reader.ReadLines(1), Is.EqualTo(new string[] {}));
            Assert.That(reader.LineNumber, Is.EqualTo(1));
        }

        [Test]
        public void EndOfFile()
        {
            IReader reader = new StringReader("One");
            Assert.That(reader.EndOfFile, Is.False);
            Assert.That(reader.ReadLines(1), Is.EqualTo(new string[] {"One"}));
            Assert.That(reader.EndOfFile, Is.True);
        }

        [Test]
        public void EndOfFileWithContent()
        {
            IReader reader = new StringReader("One\r\nTwo\r\nThree\r\n");
            Assert.That(reader.EndOfFile, Is.False);
            Assert.That(reader.ReadLines(2), Is.EqualTo(new[] {"One", "Two"}));
            Assert.That(reader.EndOfFile, Is.False);
            Assert.That(reader.ReadLines(2), Is.EqualTo(new[] {"Three"}));
            Assert.That(reader.EndOfFile, Is.True);
        }

        [Test]
        public void ReadLine()
        {
            IReader reader = new StringReader("One\r\nTwo\r\nThree\r\nFour");
            Assert.That(reader.LineNumber, Is.EqualTo(1));
            Assert.That(reader.ReadLines(1), Is.EqualTo(new[] {"One"}));
            Assert.That(reader.PeekAhead(0), Is.EqualTo("Two"));
            Assert.That(reader.LineNumber, Is.EqualTo(2));
            Assert.That(reader.ReadLines(2), Is.EqualTo(new[] {"Two", "Three"}));

            Assert.That(reader.EndOfFile, Is.False);

            Assert.That(reader.PeekAhead(0), Is.EqualTo("Four"));
            Assert.That(reader.LineNumber, Is.EqualTo(4));
            Assert.That(reader.EndOfFile, Is.False);
            Assert.That(reader.ReadLines(2), Is.EqualTo(new[] {"Four"}));
            Assert.That(reader.EndOfFile, Is.True);
            Assert.That(reader.LineNumber, Is.EqualTo(5));
        }

        [Test]
        public void TestToString()
        {
            IReader reader = new StringReader("One\r\nTwo\r\nThree\r\nFour");
            Assert.That(reader.ToString(), Is.EqualTo("Line: 1 'One'"));
            reader.ReadLine();
            Assert.That(reader.ToString(), Is.EqualTo("Line: 2 'Two'"));
            reader.ReadLine();
            Assert.That(reader.ToString(), Is.EqualTo("Line: 3 'Three'"));
            reader.ReadLine();
            Assert.That(reader.ToString(), Is.EqualTo("Line: 4 'Four'"));
            reader.ReadLine();
            Assert.That(reader.ToString(), Is.EqualTo("End of file: 4 lines"));
        }

    }
}