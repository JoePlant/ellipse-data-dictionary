using NUnit.Framework;

namespace Ellipse.DataDictionary.Models
{
    [TestFixture]
    public class PageHeaderModelUnitTests : TestFixture
    {
         [Test]
         public void CheckToString()
         {
             PageHeaderModel model = new PageHeaderModel(new string[] {"One", "Two", "Three"});

             string expected = string.Format("{0}One{1}{0}Two{1}{0}Three{1}", "[Page] ", "\r\n");
             Assert.That(model.ToString(), Is.EqualTo(expected));
         }
    }
}