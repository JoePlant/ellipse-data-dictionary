using System.Collections.Generic;

namespace Ellipse.DataDictionary.Parsers.Cobol
{
    public static class Prefix
    {
        public readonly static string Prefix01      = "01  ";
        public readonly static string Prefix03      = "    03  ";
        public readonly static string Level0588     = "        88  ";
        public readonly static string Prefix05      = "        05  ";
        public readonly static string Level0788     = "            88  "; 
        public readonly static string Prefix07      = "            07  ";
        public readonly static string Level0988     = "                88  ";
        public readonly static string Prefix09      = "                09  ";
        public readonly static string Level1188     = "                    88  ";
        public readonly static string Prefix11      = "                    11  ";
        public readonly static string Level1388     = "                        88  "; 
        public readonly static string Prefix13      = "                        13  ";
        public readonly static string Level1588     = "                            88  ";
        public readonly static string Prefix15      = "                            15  ";
        public readonly static string Prefix17      = "                               17  ";
        public readonly static string Prefix19      = "                               19  ";
        public readonly static string Prefix21      = "                               21  ";
        public readonly static string Prefix23      = "                               23  ";
        public readonly static string Prefix25      = "                               25  ";
        public readonly static string Prefix27      = "                               27  ";
        public readonly static string Prefix29      = "                               29  ";
        public readonly static string Prefix31      = "                               31  ";
        public readonly static string Empty         = "                               ";

        private static readonly Dictionary<int, string> MarkerDictionary = new Dictionary<int, string>()
            {
                {1, "01"},
                {2, "03"},
                {3, "05"},
                {4, "07"},
                {5, "09"},
                {6, "11"},
                {7, "13"},
                {8, "15"},
                {9, "17"},
                {10, "19"},
                {11, "21"},
                {12, "23"},
                {13, "25"},
                {14, "27"},
                {15, "29"},
                {16, "31"},
                {88, "88"}

            };

        public static string Marker(int level)
        {
            string marker;
            return MarkerDictionary.TryGetValue(level, out marker) ? marker : null;
        }
    }
}