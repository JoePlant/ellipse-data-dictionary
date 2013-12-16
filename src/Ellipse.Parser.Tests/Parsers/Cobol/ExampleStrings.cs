namespace Ellipse.DataDictionary.Parsers.Cobol
{
    public static class ExampleStrings
    {
        public static class Class
        {
            public const string Case1 = "01  MSF001-RECORD.";

            public static string[] AllCases()
            {
                return new[] { Case1 };
            }
        }

        public static class Property
        {
            /// <summary>
            ///     Simple cases
            /// </summary>
            /// <returns></returns>
            public static string[] SimpleCases()
            {
                return new[] {Case1, Case2, Case3, Case4, Case5};
            }

            /// <summary>
            ///     Single Line property (03)
            /// </summary>
            public const string Case1 =
                "    03  KEY-004.                                            [   1] key of MSF004                            FK:0";

            /// <summary>
            ///     Multi-line Property (03)
            /// </summary>
            public const string Case2 =
                "    03  END-DATE.                                           [  11] Ending date                              DATE\n" +
                "                                                                                                            DB";

            /// <summary>
            ///     Multi-line property (05)
            /// </summary>
            public const string Case3 =
                "        05  CONTROL-ID.                                     [  29] ID's Subledger,MIMS Sys & InterComp Ctl  MANDATORY\n" +
                "                                                                                                            DB,KEY:0";

            /// <summary>
            ///     Single line property (07)
            /// </summary>
            public const string Case4 =
                "            07  CONTROL-NUMBER.                             [  30] No Identifying MIMS System Ctl Account";

            /// <summary>
            ///     Multi line property (09)
            /// </summary>
            public const string Case5 =
                "                09  INT-DSTRCT.                             [  30] InterDist Dist Code Ident. Target Dist   MANDATORY VALUE\n" +
                "                                                                                                            (DSTRCT-CODE) ERROR\n" +
                "                                                                                                            (6534) ACTIVE";

            /// <summary>
            ///  Property followed by DataType
            /// </summary>
            public const string FollowingCase6 =
                "        05  LINE-NO.                                        [  19] Line No. of description                  DB,KEY:0\n" +
                "            07  LINE-NO-9       PIC 9(4).                   [  19] Line No. of description";

        }

        public static class DataType
        {
            public static string[] AllCases()
            {
                return new[] { Case1, Case2, Case3, Case4 };
            }

            public const string Case1 =
                "    03  TAX-PERIOD-CLOSED       PIC X(1).                   [  27] Tax Period Closed                        DB";

            public const string Case2 =
                "        05  FULL-PERIOD         PIC X(6).                   [   5] Full Period CCYYPP                       DB,KEY:0";

            public const string Case3 =
                "                    11  SUBLEDGER-TYPE PIC X(2).            [  30] Subledger Account Type                   OPTIONAL TABLE ('SA')";


            public const string Case4 =
                "        05  DSTRCT-CODE         PIC X(4).                   [   1] District Code                            MANDATORY VALUE\n" +
                "                                                                                                            (DSTRCT-CODE) ERROR\n" +
                "                                                                                                            (6534) ACTIVE\n" +
                "                                                                                                            DB,KEY:0";
        }

        public static class EnumValue
        {
            public static string[] AllCases()
            {
                return new[] {Case1, Case2, Case3};
            }

            public const string Case1 = 
                "                88  MIMS-CONTROL VALUE 'M'.                        Indicates MIMS System Control Account";

            public const string Case2 =
                "        88  PO-NO-ITEM          VALUE 'PO'.                        Purchase Order Number Item";

            public const string Case3 =
                "        88  TARGT-NO-AUTOGEN    VALUE 'N'.                         No Autogenerate Interdistrict Account\n" +
                "                                                                   Entries";
        }
    }
}