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
                return new[] {Case1, Case2, Case3, Case4, Case5, Case6, Case7, Case8};
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
            ///     Single line property (11)
            /// </summary>
            public const string Case6 =
                "                    11  MSF061-DATA-1-061-1A.               [   5] Reference Data 1a";

            /// <summary>
            ///     Single line property (13)
            /// </summary>
            public const string Case7 =
                "                        13  MSF062-CONTRACT-NO-RC.          [  38] Contract Number";

            /// <summary>
            ///     Single line property (15)
            /// </summary>
            public const string Case8 =
                "                            15  MSF062-ACCOUNT-CODE-NA.     [  33] Account Code number                      ACCOUNT-CODE";

            /// <summary>
            ///     Single line property (17)
            /// </summary>
            public const string Case9 =
                "                               17  MSF062-DATA-2-062-PA.    [  33] Reference data 2";

            /// <summary>
            ///     Single line property (19)
            /// </summary>
            public const string Case10 =
                "                               19  MSF062-TOP-PAR-PA-PB.    [  33] Parent Account Code";

            /// <summary>
            ///     Single line property (21)
            /// </summary>
            public const string Case11 =
                "                               21  MSF062-DATA-2-062-WA.    [  33] Reference data 2";

            /// <summary>
            ///     Single line property (23)
            /// </summary>
            public const string Case12 =
                "                               23  MSF062-WLD-ACCT-WA-WB.   [  33] Account Code number                      ACCOUNT-CODE";

            /// <summary>
            ///     Single line property (25)
            /// </summary>
            public const string Case13 =
                "                               25  MSF062-DATA-2-062-WB.    [  33] Reference data 2";

            /// <summary>
            ///     Single line property (27)
            /// </summary>
            public const string Case14 =
                "                               27  MSF062-DATA-2-062-BA.    [  33] Reference data 2";

            /// <summary>
            ///     Multi line property (29)
            /// </summary>
            public const string Case15 =
                "                               29                           [  33] Account Code number                      ACCOUNT-CODE\n" +
                "                                MSF062-BUDG-ACCT-CODE-BA.";

            /// <summary>
            ///  Property followed by DataType
            /// </summary>
            public const string FollowingCase7 =
                "        05  LINE-NO.                                        [  19] Line No. of description                  DB,KEY:0\n" +
                "            07  LINE-NO-9       PIC 9(4).                   [  19] Line No. of description";

        }

        public static class DataType
        {
            public static string[] AllCases()
            {
                return new[]
                    {Case03SingleLine, Case05SingleLine, Case11SingleLine, Case13SingleLine, Case05MultipleLines};
            }

            public const string Case03SingleLine =
                "    03  TAX-PERIOD-CLOSED       PIC X(1).                   [  27] Tax Period Closed                        DB";

            public const string Case05SingleLine =
                "        05  FULL-PERIOD         PIC X(6).                   [   5] Full Period CCYYPP                       DB,KEY:0";

            public const string Case09SingleLine =
                "                09  AVAIL-SOH   PIC S9(9) COMP-3.           [1851] Available SOH";

            public const string Case11SingleLine =
                "                    11  SUBLEDGER-TYPE PIC X(2).            [  30] Subledger Account Type                   OPTIONAL TABLE ('SA')";

            public const string Case13SingleLine =
                "                        13  MSF061-MSB566-RUN-1A PIC X(4).  [   5] MSB566 Run Number                        NUMERIC RANGE (11)";

            public const string Case15SingleLine =
                "                            15  MSF062-REQ-NO-RC PIC X(6).  [  38] Requisition number";

            public const string Case05MultipleLines =
                "        05  DSTRCT-CODE         PIC X(4).                   [   1] District Code                            MANDATORY VALUE\n" +
                "                                                                                                            (DSTRCT-CODE) ERROR\n" +
                "                                                                                                            (6534) ACTIVE\n" +
                "                                                                                                            DB,KEY:0";

            public const string Case31MultipleLines =
                "                               31  MSF062-DATA-2-062-PB PIC [  33] Reference data 2\n" +
                "                                X(24).";
        }

        public static class EnumValue
        {
            public static string[] AllCases()
            {
                return new[] {Case07SingleLine, Case03SingleLine, Case03MultiLine};
            }

            public const string Case07SingleLine =
                "                88  MIMS-CONTROL VALUE 'M'.                        Indicates MIMS System Control Account";

            public const string Case09SingleLine =
                "                    88  EGI-TYPE VALUE 'G'.                        EGI type record";

            public const string Case11SingleLine =
                "                        88  RES-TY VALUE 'R'.                      Resource Type";

            public const string Case13MultiLine =
                "                            88  MSF062-ETP-TRAIN-PROG VALUE        Employee Training Plan Program\n" +
                "                                'P'.";

            public const string Case03SingleLine =
                "        88  PO-NO-ITEM          VALUE 'PO'.                        Purchase Order Number Item";

            public const string Case03MultiLine =
                "        88  TARGT-NO-AUTOGEN    VALUE 'N'.                         No Autogenerate Interdistrict Account\n" +
                "                                                                   Entries";
        }
    }
}