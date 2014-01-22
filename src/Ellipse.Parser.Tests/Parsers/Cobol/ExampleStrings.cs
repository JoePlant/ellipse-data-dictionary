namespace Ellipse.DataDictionary.Parsers.Cobol
{
    public static class ExampleStrings
    {
        public static class Class
        {
            public const string SingleLine01 = "01  MSF001-RECORD.";

            public static string[] AllCases()
            {
                return new[]
                    {
                        SingleLine01
                    };
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
                return new[]
                    {
                        SingleLine03, SingleLine07,
                        SingleLine11, SingleLine13, SingleLine15, SingleLine17, SingleLine19,
                        SingleLine21, SingleLine23, SingleLine25, SingleLine27,
                        MultiLine03, MultiLine05, MultiLine09, MultiLine29,
                        PropertyContainingReservedWord
                    };
            }

            /// <summary>
            ///     Single Line property (03)
            /// </summary>
            public const string SingleLine03 =
                "    03  KEY-004.                                            [   1] key of MSF004                            FK:0";

            /// <summary>
            ///     Multi-line Property (03)
            /// </summary>
            public const string MultiLine03 =
                "    03  END-DATE.                                           [  11] Ending date                              DATE\n" +
                "                                                                                                            DB";

            /// <summary>
            ///     Multi-line property (05)
            /// </summary>
            public const string MultiLine05 =
                "        05  CONTROL-ID.                                     [  29] ID's Subledger,MIMS Sys & InterComp Ctl  MANDATORY\n" +
                "                                                                                                            DB,KEY:0";

            /// <summary>
            ///     Single line property (07)
            /// </summary>
            public const string SingleLine07 =
                "            07  CONTROL-NUMBER.                             [  30] No Identifying MIMS System Ctl Account";

            /// <summary>
            ///     Multi line property (09)
            /// </summary>
            public const string MultiLine09 =
                "                09  INT-DSTRCT.                             [  30] InterDist Dist Code Ident. Target Dist   MANDATORY VALUE\n" +
                "                                                                                                            (DSTRCT-CODE) ERROR\n" +
                "                                                                                                            (6534) ACTIVE";

            /// <summary>
            ///     Single line property (11)
            /// </summary>
            public const string SingleLine11 =
                "                    11  MSF061-DATA-1-061-1A.               [   5] Reference Data 1a";

            /// <summary>
            ///     Single line property (13)
            /// </summary>
            public const string SingleLine13 =
                "                        13  MSF062-CONTRACT-NO-RC.          [  38] Contract Number";

            /// <summary>
            ///     Single line property (15)
            /// </summary>
            public const string SingleLine15 =
                "                            15  MSF062-ACCOUNT-CODE-NA.     [  33] Account Code number                      ACCOUNT-CODE";

            /// <summary>
            ///     Single line property (17)
            /// </summary>
            public const string SingleLine17 =
                "                               17  MSF062-DATA-2-062-PA.    [  33] Reference data 2";

            /// <summary>
            ///     Single line property (19)
            /// </summary>
            public const string SingleLine19 =
                "                               19  MSF062-TOP-PAR-PA-PB.    [  33] Parent Account Code";

            /// <summary>
            ///     Single line property (21)
            /// </summary>
            public const string SingleLine21 =
                "                               21  MSF062-DATA-2-062-WA.    [  33] Reference data 2";

            /// <summary>
            ///     Single line property (23)
            /// </summary>
            public const string SingleLine23 =
                "                               23  MSF062-WLD-ACCT-WA-WB.   [  33] Account Code number                      ACCOUNT-CODE";

            /// <summary>
            ///     Single line property (25)
            /// </summary>
            public const string SingleLine25 =
                "                               25  MSF062-DATA-2-062-WB.    [  33] Reference data 2";

            /// <summary>
            ///     Single line property (27)
            /// </summary>
            public const string SingleLine27 =
                "                               27  MSF062-DATA-2-062-BA.    [  33] Reference data 2";

            /// <summary>
            ///     Multi line property (29)
            /// </summary>
            public const string MultiLine29 =
                "                               29                           [  33] Account Code number                      ACCOUNT-CODE\n" +
                "                                MSF062-BUDG-ACCT-CODE-BA.";

            /// <summary>
            ///  Property followed by DataType
            /// </summary>
            public const string Property05DataType07 =
                "        05  LINE-NO.                                        [  19] Line No. of description                  DB,KEY:0\n" +
                "            07  LINE-NO-9       PIC 9(4).                   [  19] Line No. of description";

            public const string PropertyContainingReservedWord =
                "        05  LST-CON-PICK-NO.                                [  11] Last Consolidate Picking Slip Number     DB";
        }

        public static class DataType
        {
            public static string[] AllCases()
            {
                return new[]
                    {
                        SingleLine03, SingleLine05, SingleLine09,
                        SingleLine11, SingleLine13, SingleLine15,
                        MultiLines05, MultiLines31
                    };
            }

            public const string SingleLine03 =
                "    03  TAX-PERIOD-CLOSED       PIC X(1).                   [  27] Tax Period Closed                        DB";

            public const string SingleLine05 =
                "        05  FULL-PERIOD         PIC X(6).                   [   5] Full Period CCYYPP                       DB,KEY:0";

            public const string SingleLine09 =
                "                09  AVAIL-SOH   PIC S9(9) COMP-3.           [1851] Available SOH";

            public const string SingleLine11 =
                "                    11  SUBLEDGER-TYPE PIC X(2).            [  30] Subledger Account Type                   OPTIONAL TABLE ('SA')";

            public const string SingleLine13 =
                "                        13  MSF061-MSB566-RUN-1A PIC X(4).  [   5] MSB566 Run Number                        NUMERIC RANGE (11)";

            public const string SingleLine15 =
                "                            15  MSF062-REQ-NO-RC PIC X(6).  [  38] Requisition number";

            public const string MultiLines05 =
                "        05  DSTRCT-CODE         PIC X(4).                   [   1] District Code                            MANDATORY VALUE\n" +
                "                                                                                                            (DSTRCT-CODE) ERROR\n" +
                "                                                                                                            (6534) ACTIVE\n" +
                "                                                                                                            DB,KEY:0";


            public const string MultiLines31 =
                "                               31  MSF062-DATA-2-062-PB PIC [  33] Reference data 2\n" +
                "                                X(24).";
        }

        public static class EnumValue
        {
            public static string[] AllCases()
            {
                return new[]
                    {
                        SingleLine05, SingleLine09, SingleLine11,
                        SingleLine13, SingleLine15, MultiLine05, MultiLine07
                    };
            }

            public const string SingleLine05 =
                "        88  PO-NO-ITEM          VALUE 'PO'.                        Purchase Order Number Item";

            public const string SingleLine09 =
                "                88  MIMS-CONTROL VALUE 'M'.                        Indicates MIMS System Control Account";

            public const string SingleLine11 =
                "                    88  EGI-TYPE VALUE 'G'.                        EGI type record";

            public const string SingleLine13 =
                "                        88  RES-TY VALUE 'R'.                      Resource Type";

            public const string SingleLine15 =
                "                            88  MSF062-ETP-TRAIN-PROG VALUE        Employee Training Plan Program\n" +
                "                                'P'.";

            public const string MultiLine05 =
                "        88  TARGT-NO-AUTOGEN    VALUE 'N'.                         No Autogenerate Interdistrict Account\n" +
                "                                                                   Entries";

            public const string MultiLine07 =
                "            88  ERROR           VALUE '0001' THRU '9999'           Error\r\n" +
                "                                'A000' THRU 'Z999'.";
        }

        public static class Redefines
        {
            public static string[] AllCases()
            {
                return new[]
                    {
                        SingleLine03, MultiLine05, MultiLine07, SingleLine09, MultiLine11, MultiLine13
                    };
            }

            public const string SingleLine03 =
                "    03  DOS-PREF-RPT            REDEFINES DOS-PREF-GRP.     [   5] DOS Preferred Report";

            public const string MultiLine05 =
                "        05  WH-TABLE-CODE       REDEFINES TABLE-CODE.       [   5] Warehouse Table File Code                DATASET (MSF010) ERROR\n" +
                "                                                                                                            (0041)";

            public const string MultiLine07 =
                "            07  MSF062-DATA-1-062-CC REDEFINES              [   5] Reference data 1\n" +
                "                                MSF062-DATA-1-062-AU.";

            public const string SingleLine09 =
                "                09  ACCTYRMN    REDEFINES CONTROL-REC-NO-9. [   7] Accounting year and month";

            public const string MultiLine11 =
                "                    11  MSF061-DATA-1-061-CB REDEFINES      [   5] Reference data 1\n" +
                "                                MSF061-DATA-1-061-1A.";

            public const string MultiLine13 =
                "                        13  MSF062-PORT-ELE-RC REDEFINES    [  46] Portion Number and Element Number\n" +
                "                                MSF062-IREQ-ITEM-RC.";

            public const string Level03CompositeA =
                "    03  DC0028-REC              REDEFINES DO-REC.           [  11] District Cntrl Last Internal Invoice     ST,ID1:DC,ID2:0028\r\n" +
                "        05  LST-INT-INV-NO.                                 [  11] Last Internal Invoice Number Allocated   DB\r\n" +
                "            07  LST-INT-INV-NO-9 PIC 9(9).                  [  11] Last Internal Invoice Number Allocated\r\n" +
                "        05  FILLER              PIC X(255).";

            public const string Level03CompositeB =
                "    03  DC0027-REC              REDEFINES DO-REC.           [  11] District Cntrl Last Picking Slip         ST,ID1:DC,ID2:0027\r\n" +
                "        05  LST-CON-PICK-NO.                                [  11] Last Consolidate Picking Slip Number     DB\r\n" +
                "            07  LST-CON-PICK-NO-9 PIC 9(6).                 [  11] Last Consolidate Picking Slip Number\r\n" +
                "        05  FILLER              PIC X(258).";

            public const string Level07Composite =
                "             07  W000-DATA-RT    REDEFINES W000-DATA-GRP.    [  16] Work file data\r\n" +
                "                 09  W000-REDEF-REC.                         [  16] Work file data\r\n" +
                "                     11  W000-DATA PIC X(95) OCCURS 3        [  16] Ninety five bytes 0f work data\r\n" +
                "                                 INDEXED BY W000-DATA-IDX.";

            public const string Level03CompositeImplied =
                "    03  PROFILE-CHAR            REDEFINES PROFILE PIC X(1)  [ 106] Single character of a security profile.\r\n" +
                "                                OCCURS 250 INDEXED BY\r\n" +
                "                                PROFILE-CHAR-IDX.";

            public const string Level05CompositeImplied =
                "        05  LAST-PAR-WO-NO-9    REDEFINES LAST-PAR-WO-NO    [  51] Last parent work order number\r\n" +
                "                                PIC 9(8).";

            public const string Level05CompositeImpliedB =
                "        05  LAST-WO-NO-9        REDEFINES LAST-WO-NO PIC    [  59] Last work order number\r\n" +
                "                                9(8).";

            public const string Level05CompositeImpliedPlusValue =
                "        05  CONTROL-REC-IND     REDEFINES CONTROL-REC PIC   [   5] Control Record Indicator\r\n" +
                "                                X(6).\r\n" +
                "            88  DC0001-REC-IND  VALUE 'DC0001'.                    Indicator to use DC0001-REC definition";
        }

        public static class Occurs
        {
            public static string[] AllCases()
            {
                return new[]
                    {
                        MultiLine05, MultiLine07, MultiLine09
                    };
            }

            public const string MultiLine05 =
                "        05  ITEM-DDS135         OCCURS 60 INDEXED BY        [  86] Stock Item\r\n" +
                "                                DDS135-1-IDX.";

            public const string MultiLine07 =
                "            07  LO-LEVELS       OCCURS 5 INDEXED BY         [ 179] Geographical location levels data\r\n" +
                "                                LO-LEVELS-IDX.";

            public const string MultiLine09 =
                "                09  EX-CNT-DATA OCCURS 4 INDEXED BY         [  11] Expedite Count Data\r\n" +
                "                                EX-CNT-DATA-IDX.";

            public const string ImpliedMultiLine05 =
                "        05  ERROR-CODE          PIC X(4) OCCURS 10 INDEXED  [ 137] ERROR CODE                               DB\r\n" +
                "                                BY ERROR-CODE-IDX.\r\n" +
                "            88  ERROR           VALUE '0001' THRU '9999'           Error\r\n" +
                "                                'A000' THRU 'Z999'.";

            public const string ImpliedMultiLine07 =
                "           07  CURR-ACCUM-A    PIC X(1) OCCURS 10 INDEXED  [ 183] Current Period Accumulation Identifiers  DB\r\n" +
                "                               BY CURR-ACC-A-IDX.";

            public const string ImpliedMultiLine11 =
                "                     11  STOCK-SECTION PIC X(2) OCCURS 10    [  28] Internal Stock Sections (Positional)     DB\r\n" +
                "                                INDEXED BY\r\n" +
                "                                STOCK-SECTION-IDX.";

            public const string PropertyWithImpliedMultiLine11 =
                "                09  STOCK-SECTION-N.                        [  28] Group item for stock-section-n.\r\n"
                + ImpliedMultiLine11;

            public const string ImpliedMultiLines11B =
                "                     11  W000-DATA PIC X(95) OCCURS 3        [  16] Ninety five bytes 0f work data\r\n" +
                "                                 INDEXED BY W000-DATA-IDX.";

        }
    }
}