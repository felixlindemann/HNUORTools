using ExcelTools.Worksheetfunctions;
using Extensibility;
using Microsoft.Win32;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;


namespace ExcelTools
{


    /// <summary>
    /// http://blogs.msdn.com/b/gabhan_berry/archive/2008/04/07/writing-custom-excel-worksheet-functions-in-c_2d00_sharp.aspx
    /// http://davecra.com/2013/06/08/update-creating-excel-udfs-in-c/
    /// https://excelvstoudfexample.codeplex.com/SourceControl/latest#ExcelFunctions/ExcelFunctions/Connect.cs
    /// </summary>
    ///
    [GuidAttribute("B90B1368-EE2A-4F7E-9913-F7685E214D72"),
    ProgId("ExcelTools.HNUExcelToolsTest"),
    ClassInterface(ClassInterfaceType.AutoDual),
    ComDefaultInterface(typeof(IHNUExcelToolsTest))]
    public class HNUExcelToolsTest : WorkSheetfunction, IHNUExcelToolsTest
    {


        public HNUExcelToolsTest() { }
        public string HelloWorld()
        {
            return "Hello World";
        }

        public string HelloWho(Microsoft.Office.Interop.Excel.Range SourceRange)
        {
            string s = "error";
            try
            {
                if (SourceRange.Rows.Count != 1)
                {
                    throw new NotSupportedException("This functions requires exactly one row for Input.");
                }
                if (SourceRange.Columns.Count != 1)
                {
                    throw new NotSupportedException("This functions requires exactly one column for Input.");
                }
                s = SourceRange.Value;


                s = "Hello " + s;
            }
            catch (Exception e)
            {

                s = e.Message;
            }

            return s;
        } 

    }
}
