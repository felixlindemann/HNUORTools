using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;

namespace ExcelTools
{

    [GuidAttribute("1C15FE64-ABB2-4A7A-9392-D1786B897C8F")]
    public interface IHNUExcelToolsTest
    {

        string HelloWorld();
        string HelloWho(Microsoft.Office.Interop.Excel.Range SourceRange);

    }

}
