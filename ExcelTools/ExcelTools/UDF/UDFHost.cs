using clHNUORExcel.BaseClasses;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;

namespace ExcelTools.UDF
{
    [Guid("E5EA268F-CD51-416C-B2F2-7D080C4182F2")]
    [ComVisible(true)]
    public interface IUDFHost
    {
        int MyInt
        {
            get;
            set;
        }
    }

    [Guid("E63025F9-E9D8-40B4-8C25-BDED6F68DF0D")]
    [ComVisible(true)]
    public class UDFHost : IUDFHost
    {
        private GeoSituation geo = new GeoSituation();
        public UDFHost()
        {
            MyInt = 0;
        }
        public int MyInt
        {
            get;
            set;
        }
    }
}
