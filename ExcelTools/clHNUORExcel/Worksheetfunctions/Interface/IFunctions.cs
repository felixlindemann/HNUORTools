using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;

namespace ExcelTools
{ 

        [GuidAttribute("FF4AF1AD-7E2A-4611-AA6F-47351FF46AFD")]
        public interface IFunctions
        {
            double ADD(object x, object y);
            string XLVERSION();
            string THISCELL();
            bool PROMPTYESNO(string msg);
            string MANUALCALC(int show = 0);
            string RANDOMTABLE(int r, int c);


        } 
}
