using ExcelTools.Worksheetfunctions;
using Extensibility;
using Microsoft.Office.Interop.Excel;
using Microsoft.Win32;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using Excel = Microsoft.Office.Interop.Excel;

namespace ExcelTools
{


    [GuidAttribute("30A29909-AF27-4814-9CBE-ED6A39A4B9A5"),
      ProgId("ExcelTools.Connect"),
    ClassInterface(ClassInterfaceType.AutoDual),
    ComDefaultInterface(typeof(IFunctions))]
    public class Connect : WorkSheetfunction, IFunctions
    {

        public Connect() { }// constructor

        /// <summary>
        /// Generates a random able in the row and cell just
        /// below the current cell - regardless of current
        /// contents
        /// </summary>
        /// <param name="r"></param>
        /// <param name="c"></param>
        /// <returns></returns>
        public string RANDOMTABLE(int r, int c)
        {
            Excel.Range rng = (Excel.Range)app.get_Caller(1);
            new Thread(() =>
            {
                for (int rowCnt = rng.Row + 1; rowCnt <= (rng.Row + r); rowCnt++)
                {
                    for (int colCnt = rng.Column; colCnt < (rng.Column + c); colCnt++)
                    {
                        Excel.Range nextCell = ((Excel.Worksheet)rng.Parent).Cells[rowCnt, colCnt];
                        nextCell.Value2 = new Random().Next(999).ToString();
                        Marshal.ReleaseComObject(nextCell);
                    }
                }
                Marshal.FinalReleaseComObject(rng);
            }).Start();
            return "RANDOM TABLE";
        }

        /// <summary>
        /// Turns off manu calculations
        /// </summary>
        /// <param name="show"></param>
        /// <returns></returns>
        public string MANUALCALC(int show = 0)
        {
            new Thread(() =>
            {
                app.Calculation = Excel.XlCalculation.xlCalculationManual;
                if (show >= 1)
                    MessageBox.Show("Calculation Disabled");

            }).Start();

            return "MANUAL";
        }

        /// <summary>
        /// Prompts the user with a Yes/No dialog and puts
        /// TRUE/FALSE in the resulting cell
        /// </summary>
        /// <param name="msg"></param>
        /// <returns></returns>
        public bool PROMPTYESNO(string msg)
        {
            DialogResult dr = MessageBox.Show(msg, "CALCULATION", MessageBoxButtons.YesNo);
            return dr == DialogResult.Yes;
        }

        /// <summary>
        /// ADDS two number, cells or values together
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        public double ADD(object x, object y)
        {
            double value1 = 0;
            double value2 = 0;

            if (x is Excel.Range)
                value1 = ((Excel.Range)x).Value2 != null ? ((Excel.Range)x).Value2 : 0;
            else
                value1 = double.Parse(x.ToString());
            if (y is Excel.Range)
                value2 = ((Excel.Range)y).Value2 != null ? ((Excel.Range)y).Value2 : 0;
            else
                value2 = double.Parse(y.ToString());

            return value1 + value2;
        }

        /// <summary>
        /// Places the version of the Excel application into
        /// the current cell
        /// </summary>
        /// <returns></returns>
        public string XLVERSION()
        {
            return app.Version;
        }

        /// <summary>
        /// Places the address of the cell being evaluated in
        /// the cell value
        /// </summary>
        /// <returns></returns>
        public string THISCELL()
        {
            Excel.Range rng = (Excel.Range)app.get_Caller(1);
            return rng.Address;
        }
         


    }
}
