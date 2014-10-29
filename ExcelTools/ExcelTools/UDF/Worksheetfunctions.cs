
using Extensibility;
using Microsoft.Win32;

using Excel = Microsoft.Office.Interop.Excel;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;
using ExcelTools.UDF;
using clHNUORExcel.BaseClasses;

namespace ExcelTools
{

    [Guid("7F4BFC13-9750-44BA-9D17-C65C4D0C28C0")]
    [InterfaceType(ComInterfaceType.InterfaceIsDual)]
    [ComVisible(true)]
    public interface IWorksheetfunctions
    {
        int MYINT();
        string HelloWorld();
        string RANDOMTABLE(int r, int c);
    }

    [Guid("D59F54DF-C44D-4B45-B661-7E72E45CFA1C")]
    [ClassInterface(ClassInterfaceType.None)]
    [ProgId("HNU.OR.Worksheetfunctions")]
    [ComVisible(true)]
    public class Worksheetfunctions : IWorksheetfunctions, IDTExtensibility2
    {


        static IUDFHost mySharedClass = null;
        static Microsoft.Office.Interop.Excel.Application app = null;

        #region IDTExtensibility2

        public void OnAddInsUpdate(ref Array custom)
        {
        }

        public void OnBeginShutdown(ref Array custom)
        {
        }

        public void OnConnection(object Application, ext_ConnectMode ConnectMode, object AddInInst, ref Array custom)
        {
            try
            {

                app = Application as Microsoft.Office.Interop.Excel.Application;
                mySharedClass = app.COMAddIns.Item("ExcelTools").Object;


            }
            catch (Exception ex)
            {
                Debug.Print(ex.StackTrace);
                throw;
            }
        }
        public void OnDisconnection(ext_DisconnectMode RemoveMode, ref Array custom)
        {
        }

        public void OnStartupComplete(ref Array custom)
        {
        }

        [ComRegisterFunctionAttribute]
        public static void RegisterFunction(Type type)
        {
            try
            {
                Registry.ClassesRoot.CreateSubKey(GetSubKeyName(type, "Programmable"));
                RegistryKey key = Registry.ClassesRoot.OpenSubKey(GetSubKeyName(type, "InprocServer32"), true);
                key.SetValue("", System.Environment.SystemDirectory + @"\mscoree.dll", RegistryValueKind.String);

            }
            catch (Exception)
            {
                throw;
            }
        }

        [ComUnregisterFunctionAttribute]
        public static void UnregisterFunction(Type type)
        {
            Registry.ClassesRoot.DeleteSubKey(GetSubKeyName(type, "Programmable"), false);
        }

        private static string GetSubKeyName(Type type, string subKeyName)
        {
            System.Text.StringBuilder s = new System.Text.StringBuilder();
            s.Append(@"CLSID\{");
            s.Append(type.GUID.ToString().ToUpper());
            s.Append(@"}\");
            s.Append(subKeyName);

            Debug.Print("GetSubKeyName():" + s.ToString());
            return s.ToString();
        }

        #endregion

        #region demo
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
            try
            {

                // this gives s a reference to the exact cell with this function
                // what is importat to NOTE is that wihtout passing in a reference
                // to a Range object, I still have access to the full Excel
                // application Object Model becase we are registering this DLL
                // as an IDTExtensibility2 add-in. This interface, as you will see
                // below, provides us with a referene to the Application object
                // on the Connection...
                Excel.Range rng = (Excel.Range)app.get_Caller(1);
                // now - this is important to note. Excel will not allow you to
                // manipluate other cells or change certain items while you are
                // evaluating a function as part of a claculation event. The
                // best way to describe this is that Excel is in "Edit Cell"
                // mode. Attempting to manipulate the cell or any other cell
                // will throw an exception - so, we spawn a thread and do the 
                // work - later...

                new Thread(() =>
                {
                    // In this thread which will not execute until the current
                    // calculation chain is complete, we will bild a table full
                    // of random values.
                    for (int rowCnt = rng.Row + 1; rowCnt <= (rng.Row + r); rowCnt++)
                    {
                        for (int colCnt = rng.Column; colCnt < (rng.Column + c); colCnt++)
                        {
                            try
                            {
                                Excel.Range nextCell = ((Excel.Worksheet)rng.Parent).Cells[rowCnt, colCnt];
                                nextCell.Value2 = new Random().Next(999);
                                Marshal.ReleaseComObject(nextCell); // clean
                            }
                            catch (Exception)
                            {
                                // can't evaluate 
                                //   throw;
                            }
                        }
                    }
                    // important - release the range to prevent Excel hanging
                    // around after the user closes it
                    Marshal.FinalReleaseComObject(rng);
                }).Start();

                // simply retun a string to the active cell
                return "RANDOM TABLE";
            }
            catch (Exception ex)
            {

                return ex.StackTrace;
            }
        }

        #endregion

        #region Testing
        public int MYINT()
        {
            return mySharedClass.MyInt;
        }
        public string HelloWorld()
        {
            return "Hello World";
        }

        #endregion
         
        #region TPP

        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public string TPPNorthWestCornerRule()
        {
            try
            {

                Excel.Range rng = (Excel.Range)app.get_Caller(1);
                return getInitialSolution(rng, Transportplan.InitialMethod.NorthWestCornerRule);
            }
            catch (Exception ex)
            {
                Debug.Print(ex.ToString());
                return ex.Message;
            }
        }

        #region helper
        private string getInitialSolution(Excel.Range rng, Transportplan.InitialMethod im)
        {
            Excel.Worksheet ws = rng.Worksheet;
            Excel.Workbook wb = ws.Application.ThisWorkbook;

            GeoSituation geo = getGeoSituation(wb);
            Transportplan tpp = new Transportplan(geo);
            tpp = tpp.Solve(im);
            Marshal.FinalReleaseComObject(ws);
            Marshal.FinalReleaseComObject(wb);

            return writeTPP(rng, tpp);
        }

        private string writeTPP(Excel.Range rng, Transportplan tpp)
        {

            int I = tpp.Parent.Warehouses.Count;
            int J = tpp.Parent.Customers.Count;

            new Thread(() =>
               {
                   Excel.Range nextCell;
                   for (int i = 0; i < I; i++)
                   {
                       Warehouse w = tpp.Parent.Warehouses[i];
                       nextCell = ((Excel.Worksheet)rng.Parent).Cells[rng.Row + 2 + i, rng.Column + 0];
                       nextCell.Value2 = w.Id;
                       Marshal.ReleaseComObject(nextCell);

                       nextCell = ((Excel.Worksheet)rng.Parent).Cells[rng.Row + 2 + i, rng.Column + J + 1];
                       nextCell.Value2 = w.Supply;
                       Marshal.ReleaseComObject(nextCell);


                       for (int j = 0; j < J; j++)
                       {
                           if (i == 0)
                           {
                               Customer c = tpp.Parent.Customers[j];
                               nextCell = ((Excel.Worksheet)rng.Parent).Cells[rng.Row + 1, rng.Column + j + 1];
                               nextCell.Value2 = c.Id;
                               Marshal.ReleaseComObject(nextCell);

                               nextCell = ((Excel.Worksheet)rng.Parent).Cells[rng.Row + I + 2, rng.Column + j + 1];
                               nextCell.Value2 = c.Demand;
                               Marshal.ReleaseComObject(nextCell);

                           }

                           nextCell = ((Excel.Worksheet)rng.Parent).Cells[rng.Row + i + 2, rng.Column + j + 1];
                           nextCell.Value2 = tpp.X[i, j];
                           Marshal.ReleaseComObject(nextCell);
                       }
                   }
                   // important - release the range to prevent Excel hanging
                   // around after the user closes it
                   Marshal.FinalReleaseComObject(rng);
               }).Start();

            return tpp.Algorithm.ToString();
        }

        private GeoSituation getGeoSituation(Excel.Workbook wb)
        {

            GeoSituation geo = new GeoSituation();
            Excel.Worksheet wsC = wb.Worksheets["Customers"];
            Excel.Worksheet wsW = wb.Worksheets["Warehouses"];
            Excel.Worksheet wsCIJ = wb.Worksheets["TPP.CIJ"];
            int i = 1;
            while (wsC.Cells[i, 1].value != "")
            {
                Customer c = new Customer();
                c.Id = wsC.Cells[i, 1].value;
                c.Label = wsC.Cells[i, 2].value;
                try
                {
                    c.X = wsC.Cells[i, 3].value;
                }
                catch (Exception)
                {
                    c.X = 0;
                }

                try
                {
                    c.Y = wsC.Cells[i, 4].value;
                }
                catch (Exception)
                {
                    c.Y = 0;
                }

                try
                {
                    c.Demand = wsC.Cells[i, 5].value;
                }
                catch (Exception)
                {
                    c.Demand = 0;
                }
                geo.Customers.Add(c);
                i++;
            }
            int j = 1;
            while (wsW.Cells[j, 1].value != "")
            {
                Warehouse w = new Warehouse();
                w.Id = wsW.Cells[j, 1].value;
                w.Label = wsW.Cells[j, 2].value;
                try
                {
                    w.X = wsW.Cells[j, 3].value;
                }
                catch (Exception)
                {
                    w.X = 0;
                }

                try
                {
                    w.Y = wsW.Cells[j, 4].value;
                }
                catch (Exception)
                {
                    w.Y = 0;
                }

                try
                {
                    w.Supply = wsW.Cells[j, 5].value;
                }
                catch (Exception)
                {
                    w.Supply = 0;
                }
                try
                {
                    w.FixCosts = wsW.Cells[j, 6].value;
                }
                catch (Exception)
                {
                    w.FixCosts = 0;
                }
                geo.Warehouses.Add(w);
                i++;
            }

            return geo;
        }

        #endregion

        #endregion
         
    }
}
