using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Linq;
using Excel = Microsoft.Office.Interop.Excel;
using Office = Microsoft.Office.Core;
using Microsoft.Office.Tools.Excel;
using System.Runtime.InteropServices;
using Microsoft.Win32;
using System.Windows.Forms;
using System.Diagnostics;
using Extensibility;
using ExcelTools.UDF;
using clHNUORExcel.BaseClasses;
using System.Reflection;
using System.Threading;

namespace ExcelTools
{
    public partial class ThisAddIn
    {

        #region Constants

        private List<string> names = (new string[] { "Nodes", "Customers", "Warehouses", "Links", "TPP" }).ToList();

        private const int MatrixOffset_Column = 2;
        private const int MatrixOffset_Row = 3;

        #endregion

        #region Properties

        public UDFHost _udf = null;
        public GeoSituation geo { get; set; }

        public Ribbon1 rib { get; set; }

        #endregion

        #region Init

        #region Worksheetfunction

        protected override object RequestComAddInAutomationService()
        {
            if (_udf == null)
            {
                _udf = new UDFHost();
            }
            return _udf;
        }
        #endregion

        private void ThisAddIn_Startup(object sender, System.EventArgs e)
        {
            try
            {
                Random rnd = new Random();
                if (_udf == null)
                {
                    _udf = new UDFHost();
                }
                _udf.MyInt = rnd.Next(1, 11);
                geo = new GeoSituation();

            }
            catch (Exception ex)
            {
                Debug.Print(ex.StackTrace);
                throw;
            }

        }

        private void ThisAddIn_Shutdown(object sender, System.EventArgs e)
        {
        }

        #region Von VSTO generierter Code

        /// <summary>
        /// Erforderliche Methode für die Designerunterstützung.
        /// Der Inhalt der Methode darf nicht mit dem Code-Editor geändert werden.
        /// </summary>
        private void InternalStartup()
        {
            this.Startup += new System.EventHandler(ThisAddIn_Startup);
            this.Shutdown += new System.EventHandler(ThisAddIn_Shutdown);
        }

        #endregion

        #endregion

        #region WorksheetHelper

        public void PrepareWorkbook(Boolean formatedOutput = false)
        {
            try
            {
                Excel.Application excelApp = this.Application;


                Excel.Workbook wb;
                if (excelApp.Workbooks.Count == 0)
                {
                    wb = excelApp.Workbooks.Add();
                }
                wb = excelApp.ActiveWorkbook;
                Excel.Worksheet ws = null;


                for (int i = 0; i < names.Count; i++)
                {
                    if (wb.Worksheets.Count < i + 1)
                    {
                        ws = (Excel.Worksheet)excelApp.Worksheets.Add(Missing.Value, Missing.Value,
                            Missing.Value, Missing.Value);
                    }
                    else
                    {
                        ws = excelApp.Worksheets[i + 1];
                    }
                    ws.Name = names[i];
                    ws.Cells.Clear();
                }

            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, "Error during preparing Worksheet");
            }

        }

        public void OutPutCustomers(Boolean formatedOutput = false)
        {
            Excel.Workbook wb = this.Application.ActiveWorkbook;
            Excel.Worksheet ws = wb.Worksheets[names[1]];

            string[] Colnames = new string[] { "id", "label", "x", "y", "demand", "isDummy" };


            if (formatedOutput)
            {
                new Thread(() =>
                {
                    for (int i = 0; i < geo.Customers.Count; i++)
                    {
                        if (i == 0)
                        {
                            for (int j = 0; j < Colnames.Length; j++)
                            {
                                ((Excel.Range)ws.Cells[i + 1, j + 1]).Value2 = Colnames[j];
                            }
                        }
                        Customer o = geo.Customers[i];
                        ((Excel.Range)ws.Cells[i + 2, 1]).Value2 = o.Id;
                        ((Excel.Range)ws.Cells[i + 2, 2]).Value2 = o.Label;
                        ((Excel.Range)ws.Cells[i + 2, 3]).Value2 = o.X;
                        ((Excel.Range)ws.Cells[i + 2, 4]).Value2 = o.Y;
                        ((Excel.Range)ws.Cells[i + 2, 5]).Value2 = o.Demand;
                        ((Excel.Range)ws.Cells[i + 2, 6]).Value2 = o.IsDummy;

                    }
                }).Start();
            }
            else
            {
                object[,] values = (object[,])Array.CreateInstance(typeof(object), new int[2] { geo.Customers.Count + 1, Colnames.Length }, new int[2] { 1, 1 });

                for (int i = 0; i < geo.Customers.Count; i++)
                {
                    if (i == 0)
                    {
                        for (int j = 0; j < Colnames.Length; j++)
                        {
                            values[i + 1, j + 1] = Colnames[j];
                        }
                    }
                    Customer o = geo.Customers[i];
                    values[i + 2, 1] = o.Id;
                    values[i + 2, 2] = o.Label;
                    values[i + 2, 3] = o.X;
                    values[i + 2, 4] = o.Y;
                    values[i + 2, 5] = o.Demand;
                    values[i + 2, 6] = o.IsDummy;
                }
                Excel.Range cells = (Excel.Range)ws.Range[ws.Cells[MatrixOffset_Row, MatrixOffset_Column],
                                                 ws.Cells[MatrixOffset_Row + geo.Customers.Count, MatrixOffset_Column + Colnames.Length]];
                cells.Value2 = values;
            }
        }

        public void OutPutWarehouses(Boolean formatedOutput = false)
        {
            Excel.Workbook wb = this.Application.ActiveWorkbook;
            Excel.Worksheet ws = wb.Worksheets[names[2]];

            string[] Colnames = new string[] { "id", "label", "x", "y", "supply", "FixCosts", "isOpen", "isDummy" };
            if (formatedOutput)
            {
                new Thread(() =>
                {
                    if (formatedOutput)
                    {
                        for (int i = 0; i < geo.Warehouses.Count; i++)
                        {
                            if (i == 0)
                            {
                                for (int j = 0; j < Colnames.Length; j++)
                                {
                                    ((Excel.Range)ws.Cells[i + 1, j + 1]).Value2 = Colnames[j];
                                }
                            }
                            Warehouse o = geo.Warehouses[i];
                            ((Excel.Range)ws.Cells[i + 2, 1]).Value2 = o.Id;
                            ((Excel.Range)ws.Cells[i + 2, 2]).Value2 = o.Label;
                            ((Excel.Range)ws.Cells[i + 2, 3]).Value2 = o.X;
                            ((Excel.Range)ws.Cells[i + 2, 4]).Value2 = o.Y;
                            ((Excel.Range)ws.Cells[i + 2, 5]).Value2 = o.Supply;
                            ((Excel.Range)ws.Cells[i + 2, 6]).Value2 = o.FixCosts;
                            ((Excel.Range)ws.Cells[i + 2, 7]).Value2 = o.IsOpen;
                            ((Excel.Range)ws.Cells[i + 2, 8]).Value2 = o.IsDummy;

                        }
                    }
                    else
                    {


                    }
                }).Start();
            }
            else
            {
                object[,] values = (object[,])Array.CreateInstance(typeof(object), new int[2] { geo.Warehouses.Count + 1, Colnames.Length }, new int[2] { 1, 1 });

                for (int i = 0; i < geo.Warehouses.Count; i++)
                {
                    if (i == 0)
                    {
                        for (int j = 0; j < Colnames.Length; j++)
                        {
                            values[i + 1, j + 1] = Colnames[j];
                        }
                    }
                    Warehouse o = geo.Warehouses[i];
                    values[i + 2, 1] = o.Id;
                    values[i + 2, 2] = o.Label;
                    values[i + 2, 3] = o.X;
                    values[i + 2, 4] = o.Y;
                    values[i + 2, 5] = o.Supply;
                    values[i + 2, 6] = o.FixCosts;
                    values[i + 2, 7] = o.IsOpen;
                    values[i + 2, 8] = o.IsDummy;
                }
                Excel.Range cells = (Excel.Range)ws.Range[ws.Cells[MatrixOffset_Row, MatrixOffset_Column],
                                                 ws.Cells[MatrixOffset_Row + geo.Warehouses.Count, MatrixOffset_Column + Colnames.Length]];
                cells.Value2 = values;
            }
        }

        public void OutPutLinks(Boolean formatedOutput = false)
        {
            // setup Links

            Excel.Workbook wb = this.Application.ActiveWorkbook;
            Excel.Worksheet ws = wb.Worksheets[names[3]];

            string[] Colnames = new string[] { "id", "label", "From", "To", "distance", "costs", "IsOneWay", "IsUsed" };

            new Thread(() =>
            {
                for (int i = 0; i < geo.Links.Count; i++)
                {
                    if (i == 0)
                    {
                        for (int j = 0; j < Colnames.Length; j++)
                        {
                            ((Excel.Range)ws.Cells[i + 1, j]).Value2 = Colnames[j];
                        }
                    }
                    Link o = geo.Links[i];
                    ((Excel.Range)ws.Cells[i + 2, 1]).Value2 = o.Id;
                    ((Excel.Range)ws.Cells[i + 2, 2]).Value2 = o.Label;
                    ((Excel.Range)ws.Cells[i + 2, 3]).Value2 = o.OriginNode.Id;
                    ((Excel.Range)ws.Cells[i + 2, 4]).Value2 = o.DestinationNode.Id;
                    ((Excel.Range)ws.Cells[i + 2, 5]).Value2 = o.Distance;
                    ((Excel.Range)ws.Cells[i + 2, 6]).Value2 = o.Costs;
                    ((Excel.Range)ws.Cells[i + 2, 7]).Value2 = o.IsOneWay;
                    ((Excel.Range)ws.Cells[i + 2, 8]).Value2 = o.IsUsed;

                }
            }).Start();
        }

        public void OutPutTransportplan(Boolean formatedOutput = false)
        {
            //Turn off Excel updating
            SwitchApplicationSettings(false);
            Excel.Workbook wb = this.Application.ActiveWorkbook;
            Excel.Worksheet ws = wb.Worksheets[names[0]];
            for (int i = wb.Worksheets.Count; i > 0; i--)
            {
                ws = wb.Worksheets[i];
                if (ws.Name.StartsWith(names[4]))
                {
                    ws.Delete();
                    Marshal.ReleaseComObject(ws); // clean
                }
            }

            for (int t = 0; t < geo.Transportplans.Count; t++)
            {
                Debug.Print("  TPP." + t);
                if (formatedOutput)
                {
                    OutPutTransportplanFormated(t);
                }
                else
                {
                    OutPutTransportplanUnFormated(t);
                }
            }

            SwitchApplicationSettings(true);
        }

        private void OutPutTransportplanUnFormated(int t)
        {

            if (t >= this.geo.Transportplans.Count) return;
            Transportplan tpp = this.geo.Transportplans[t];
            Excel.Workbook wb = this.Application.ActiveWorkbook;

            int I = tpp.Parent.Warehouses.Count();
            int J = tpp.Parent.Customers.Count();

            string wsname = names[4] + "." + tpp.Iteration + "." + tpp.Algorithm;
            // setup Transportplan
            Excel.Worksheet ws = wb.Worksheets.Add(Missing.Value, Missing.Value, Missing.Value, Missing.Value);
            ws.Name = wsname;
            ws.Cells.Interior.Pattern = Excel.XlPattern.xlPatternSolid;
            ws.Cells.Interior.PatternColor = System.Drawing.ColorTranslator.ToOle(System.Drawing.Color.White);

            ws.Cells.Borders[Excel.XlBordersIndex.xlInsideVertical].LineStyle = Excel.XlLineStyle.xlLineStyleNone;
            ws.Cells.Borders[Excel.XlBordersIndex.xlInsideHorizontal].LineStyle = Excel.XlLineStyle.xlLineStyleNone;
            ws.Cells.Borders[Excel.XlBordersIndex.xlDiagonalDown].LineStyle = Excel.XlLineStyle.xlLineStyleNone;
            ws.Cells.Borders[Excel.XlBordersIndex.xlDiagonalUp].LineStyle = Excel.XlLineStyle.xlLineStyleNone;

            Excel.Range cells = ws.Cells[MatrixOffset_Row - 1, MatrixOffset_Column];
            cells.Value2 = tpp.Algorithm + " Iteration: " + tpp.Iteration;


            object[,] valuesX = (object[,])Array.CreateInstance(typeof(object), new int[2] { I + 3, J + 3 }, new int[2] { 1, 1 });
            object[,] valuesOpp = (object[,])Array.CreateInstance(typeof(object), new int[2] { I + 3, J + 3 }, new int[2] { 1, 1 });
            object[,] valuesCij = (object[,])Array.CreateInstance(typeof(object), new int[2] { I + 3, J + 3 }, new int[2] { 1, 1 });
            valuesX[1, 1] = "x(i,j)";
            valuesX[1, 2 + J] = "a(i)";
            valuesX[2 + I, 1] = "b(j)";
            valuesOpp[1, 1] = "opp(i,j)";
            valuesCij[1, 1] = "c(i,j)";

            for (int i = 0; i < I; i++)
            {
                Warehouse w = geo.Warehouses[i];
                valuesX[i + 2, 1] = w.Id;
                valuesX[i + 2, 2 + J] = w.Supply;
                valuesOpp[i + 2, 1] = w.Id;
                valuesCij[i + 2, 1] = w.Id;
                for (int j = 0; j < J; j++)
                {
                    Customer c = geo.Customers[j];
                    if (i == 0)
                    {
                        valuesX[1, j + 2] = c.Id;
                        valuesX[2 + I, j + 2] = c.Demand;
                        valuesOpp[1, j + 2] = c.Id;
                        valuesCij[1, j + 2] = c.Id;
                    }

                    if (tpp.U != null)
                    {
                        if (i == 0 && j == 0)
                        {
                            valuesOpp[1, J + 2] = "U(i)";
                            valuesOpp[I + 2, 1] = "V(j)";
                        }
                        if (i == 0)
                        {
                            valuesOpp[I + 2, j + 2] = tpp.V[j];
                        }
                        if (j == 0)
                        {
                            valuesOpp[i + 2, J + 2] = tpp.U[i];
                        }
                    }
                    if (tpp.IsBaseVariable(i, j))
                    {
                        valuesX[i + 2, j + 2] = tpp.X[i, j];
                        valuesOpp[i + 2, j + 2] = "x";
                    }
                    else
                    {
                        valuesX[i + 2, j + 2] = "";
                        if (double.IsNaN(tpp.Opp[i, j]) == false)
                        {
                            valuesOpp[i + 2, j + 2] = tpp.Opp[i, j];
                        }
                    }
                    valuesCij[i + 2, j + 2] = tpp.Parent.TPP_C[i, j];
                }
            }
            I = I + 2;
            J = J + 2;
            cells = (Excel.Range)ws.Range[ws.Cells[MatrixOffset_Row, MatrixOffset_Column],
                                                      ws.Cells[MatrixOffset_Row + I, MatrixOffset_Column + J]];
            cells.Value2 = valuesX;
            formatBorders(cells, Excel.XlBorderWeight.xlMedium);

            cells = (Excel.Range)ws.Range[ws.Cells[MatrixOffset_Row, MatrixOffset_Column + J + 2],
                                                      ws.Cells[MatrixOffset_Row + I, MatrixOffset_Column + 2 * (J) + 2]];
            cells.Value2 = valuesOpp;
            formatBorders(cells, Excel.XlBorderWeight.xlMedium);

            cells = (Excel.Range)ws.Range[ws.Cells[MatrixOffset_Row + I + 2, MatrixOffset_Column],
                                                   ws.Cells[MatrixOffset_Row + 2 * (I) + 2, MatrixOffset_Column + J]];
            cells.Value2 = valuesCij;
            formatBorders(cells, Excel.XlBorderWeight.xlMedium);
            Marshal.ReleaseComObject(cells); // clean#

        }
        private void OutPutTransportplanFormated(int t)
        {


            if (t >= this.geo.Transportplans.Count) return;
            Transportplan tpp = this.geo.Transportplans[t];
            Excel.Workbook wb = this.Application.ActiveWorkbook;
            string wsname = names[4] + "." + tpp.Iteration + "." + tpp.Algorithm;

            int I = tpp.Parent.Warehouses.Count();
            int J = tpp.Parent.Customers.Count();

            // setup Transportplan
            Excel.Worksheet ws = wb.Worksheets.Add(Missing.Value, Missing.Value, Missing.Value, Missing.Value);
            ws.Name = wsname;

            ws.Cells.Interior.Pattern = Excel.XlPattern.xlPatternSolid;
            ws.Cells.Interior.PatternColor = System.Drawing.ColorTranslator.ToOle(System.Drawing.Color.White);

            ws.Cells.Borders[Excel.XlBordersIndex.xlInsideVertical].LineStyle = Excel.XlLineStyle.xlLineStyleNone;
            ws.Cells.Borders[Excel.XlBordersIndex.xlInsideHorizontal].LineStyle = Excel.XlLineStyle.xlLineStyleNone;
            ws.Cells.Borders[Excel.XlBordersIndex.xlDiagonalDown].LineStyle = Excel.XlLineStyle.xlLineStyleNone;
            ws.Cells.Borders[Excel.XlBordersIndex.xlDiagonalUp].LineStyle = Excel.XlLineStyle.xlLineStyleNone;

            new Thread(() =>
            {
                // Titel
                Excel.Range cell = ws.Cells[MatrixOffset_Row - 1, MatrixOffset_Column];
                cell.Value2 = tpp.Algorithm + " Iteration: " + tpp.Iteration;
                formatFont(cell, System.Drawing.Color.Black, true, false);
                cell.HorizontalAlignment = Excel.XlHAlign.xlHAlignLeft;
                Marshal.ReleaseComObject(cell); // clean#

                // Zelle Links oben
                cell = ws.Cells[MatrixOffset_Row, MatrixOffset_Column];
                cell.Value2 = "tpp";
                formatFont(cell, System.Drawing.Color.Black, true, false);
                formatBorders(cell, Excel.XlBorderWeight.xlMedium);
                Marshal.ReleaseComObject(cell); // clean#

                // Zelle ai
                cell = (Excel.Range)ws.Cells[MatrixOffset_Row, MatrixOffset_Column + 1 + 2 * J];
                formatFont(cell, System.Drawing.Color.Black, true, false);
                formatBorders(cell, Excel.XlBorderWeight.xlMedium);
                cell.Value2 = "ai";
                Marshal.ReleaseComObject(cell); // clean#

                // Zelle Bj
                cell = (Excel.Range)ws.Cells[MatrixOffset_Row + 1 + 2 * I, MatrixOffset_Column];
                formatFont(cell, System.Drawing.Color.Black, true, false);
                formatBorders(cell, Excel.XlBorderWeight.xlMedium);
                cell.Value2 = "bj";
                Marshal.ReleaseComObject(cell); // clean#

                // Zelle F
                cell = (Excel.Range)ws.Cells[MatrixOffset_Row + 1 + 2 * I, MatrixOffset_Column + 1 + 2 * J];
                formatFont(cell, System.Drawing.Color.Black, true, false);
                formatBorders(cell, Excel.XlBorderWeight.xlMedium);
                cell.Value2 = "F:" + tpp.F.ToString();
                Marshal.ReleaseComObject(cell); // clean#

                for (int i = 0; i < I; i++)
                {
                    Warehouse w = geo.Warehouses[i];
                    // Label Warehouse 
                    cell = (Excel.Range)ws.Cells[MatrixOffset_Row + 1 + 2 * i, MatrixOffset_Column];
                    formatFont(cell, System.Drawing.Color.Black, true, false);
                    cell.Value2 = w.Id;
                    Marshal.ReleaseComObject(cell); // clean
                    // Merge with lower Cell
                    cell = (Excel.Range)ws.Range[ws.Cells[MatrixOffset_Row + 1 + 2 * i + 0, MatrixOffset_Column],
                                                 ws.Cells[MatrixOffset_Row + 1 + 2 * i + 1, MatrixOffset_Column]];
                    cell.Merge();
                    formatBorders(cell, Excel.XlBorderWeight.xlMedium);
                    Marshal.ReleaseComObject(cell); // clean#

                    // Supply of Warehouse
                    cell = (Excel.Range)ws.Cells[MatrixOffset_Row + 1 + 2 * i, MatrixOffset_Column + 1 + 2 * J];
                    formatFont(cell, System.Drawing.Color.Black, true, false);
                    cell.Value2 = w.Supply;
                    Marshal.ReleaseComObject(cell); // clean 
                    cell = (Excel.Range)ws.Range[ws.Cells[MatrixOffset_Row + 1 + 2 * i + 0, MatrixOffset_Column + 1 + 2 * J],
                                                 ws.Cells[MatrixOffset_Row + 1 + 2 * i + 1, MatrixOffset_Column + 1 + 2 * J]];
                    cell.Merge();
                    formatBorders(cell, Excel.XlBorderWeight.xlMedium);
                    Marshal.ReleaseComObject(cell); // clean#

                    if (tpp.U != null)
                    {
                        if (i == 0)
                        {
                            // Zelle ui
                            cell = (Excel.Range)ws.Cells[MatrixOffset_Row, MatrixOffset_Column + 1 + 2 * J + 1];
                            formatFont(cell, System.Drawing.Color.Black, true, false);
                            formatBorders(cell, Excel.XlBorderWeight.xlMedium);
                            cell.Value2 = "U(i)";
                            Marshal.ReleaseComObject(cell); // clean#
                        }
                        // Zelle ui
                        cell = (Excel.Range)ws.Cells[MatrixOffset_Row + 1 + 2 * i, MatrixOffset_Column + 1 + 2 * J + 1];
                        cell.Value2 = tpp.U[i];
                        Marshal.ReleaseComObject(cell); // clean#

                        cell = (Excel.Range)ws.Range[ws.Cells[MatrixOffset_Row + 1 + 2 * i + 0, MatrixOffset_Column + 1 + 2 * J + 1],
                                                     ws.Cells[MatrixOffset_Row + 1 + 2 * i + 1, MatrixOffset_Column + 1 + 2 * J + 1]];

                        cell.Merge();
                        formatFont(cell, System.Drawing.Color.Black, true, false);
                        formatBorders(cell, Excel.XlBorderWeight.xlMedium);
                        Marshal.ReleaseComObject(cell); // clean#

                    }

                    // For all Customers
                    for (int j = 0; j < J; j++)
                    {
                        // Instance of Customer
                        Customer c = geo.Customers[j];

                        // c[i,j]
                        cell = (Excel.Range)ws.Cells[MatrixOffset_Row + 1 + 2 * i, MatrixOffset_Column + 1 + 2 * j];
                        formatFont(cell, System.Drawing.Color.Black, false, false);
                        formatBorders(cell);
                        cell.Value2 = geo.TPP_C[i, j];
                        Marshal.ReleaseComObject(cell); // clean

                        cell = (Excel.Range)ws.Range[ws.Cells[MatrixOffset_Row + 1 + 2 * i + 0, MatrixOffset_Column + 1 + 2 * j],
                                                     ws.Cells[MatrixOffset_Row + 1 + 2 * i + 1, MatrixOffset_Column + 1 + 2 * j + 1]];
                        formatBorders(cell, Excel.XlBorderWeight.xlMedium);
                        Marshal.ReleaseComObject(cell); // clean

                        if (tpp.IsBaseVariable(i, j))
                        {
                            // xij
                            cell = (Excel.Range)ws.Cells[MatrixOffset_Row + 1 + 2 * i + 1, MatrixOffset_Column + 1 + 2 * j];
                            cell.Value2 = tpp.X[i, j];
                            formatFont(cell, System.Drawing.Color.Black, true, true);
                            Marshal.ReleaseComObject(cell); // clean  

                            cell = (Excel.Range)ws.Range[ws.Cells[MatrixOffset_Row + 1 + 2 * i + 1, MatrixOffset_Column + 1 + 2 * j],
                                                         ws.Cells[MatrixOffset_Row + 1 + 2 * i + 1, MatrixOffset_Column + 1 + 2 * j + 1]];
                            cell.Merge();
                            Marshal.ReleaseComObject(cell); // clean  


                        }
                        else
                        {
                            if (Double.IsNaN(tpp.Opp[i, j]))
                            {
                                // Wenn keine Opp Kosten definiert, dann tu auch nichts
                            }
                            else
                            {
                                // Wenn OppKosten definiert, dann gib diese aus
                                cell = (Excel.Range)ws.Cells[MatrixOffset_Row + 1 + 2 * i, MatrixOffset_Column + 2 + 2 * j];
                                cell.Value2 = tpp.Opp[i, j];
                                if (tpp.Opp[i, j] < 0)
                                {
                                    formatFont(cell, System.Drawing.Color.Red, false, false);
                                }
                                else
                                {
                                    formatFont(cell, System.Drawing.Color.Green, false, false);
                                }
                                Marshal.ReleaseComObject(cell); // clean
                            }
                        }
                        if (i == 0)
                        {
                            // Customer Label
                            cell = (Excel.Range)ws.Cells[MatrixOffset_Row, MatrixOffset_Column + 1 + 2 * j];
                            cell.Value2 = c.Id;
                            formatFont(cell, System.Drawing.Color.Black, true, false);
                            Marshal.ReleaseComObject(cell); // clean#

                            cell = (Excel.Range)ws.Range[ws.Cells[MatrixOffset_Row, MatrixOffset_Column + 1 + 2 * j],
                                                         ws.Cells[MatrixOffset_Row, MatrixOffset_Column + 1 + 2 * j + 1]];
                            cell.Merge();
                            formatBorders(cell, Excel.XlBorderWeight.xlMedium);
                            Marshal.ReleaseComObject(cell); // clean#

                            // Demand of Customer
                            cell = (Excel.Range)ws.Cells[MatrixOffset_Row + 1 + 2 * I, MatrixOffset_Column + 1 + 2 * j];
                            formatFont(cell, System.Drawing.Color.Black, true, false);
                            cell.Value2 = c.Demand;
                            Marshal.ReleaseComObject(cell); // clean#

                            cell = (Excel.Range)ws.Range[ws.Cells[MatrixOffset_Row + 1 + 2 * I, MatrixOffset_Column + 1 + 2 * j],
                                                         ws.Cells[MatrixOffset_Row + 1 + 2 * I, MatrixOffset_Column + 1 + 2 * j + 1]];
                            cell.Merge();
                            formatBorders(cell, Excel.XlBorderWeight.xlMedium);
                            Marshal.ReleaseComObject(cell); // clean

                        }


                        if (tpp.V != null)
                        {
                            if (j == 0)
                            {
                                // Zelle Vj
                                //                           MatrixOffset_Row + 1 + 2 * I, MatrixOffset_Column
                                cell = (Excel.Range)ws.Cells[MatrixOffset_Row + 1 + 2 * I + 1, MatrixOffset_Column];
                                formatFont(cell, System.Drawing.Color.Black, true, false);
                                formatBorders(cell, Excel.XlBorderWeight.xlMedium);
                                cell.Value2 = "V(j)";
                                Marshal.ReleaseComObject(cell); // clean#
                            }
                            // Zelle vj
                            cell = (Excel.Range)ws.Cells[MatrixOffset_Row + 1 + 2 * I + 1, MatrixOffset_Column + 1 + 2 * j];
                            cell.Value2 = tpp.V[j];
                            Marshal.ReleaseComObject(cell); // clean#

                            cell = (Excel.Range)ws.Range[ws.Cells[MatrixOffset_Row + 1 + 2 * I + 1, MatrixOffset_Column + 1 + 2 * j],
                                                         ws.Cells[MatrixOffset_Row + 1 + 2 * I + 1, MatrixOffset_Column + 1 + 2 * j + 1]];

                            cell.Merge();
                            formatFont(cell, System.Drawing.Color.Black, true, false);
                            formatBorders(cell, Excel.XlBorderWeight.xlMedium);
                            Marshal.ReleaseComObject(cell); // clean#

                        }


                    }
                }
                if (cell != null) Marshal.ReleaseComObject(cell); // clean
                cell = null;
            }).Start();
        }

        private void formatFont(Excel.Range rng, System.Drawing.Color col, Boolean FontBold = false, Boolean FontUnderline = false)
        {
            rng.Font.Bold = FontBold;
            rng.Font.Underline = FontUnderline;
            rng.HorizontalAlignment = Excel.XlHAlign.xlHAlignCenter;
            rng.VerticalAlignment = Excel.XlVAlign.xlVAlignCenter;
            rng.WrapText = false;
            rng.Font.Color = System.Drawing.ColorTranslator.ToOle(col);
        }

        private void formatBorders(Excel.Range rng, Excel.XlBorderWeight lw = Excel.XlBorderWeight.xlThin, Excel.XlLineStyle ls = Excel.XlLineStyle.xlContinuous)
        {
            Excel.XlBordersIndex[] lbs = { Excel.XlBordersIndex.xlEdgeBottom, Excel.XlBordersIndex.xlEdgeLeft, Excel.XlBordersIndex.xlEdgeRight, Excel.XlBordersIndex.xlEdgeTop };
            foreach (Excel.XlBordersIndex lb in lbs)
            {
                rng.Borders[lb].LineStyle = ls;
                rng.Borders[lb].ColorIndex = 0;
                rng.Borders[lb].TintAndShade = 0;
                rng.Borders[lb].Weight = lw;
            }
            Excel.XlBordersIndex[] lbn = { 
                                              Excel.XlBordersIndex.xlDiagonalDown, Excel.XlBordersIndex.xlDiagonalUp 
                                             //,Excel.XlBordersIndex.xlInsideHorizontal, Excel.XlBordersIndex.xlInsideVertical
                                         };

            foreach (Excel.XlBordersIndex lb in lbn)
            {
                rng.Borders[lb].LineStyle = Excel.XlLineStyle.xlLineStyleNone;
            }
        }


        private void SwitchApplicationSettings(bool on)
        {

            Excel.Application excelApp = this.Application;
            excelApp.ScreenUpdating = on;
            excelApp.DisplayAlerts = on;
            excelApp.Calculation = on ? Excel.XlCalculation.xlCalculationAutomatic : Excel.XlCalculation.xlCalculationManual; ;
            excelApp.UserControl = on;
            excelApp.EnableEvents = on;
        }

        #endregion

    }
}
