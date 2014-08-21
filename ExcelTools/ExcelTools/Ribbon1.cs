using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.Office.Tools.Ribbon;
using Microsoft.Office.Interop.Excel;
using System.Windows.Forms;
using clHNUORExcel.BaseClasses;
using System.Reflection;

namespace ExcelTools
{
    public partial class Ribbon1
    {

        private List<string> names = (new string[] { "Nodes", "Customers", "Warehouses", "Links", "TPP.Transportplan" }).ToList();
        public GeoSituation geo { get; set; }

        private void Ribbon1_Load(object sender, RibbonUIEventArgs e)
        {
            geo = new GeoSituation();
        }

        private void buttonRandomTPP_Click(object sender, RibbonControlEventArgs e)
        {
            try
            {
                createRandomWLPTPP();
            }
            catch (Exception ex)
            {
                 
            } 
        }
            private void createRandomWLPTPP(){
            FormSetupRandomTPP frm = new FormSetupRandomTPP();
            frm.Parent = this;
            if (frm.ShowDialog() == DialogResult.OK)
            {
                this.geo = frm.geo;
                PrepareWorkbook();
                Workbook wb = Globals.ThisAddIn.Application.ActiveWorkbook;
                Worksheet ws;

                // setup Customers
                ws = wb.Worksheets[2];

                string[] Colnames = { "id", "label", "x", "y", "demand" };

                for (int i = 0; i <= geo.Customers.Count; i++)
                {
                    if (i == 0)
                    {
                        for (int j = 0; j < Colnames.Length; j++)
                        {
                            ((Range)ws.Rows[i + 1, j + 1]).Value = Colnames[j];
                        }
                    }
                    else
                    {
                        Customer o = geo.Customers[i];
                        ((Range)ws.Rows[i + 1, 1]).Value = o.Id;
                        ((Range)ws.Rows[i + 1, 2]).Value = o.Label;
                        ((Range)ws.Rows[i + 1, 3]).Value = o.X;
                        ((Range)ws.Rows[i + 1, 4]).Value = o.Y;
                        ((Range)ws.Rows[i + 1, 5]).Value = o.Demand;
                    }
                }

                // setup Warehouses
                ws = wb.Worksheets[3];

                Colnames = new string[] { "id", "label", "x", "y", "supply", "FixCosts" };
                for (int i = 0; i <= geo.Warehouses.Count; i++)
                {
                    if (i == 0)
                    {
                        for (int j = 0; j < Colnames.Length; j++)
                        {
                            ((Range)ws.Rows[i + 1, j]).Value = Colnames[j];
                        }
                    }
                    else
                    {
                        Warehouse o = geo.Warehouses[i];
                        ((Range)ws.Rows[i + 1, 1]).Value = o.Id;
                        ((Range)ws.Rows[i + 1, 2]).Value = o.Label;
                        ((Range)ws.Rows[i + 1, 3]).Value = o.X;
                        ((Range)ws.Rows[i + 1, 4]).Value = o.Y;
                        ((Range)ws.Rows[i + 1, 5]).Value = o.Supply;
                        ((Range)ws.Rows[i + 1, 6]).Value = o.FixCosts;
                    }
                }

                // setup Links
                ws = wb.Worksheets[4];

                Colnames = new string[] { "id", "label", "From", "To", "distance", "costs", "IsOneWay", "IsUsed" };
                for (int i = 0; i <= geo.Links.Count; i++)
                {
                    if (i == 0)
                    {
                        for (int j = 0; j < Colnames.Length; j++)
                        {
                            ((Range)ws.Rows[i + 1, j]).Value = Colnames[j];
                        }
                    }
                    else
                    {
                        Link o = geo.Links[i];
                        ((Range)ws.Rows[i + 1, 1]).Value = o.Id;
                        ((Range)ws.Rows[i + 1, 2]).Value = o.Label;
                        ((Range)ws.Rows[i + 1, 3]).Value = o.OriginNode.Id;
                        ((Range)ws.Rows[i + 1, 4]).Value = o.DestinationNode.Id;
                        ((Range)ws.Rows[i + 1, 5]).Value = o.Distance;
                        ((Range)ws.Rows[i + 1, 6]).Value = o.Costs;
                        ((Range)ws.Rows[i + 1, 7]).Value = o.IsOneWay;
                        ((Range)ws.Rows[i + 1, 8]).Value = o.IsUsed;
                    }
                }
                // setup Links
                ws = wb.Worksheets[5];
                ((Range)ws.Rows[1, 1]).Value = "";
            }


        }

        private void buttonPrepareEmptyWorksheet_Click(object sender, RibbonControlEventArgs e)
        {
            PrepareWorkbook();

        }



        private void buttonDrawGeoSituation_Click(object sender, RibbonControlEventArgs e)
        {

        }

        public void PrepareWorkbook()
        {
            try
            {
                Microsoft.Office.Interop.Excel.Application excelApp = Globals.ThisAddIn.Application;


                Workbook wb;
                if (excelApp.Workbooks.Count == 0)
                {
                    wb = excelApp.Workbooks.Add();
                }
                wb = excelApp.ActiveWorkbook;
                Worksheet ws = null;


                for (int i = 0; i < names.Count; i++)
                {
                    if (wb.Worksheets.Count < i + 1)
                    {
                        ws = (Worksheet)excelApp.Worksheets.Add(Missing.Value, Missing.Value, Missing.Value, Missing.Value);
                    }
                    else
                    {
                        ws = excelApp.Worksheets[i + 1];
                    }
                    ws.Name = names[i];
                }

            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, "Error during preparing Worksheet");
            }

        }

    }
}
