using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.Office.Tools.Ribbon;
using Excel = Microsoft.Office.Interop.Excel;
using System.Windows.Forms;
using clHNUORExcel.BaseClasses;
using System.Reflection;
using System.Diagnostics;
using ExcelTools.UDF;

namespace ExcelTools
{
    public partial class Ribbon1
    {

        #region Properties

        public ExcelTools.ThisAddIn addin { get; set; }
        public Excel.Application excelApp { get; set; }


        #endregion

        #region Init

        private void Ribbon1_Load(object sender, RibbonUIEventArgs e)
        {
            addin = Globals.ThisAddIn;
            addin.rib = this;
            excelApp = addin.Application;
        }
        #endregion

        #region ButtonEreignisse

        private void buttonRandomTPP_Click(object sender, RibbonControlEventArgs e)
        {
            try
            {
                createRandomTPP();
            }
            catch (Exception ex)
            {
                Debug.Print(ex.StackTrace);
                MessageBox.Show(ex.Message);
            }
        }

        private void buttonPrepareEmptyWorksheet_Click(object sender, RibbonControlEventArgs e)
        {
            addin.PrepareWorkbook();
        }

        private void buttonDrawGeoSituation_Click(object sender, RibbonControlEventArgs e)
        {

        }

        #endregion

        #region implemented Methods

        #region TPP

        private void createRandomTPP()
        {
            FormSetupRandomTPP frm = new FormSetupRandomTPP();
            frm.ParentRibbon = this;
            if (frm.ShowDialog() == DialogResult.OK)
            {
                this.addin.geo = frm.geo;
                this.addin.PrepareWorkbook();
                this.addin.OutPutCustomers();
                this.addin.OutPutWarehouses();
                try
                { 
                    this.addin.OutPutTransportplan(); 
                }
                catch (Exception ex)
                {
                    Debug.Print(ex.StackTrace);
                    throw;
                }
            }
        }



        #endregion


        #endregion



        public GeoSituation fromCurrentWorkbook()
        {

            Excel.Workbook wb = excelApp.ActiveWorkbook;
            GeoSituation geo = WorksheetHelper.getFromWorkBook(wb);


            return geo;
        }




        private void button1_Click(object sender, RibbonControlEventArgs e)
        {
            //    @"./Resources/HNU - OR-Tools 4 Excel.chm", HelpNavigator.TopicId, 6);


        }


    }
}
