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
using ExcelTools.Worksheetfunctions;
using System.Diagnostics;
using Extensibility;
using ExcelFunctions;

namespace ExcelTools
{
    public partial class ThisAddIn
    {
        ExcelFunctions.Connect functionsAddinRef = null; 

        private void reg1()
        {
            functionsAddinRef = new ExcelFunctions.Connect();

            // get the name and GUIF from the class
            string NAME = functionsAddinRef.GetType().Namespace + "." + functionsAddinRef.GetType().Name;
            string GUID = functionsAddinRef.GetType().GUID.ToString().ToUpper();

            Excel.Application app = Globals.ThisAddIn.Application;
            // is the add-in already loaded in Excel, but maybe disabled
            // if this is the case - try to re-enable it
            bool fFound = false;
            foreach (Excel.AddIn a in app.AddIns)
            {
                try
                {
                    if (a.CLSID.Contains(GUID))
                    {
                        fFound = true;
                        if (!a.Installed)
                            a.Installed = true;
                        break;
                    }
                }
                catch { }
            }

            // if we do not see the UDF class in the list of installed addin we need to
            // add it to the collection
            if (!fFound)
            {
                // first register it
                functionsAddinRef.Register();
                // then install it
                app.AddIns.Add(NAME).Installed = true;
            }
        }
         private void ThisAddIn_Startup(object sender, System.EventArgs e)
        {

            reg1(); 
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
    }
}
