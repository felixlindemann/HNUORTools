namespace ExcelTools
{
    partial class Ribbon1 : Microsoft.Office.Tools.Ribbon.RibbonBase
    {
        /// <summary>
        /// Erforderliche Designervariable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        public Ribbon1()
            : base(Globals.Factory.GetRibbonFactory())
        {
            InitializeComponent();
        }

        /// <summary> 
        /// Verwendete Ressourcen bereinigen.
        /// </summary>
        /// <param name="disposing">"true", wenn verwaltete Ressourcen gelöscht werden sollen; andernfalls "false".</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Vom Komponenten-Designer generierter Code

        /// <summary>
        /// Erforderliche Methode für Designerunterstützung -
        /// Der Inhalt der Methode darf nicht mit dem Code-Editor geändert werden.
        /// </summary>
        private void InitializeComponent()
        {
            this.tab1 = this.Factory.CreateRibbonTab();
            this.group1 = this.Factory.CreateRibbonGroup();
            this.tab2 = this.Factory.CreateRibbonTab();
            this.group3 = this.Factory.CreateRibbonGroup();
            this.buttonPrepareEmptyWorksheet = this.Factory.CreateRibbonButton();
            this.buttonDrawGeoSituation = this.Factory.CreateRibbonButton();
            this.group2 = this.Factory.CreateRibbonGroup();
            this.buttonRandomGeoSituation = this.Factory.CreateRibbonButton();
            this.splitButton1 = this.Factory.CreateRibbonSplitButton();
            this.buttonSolveNorthWestCornerRule = this.Factory.CreateRibbonButton();
            this.buttonSolveColumnMinimum = this.Factory.CreateRibbonButton();
            this.buttonSolveMatrixMinimum = this.Factory.CreateRibbonButton();
            this.buttonSolveVogel = this.Factory.CreateRibbonButton();
            this.splitButton2 = this.Factory.CreateRibbonSplitButton();
            this.buttonSteppingStone = this.Factory.CreateRibbonButton();
            this.buttonSolveModi = this.Factory.CreateRibbonButton();
            this.buttonDrawTPP = this.Factory.CreateRibbonButton();
            this.tab1.SuspendLayout();
            this.tab2.SuspendLayout();
            this.group3.SuspendLayout();
            this.group2.SuspendLayout();
            // 
            // tab1
            // 
            this.tab1.ControlId.ControlIdType = Microsoft.Office.Tools.Ribbon.RibbonControlIdType.Office;
            this.tab1.Groups.Add(this.group1);
            this.tab1.Label = "TabAddIns";
            this.tab1.Name = "tab1";
            // 
            // group1
            // 
            this.group1.Label = "group1";
            this.group1.Name = "group1";
            // 
            // tab2
            // 
            this.tab2.Groups.Add(this.group3);
            this.tab2.Groups.Add(this.group2);
            this.tab2.Label = "HNU-OR-Tools";
            this.tab2.Name = "tab2";
            // 
            // group3
            // 
            this.group3.Items.Add(this.buttonPrepareEmptyWorksheet);
            this.group3.Items.Add(this.buttonDrawGeoSituation);
            this.group3.Label = "GeoSituation";
            this.group3.Name = "group3";
            // 
            // buttonPrepareEmptyWorksheet
            // 
            this.buttonPrepareEmptyWorksheet.Label = "Prepare Empty Worksheet";
            this.buttonPrepareEmptyWorksheet.Name = "buttonPrepareEmptyWorksheet";
            this.buttonPrepareEmptyWorksheet.Click += new Microsoft.Office.Tools.Ribbon.RibbonControlEventHandler(this.buttonPrepareEmptyWorksheet_Click);
            // 
            // buttonDrawGeoSituation
            // 
            this.buttonDrawGeoSituation.Label = "Draw GeoSituation";
            this.buttonDrawGeoSituation.Name = "buttonDrawGeoSituation";
            this.buttonDrawGeoSituation.Click += new Microsoft.Office.Tools.Ribbon.RibbonControlEventHandler(this.buttonDrawGeoSituation_Click);
            // 
            // group2
            // 
            this.group2.Items.Add(this.buttonRandomGeoSituation);
            this.group2.Items.Add(this.splitButton1);
            this.group2.Items.Add(this.splitButton2);
            this.group2.Items.Add(this.buttonDrawTPP);
            this.group2.Label = "TPP";
            this.group2.Name = "group2";
            // 
            // buttonRandomGeoSituation
            // 
            this.buttonRandomGeoSituation.Label = "Setup Random GeoSituation";
            this.buttonRandomGeoSituation.Name = "buttonRandomGeoSituation";
            this.buttonRandomGeoSituation.Click += new Microsoft.Office.Tools.Ribbon.RibbonControlEventHandler(this.buttonRandomTPP_Click);
            // 
            // splitButton1
            // 
            this.splitButton1.Items.Add(this.buttonSolveNorthWestCornerRule);
            this.splitButton1.Items.Add(this.buttonSolveColumnMinimum);
            this.splitButton1.Items.Add(this.buttonSolveMatrixMinimum);
            this.splitButton1.Items.Add(this.buttonSolveVogel);
            this.splitButton1.Label = "Get Initial Solution";
            this.splitButton1.Name = "splitButton1";
            // 
            // buttonSolveNorthWestCornerRule
            // 
            this.buttonSolveNorthWestCornerRule.Label = "Northwest Corner Rule";
            this.buttonSolveNorthWestCornerRule.Name = "buttonSolveNorthWestCornerRule";
            this.buttonSolveNorthWestCornerRule.ShowImage = true;
            // 
            // buttonSolveColumnMinimum
            // 
            this.buttonSolveColumnMinimum.Label = "Column Minimum Method";
            this.buttonSolveColumnMinimum.Name = "buttonSolveColumnMinimum";
            this.buttonSolveColumnMinimum.ShowImage = true;
            // 
            // buttonSolveMatrixMinimum
            // 
            this.buttonSolveMatrixMinimum.Label = "Matrix Minimum Method";
            this.buttonSolveMatrixMinimum.Name = "buttonSolveMatrixMinimum";
            this.buttonSolveMatrixMinimum.ShowImage = true;
            // 
            // buttonSolveVogel
            // 
            this.buttonSolveVogel.Label = "Vogel\'sche Approximation";
            this.buttonSolveVogel.Name = "buttonSolveVogel";
            this.buttonSolveVogel.ShowImage = true;
            // 
            // splitButton2
            // 
            this.splitButton2.Items.Add(this.buttonSteppingStone);
            this.splitButton2.Items.Add(this.buttonSolveModi);
            this.splitButton2.Label = "Optimize Solution";
            this.splitButton2.Name = "splitButton2";
            // 
            // buttonSteppingStone
            // 
            this.buttonSteppingStone.Label = "Use Stepping Stone";
            this.buttonSteppingStone.Name = "buttonSteppingStone";
            this.buttonSteppingStone.ShowImage = true;
            // 
            // buttonSolveModi
            // 
            this.buttonSolveModi.Label = "Use Modi Method";
            this.buttonSolveModi.Name = "buttonSolveModi";
            this.buttonSolveModi.ShowImage = true;
            // 
            // buttonDrawTPP
            // 
            this.buttonDrawTPP.ControlSize = Microsoft.Office.Core.RibbonControlSize.RibbonControlSizeLarge;
            this.buttonDrawTPP.Image = global::ExcelTools.Properties.Resources._1547_ballpointpen;
            this.buttonDrawTPP.Label = "Draw TPP";
            this.buttonDrawTPP.Name = "buttonDrawTPP";
            this.buttonDrawTPP.ShowImage = true;
            // 
            // Ribbon1
            // 
            this.Name = "Ribbon1";
            this.RibbonType = "Microsoft.Excel.Workbook";
            this.Tabs.Add(this.tab1);
            this.Tabs.Add(this.tab2);
            this.Load += new Microsoft.Office.Tools.Ribbon.RibbonUIEventHandler(this.Ribbon1_Load);
            this.tab1.ResumeLayout(false);
            this.tab1.PerformLayout();
            this.tab2.ResumeLayout(false);
            this.tab2.PerformLayout();
            this.group3.ResumeLayout(false);
            this.group3.PerformLayout();
            this.group2.ResumeLayout(false);
            this.group2.PerformLayout();

        }

        #endregion

        internal Microsoft.Office.Tools.Ribbon.RibbonTab tab1;
        internal Microsoft.Office.Tools.Ribbon.RibbonGroup group1;
        internal Microsoft.Office.Tools.Ribbon.RibbonTab tab2;
        internal Microsoft.Office.Tools.Ribbon.RibbonGroup group2;
        internal Microsoft.Office.Tools.Ribbon.RibbonButton buttonRandomGeoSituation;
        internal Microsoft.Office.Tools.Ribbon.RibbonSplitButton splitButton1;
        internal Microsoft.Office.Tools.Ribbon.RibbonButton buttonSolveNorthWestCornerRule;
        internal Microsoft.Office.Tools.Ribbon.RibbonButton buttonSolveColumnMinimum;
        internal Microsoft.Office.Tools.Ribbon.RibbonButton buttonSolveMatrixMinimum;
        internal Microsoft.Office.Tools.Ribbon.RibbonButton buttonSolveVogel;
        internal Microsoft.Office.Tools.Ribbon.RibbonSplitButton splitButton2;
        internal Microsoft.Office.Tools.Ribbon.RibbonButton buttonSteppingStone;
        internal Microsoft.Office.Tools.Ribbon.RibbonButton buttonSolveModi;
        internal Microsoft.Office.Tools.Ribbon.RibbonButton buttonDrawTPP;
        internal Microsoft.Office.Tools.Ribbon.RibbonGroup group3;
        internal Microsoft.Office.Tools.Ribbon.RibbonButton buttonDrawGeoSituation;
        internal Microsoft.Office.Tools.Ribbon.RibbonButton buttonPrepareEmptyWorksheet;
    }

    partial class ThisRibbonCollection
    {
        internal Ribbon1 Ribbon1
        {
            get { return this.GetRibbon<Ribbon1>(); }
        }
    }
}
