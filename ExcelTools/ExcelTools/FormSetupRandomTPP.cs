using clHNUORExcel.BaseClasses;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Diagnostics;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace ExcelTools
{
    public partial class FormSetupRandomTPP : Form
    {
        public Ribbon1 ParentRibbon { get; set; }
        public GeoSituation geo { get; set; }


        #region Gui-Operation

        public FormSetupRandomTPP()
        {
            InitializeComponent();
        }

        private void FormSetupRandomTPP_Load(object sender, EventArgs e)
        {
            if (geo == null) geo = new GeoSituation();
        }

        private void buttonCreate_Click(object sender, EventArgs e)
        {
            this.DialogResult = DialogResult.OK;
            this.Close();
        }
        private void radioButtonSolveWLP_CheckedChanged(object sender, EventArgs e)
        {
            changeVisibility();
        }

        private void radioButtonSolveTPP_CheckedChanged(object sender, EventArgs e)
        {
            changeVisibility();
        }

        private Boolean isWLP() { return radioButtonSolveWLP.Checked; }
        private void changeVisibility()
        {


            panelTPP.Visible = !isWLP();
            panelWLP.Visible = isWLP();

        }
        #endregion

        #region Draw Szenario

        private void drawProblem(int position = -1)
        {


            if (geo != null)
            {
                pictureBox1.Image = geo.drawImage(1, position);
            }

        }

        #region Callers
        private void bindingSourceTransportplan_PositionChanged(object sender, EventArgs e)
        {

            drawProblem(bindingSourceTransportplan.Position);
        }
        private void buttonDraw_Click(object sender, EventArgs e)
        {
            drawProblem();
        }


        private void bindingSourceTransportplan_DataSourceChanged(object sender, EventArgs e)
        {
            drawProblem();
        }

        #endregion


        #endregion

        #region Update Szenario

        #region Callers

        private void customerBindingSource_CurrentChanged(object sender, EventArgs e)
        {
            updateSzenario();
        }

        private void warehouseBindingSource_CurrentChanged(object sender, EventArgs e)
        {
            updateSzenario();
        }



        #endregion

        private void updateSzenario()
        {
            if (geo != null)
            {
                geo.Transportplans.Clear();
                geo.generateTPPWLPCostMatrix(checkBoxIntegerValues.Checked, (double)numericUpDownCostFactor.Value);
                drawProblem();
            }
        }

        #endregion

        #region New Szenario By Seed

        private void newProblemBySeed()
        {
            try
            {
                generateProblem();
                drawProblem();
            }
            catch (Exception ex)
            {
                Debug.Print(ex.StackTrace);
                MessageBox.Show(ex.ToString(), ex.Message);
            }
        }

        private void generateProblem()
        {
            generateProblem((int)numericUpDown1.Value);
        }

        private void generateProblem(int seed)
        {

            Random r = new Random(seed);

            geo = new GeoSituation();
            geo.Warehouses = new List<Warehouse>();
            geo.Customers = new List<Customer>();

            double[] size = { (double)numericUpDownWidth.Value, (double)numericUpDownHeigth.Value };

            double[] supply = { (double)numericUpDownMinSupply.Value, (double)numericUpDownMaxSupply.Value };

            double[] demand = { (double)numericUpDownMinDemand.Value, (double)numericUpDownMaxDemand.Value };
            if (demand[1] < demand[0]) throw new NotSupportedException("The given interval for demand is not valid. The upper bound must not be smaller than the lower bound.");


            int I = (int)numericUpDownNumberOfWarehouses.Value;
            int J = (int)numericUpDownNumberOfCustomers.Value;
            for (int i = 0; i < I; i++)
            {
                geo.Warehouses.Add(new Warehouse()
                {
                    Id = "" + (i + 1),
                    X = r.NextDouble() * (size[0] - 10) + 5,
                    Y = r.NextDouble() * (size[1] - 10) + 5,
                    Supply = supply[0] + r.NextDouble() * (supply[1] - supply[0]),
                    IsDummy = false,
                    IsOpen = true,
                    FixCosts = 200
                });
            }
            for (int j = 0; j < J; j++)
            {
                geo.Customers.Add(new Customer()
                {
                    Id = (j + 1).ToString(),
                    X = r.NextDouble() * (size[0] - 10) + 5,
                    Y = r.NextDouble() * (size[1] - 10) + 5,
                    Demand = demand[0] + r.NextDouble() * (demand[1] - demand[0]),
                    IsDummy = false
                });
            }

            if (checkBoxIntegerValues.Checked)
            {

                foreach (Node o in geo.Nodes)
                {
                    o.X = Math.Truncate(o.X);
                    o.Y = Math.Truncate(o.Y);
                }
                foreach (Warehouse o in geo.Warehouses)
                {
                    o.X = Math.Truncate(o.X);
                    o.Y = Math.Truncate(o.Y);
                    o.Supply = Math.Truncate(o.Supply);
                    o.FixCosts = Math.Truncate(o.FixCosts);
                }
                foreach (Customer o in geo.Customers)
                {
                    o.X = Math.Truncate(o.X);
                    o.Y = Math.Truncate(o.Y);
                    o.Demand = Math.Truncate(o.Demand);
                }
            }

            if (checkBoxClosedModel.Checked)
            {
                double t_supply = geo.Warehouses.Select(x => x.Supply).Sum();
                double t_demand = geo.Customers.Select(x => x.Demand).Sum();
                double delta_t = t_supply - t_demand;
                if (delta_t > 0)
                {
                    // More supply than demand
                    geo.Customers.Last().Demand += Math.Abs(delta_t);
                }
                else if (delta_t < 0)
                {
                    // More demand than supply
                    geo.Warehouses.Last().Supply += Math.Abs(delta_t);
                }
            }
            generateCostMatrix();
            this.warehouseBindingSource.DataSource = geo.Warehouses;
            this.customerBindingSource.DataSource = geo.Customers;


        }

        private void generateCostMatrix()
        {
            geo.generateTPPWLPCostMatrix(checkBoxIntegerValues.Checked, (double)numericUpDownCostFactor.Value);

        }

        #region Callers

        private void buttonNewRandomProblem_Click(object sender, EventArgs e)
        {
            try
            {
                Random r = new Random();
                numericUpDown1.Value = r.Next((int)numericUpDown1.Maximum);
                newProblemBySeed();
            }
            catch (Exception ex)
            {
                Debug.Print(ex.StackTrace);
                MessageBox.Show(ex.ToString(), ex.Message);
            }
        }

        private void buttonNewProblem_Click(object sender, EventArgs e)
        {
            newProblemBySeed();
        }

        private void numericUpDown1_ValueChanged(object sender, EventArgs e)
        {
            newProblemBySeed();
        }
        private void numericUpDownNumberOfCustomers_ValueChanged(object sender, EventArgs e)
        {
            newProblemBySeed();
        }

        private void numericUpDownMaxDemand_ValueChanged(object sender, EventArgs e)
        {
            newProblemBySeed();
        }

        private void numericUpDownMinDemand_ValueChanged(object sender, EventArgs e)
        {
            newProblemBySeed();
        }

        private void numericUpDownMaxSupply_ValueChanged(object sender, EventArgs e)
        {
            newProblemBySeed();
        }

        private void numericUpDownMinSupply_ValueChanged(object sender, EventArgs e)
        {
            newProblemBySeed();
        }

        private void numericUpDownMinFixCosts_ValueChanged(object sender, EventArgs e)
        {
            newProblemBySeed();
        }

        private void numericUpDownMaxFixCosts_ValueChanged(object sender, EventArgs e)
        {
            newProblemBySeed();
        }

        private void numericUpDownNumberOfWarehouses_ValueChanged(object sender, EventArgs e)
        {
            newProblemBySeed();
        }


        #endregion


        #endregion

        #region Solving Problem

        private void solveProblem()
        {
            try
            {
                if (geo == null) throw new ArgumentNullException("the GeoSituation is unexcpectedly null.");

                if (isWLP())
                {

                    return;
                }
                if (!isWLP())
                {
                    Transportplan.InitialMethod im = Transportplan.InitialMethod.NotDefined;
                    Transportplan.OptimizingMethod om = Transportplan.OptimizingMethod.NotDefined;
                    //is TPP 
                    if (radioButtonTPPNW.Checked) im = Transportplan.InitialMethod.NorthWestCornerRule;
                    if (radioButtonTPPCM1.Checked) im = Transportplan.InitialMethod.ColumnMinimumMethod1;
                    if (radioButtonTPPCM2.Checked) im = Transportplan.InitialMethod.ColumnMinimumMethod2;
                    if (radioButtonTPPMM.Checked) im = Transportplan.InitialMethod.MatrixMinimumMethod;
                    if (radioButtonTPPVA.Checked) im = Transportplan.InitialMethod.VogelApproximation;

                    if (radioButtonTPPNoOpt.Checked) om = Transportplan.OptimizingMethod.NoOptimizing;
                    if (radioButtonTPPMODI.Checked) om = Transportplan.OptimizingMethod.MODIMethod;
                    if (radioButtonTPPSTEPSTONE.Checked) om = Transportplan.OptimizingMethod.SteppingStoneMethod;

                    geo.Transportplans.Clear();
                    int maxIteration = geo.Warehouses.Count + geo.Customers.Count - 1;
                    Transportplan ti = new Transportplan(geo);
                    ti.IntegersOnly = checkBoxIntegerValues.Checked;
                    ti.CostFactor = (double)numericUpDownCostFactor.Value;
                    Transportplan to = ti.Solve(im);


                    if (om != Transportplan.OptimizingMethod.NoOptimizing)
                    {
                        to.Optimize(om);
                        this.bindingSourceTransportplan.DataSource = geo.Transportplans;
                    }
                }
                this.bindingSourceTransportplan.DataSource = null;
                this.bindingSourceTransportplan.DataSource = geo.Transportplans;
            }
            catch (Exception ex)
            {
                Debug.Print(ex.StackTrace);
                MessageBox.Show(ex.ToString(), ex.Message);
            }
        }


        #region Caller


        private void buttonSolve_Click(object sender, EventArgs e)
        {
            solveProblem();
            drawProblem();
        }

        #endregion

        private void bindingSourceTransportplan_CurrentChanged(object sender, EventArgs e)
        {

        }
        #endregion

        private void buttonCreateCostMatrix_Click(object sender, EventArgs e)
        {
            try
            {
                EditCostmatrix();
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, ex.ToString());
            }
        }
        private void EditCostmatrix()
        {
            MatrixViewer frm = new MatrixViewer();
            frm.geo = this.geo;
            frm.Matrix = this.geo.TPP_C;
            if (checkBoxIntegerValues.Checked) frm.Digits = 0;
            if (frm.ShowDialog() == System.Windows.Forms.DialogResult.OK)
            {
                int I = this.geo.Warehouses.Count;
                int J = this.geo.Customers.Count;

                for (int i = 0; i < I; i++)
                {
                    for (int j = 0; j < J; j++)
                    {
                        this.geo.TPP_C[i, j] = frm.Matrix[i, j];
                    }
                }

            }
        }

        private void buttonRecalcCostmatrix_Click(object sender, EventArgs e)
        {
            generateCostMatrix();
        }
        private void callHelp(String id)
        {
            if ((ModifierKeys & Keys.Control) == Keys.Control)
            {

                HelpNavigator navigator = HelpNavigator.TopicId;
                Help.ShowHelp(this, this.helpProvider1.HelpNamespace, navigator, id);
            }
        }
        private void radioButtonTPPNW_MouseClick(object sender, MouseEventArgs e)
        {
            callHelp("9");
        }

        private void radioButtonTPPVA_MouseClick(object sender, MouseEventArgs e)
        {
            callHelp("10");
        }


    }
}
