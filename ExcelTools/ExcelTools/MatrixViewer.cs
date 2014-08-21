using clHNUORExcel.BaseClasses;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace ExcelTools
{
    public partial class MatrixViewer : Form
    {

        public GeoSituation geo { get; set; }
        public int Digits { get; set; }

        private double[,] matrix = null;
        public NumericUpDown[,] n = null;
        public double[,] Matrix
        {
            get { return matrix; }
            set
            {
                matrix = value;
                Size s = new Size(75, 25);
                int offset_x = 5;
                int offset_y = 5;
                int delta_x = 5;
                int delta_y = 5;

                Label l = null;
                if (matrix == null) throw new ArgumentNullException("Error while the Setting Matrix. Provided Matrix is null.");

                if (matrix.Rank != 2) throw new Exception("Error while the Setting Matrix. A Matrix of Rank 2 is excepted. Provided Rank: " + matrix.Rank);
                /*
                if (matrix.GetLength(0) != I) throw new Exception("Error while Setting the Matrix. A Matrix of  " + I+ " rows is expected. Provided row Length: " + matrix.GetLength(0));
                
                if (matrix.GetLength(1) != J) throw new Exception("Error while Setting the Matrix. A Matrix of  " + J + " columns is expected. Provided column Length: " + matrix.GetLength(1));

                if (matrix.Length != I*J) throw new Exception("Error while Setting the Matrix. A Matrix of Length : " + (I*J) + " is expected. Provided Length: " + matrix.Length);
                */
                int I = matrix.GetLength(0);
                int J = matrix.GetLength(1);
                n = new NumericUpDown[I, J];

                for (int i = 0; i < I; i++)
                {

                    l = new Label();
                    l.Text = "i:" + (i + 1);
                    l.Size = s;
                    l.Location = new Point(offset_x, offset_y + (i + 1) * (delta_y + s.Height));
                    l.TextAlign = ContentAlignment.MiddleRight;
                    l.AutoSize = false;
                    panel1.Controls.Add(l);
                    for (int j = 0; j < J; j++)
                    {

                        if (i == 0)
                        {
                            l = new Label();
                            l.Text = "j: " + (j + 1);
                            l.Size = s;
                            l.Location = new Point(offset_x + (j + 1) * (delta_x + s.Width), offset_y);
                            l.TextAlign = ContentAlignment.MiddleCenter;
                            l.AutoSize = false;
                            panel1.Controls.Add(l);
                        }
                        n[i, j] = new NumericUpDown();
                        n[i, j].DecimalPlaces = Digits;
                        n[i, j].Size = s;
                        n[i, j].Minimum = decimal.MinValue ;
                        n[i, j].Maximum = decimal.MaxValue;
                        n[i, j].Value = (decimal)matrix[i, j];
                        n[i, j].Location = new Point(offset_x + (j + 1) * (delta_x + s.Width),
                                                     offset_y + (i + 1) * (delta_y + s.Height));
                        panel1.Controls.Add(n[i, j]);
                        toolTip1.SetToolTip(n[i, j], "i/j: (" + (i + 1) + "/" + (j + 1) + ")");
                    }
                }

            }
        }

        public MatrixViewer()
        {
            InitializeComponent();
            Digits = 3;
        }

        private void MatrixViewer_Load(object sender, EventArgs e)
        {

        }

        private void buttonSave_Click(object sender, EventArgs e)
        {
            int I = matrix.GetLength(0);
            int J = matrix.GetLength(1);


            for (int i = 0; i < I; i++)
            {
                for (int j = 0; j < J; j++)
                {
                    this.matrix[i, j] = (double)n[i, j].Value;
                }
            }

            this.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.Close();

        }

        private void buttonCancel_Click(object sender, EventArgs e)
        {
            this.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.Close();
        }




    }
}
