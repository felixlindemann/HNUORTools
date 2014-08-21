using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Linq;
using System.Text;

namespace clHNUORExcel.BaseClasses
{
    public class Warehouse : Node
    {
        private bool isDummy = false;
        private bool isOpen = false;
        private double supply = 0;
        private double fixCosts = 0;

        public bool IsOpen
        {
            get { return isOpen; }
            set { SetPropertyField("IsOpen", ref isOpen, value); }
        }
        public bool IsDummy
        {
            get { return isDummy; }
            set { SetPropertyField("IsDummy", ref isDummy, value); }
        }
        public double Supply
        {
            get { return supply; }
            set { SetPropertyField("Supply", ref supply, value); }
        }
        public double FixCosts
        {
            get { return fixCosts; }
            set { SetPropertyField("FixCosts", ref fixCosts, value); }
        }


        public GraphicsPath getGraphicsPath(double zoom = 1)
        {

            List<float> lx = (new float[] { 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 0 }).ToList();
            List<float> ly = (new float[] { 0, 3.5f, 2, 3.5f, 2, 3.5f, 2, 3.5f, 2, 5, 5, 0, 0 }).ToList();
             
            GraphicsPath myPath = new GraphicsPath();
            myPath.StartFigure();

            for (int i = 1; i < lx.Count  ; i++)
            {
                myPath.AddLine(new PointF((lx[i - 1] - 2.5f) * 10 + (float)(this.X * zoom),
                                          (1.5f - ly[i - 1]) * 10 + (float)(this.Y * zoom)),
                               new PointF((lx[i - 0] - 2.5f) * 10 + (float)(this.X * zoom),
                                          (1.5f - ly[i - 0]) * 10 + (float)(this.Y * zoom)));
            }
             

            myPath.CloseFigure();

            return myPath;

        }

    }
}
