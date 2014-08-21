using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Linq;
using System.Text;

namespace clHNUORExcel.BaseClasses
{
    public class Customer : Node
    {

        private bool isDummy = false;
        private double demand = 0;

        public bool IsDummy
        {
            get { return isDummy; }
            set { SetPropertyField("IsDummy", ref isDummy, value); }
        }
        public double Demand
        {
            get { return demand; }
            set { SetPropertyField("Demand", ref demand, value); }
        }

        public GraphicsPath getGraphicsPath(double zoom = 1)
        {
            List<float> lx = (new float[] { 0, 0, -0.5f, 1, 2.5f, 2, 2, 0 }).ToList();
            List<float> ly = (new float[] { 0, 2, 2, 3.5f, 2, 2, 0, 0 }).ToList();

            GraphicsPath myPath = new GraphicsPath();
            myPath.StartFigure();

            
            for (int i = 1; i < lx.Count  ; i++)
            {
                myPath.AddLine(new PointF((lx[i - 1] - 1.0f) * 10 + (float)(this.X * zoom),
                                          (1.5f - ly[i - 1]) * 10 + (float)(this.Y * zoom)),
                               new PointF((lx[i - 0] - 1.0f) * 10 + (float)(this.X * zoom),
                                          (1.5f - ly[i - 0]) * 10 + (float)(this.Y * zoom)));
            }
             
            myPath.CloseFigure();

            return myPath;

        }

    }
}
