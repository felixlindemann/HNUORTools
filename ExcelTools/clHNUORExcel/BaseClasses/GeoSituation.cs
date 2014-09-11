using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Drawing;
using System.Linq;
using System.Text;

namespace clHNUORExcel.BaseClasses
{
    public class GeoSituation : implNotifyPropertyChanged
    {
        
        public double[,] TPP_C = null;
        public double[,] TSP_C = null;

        private List<Node> nodes = new List<Node>();
        private List<Customer> customers = new List<Customer>();
        private List<Warehouse> warehouses = new List<Warehouse>();
        private List<Link> links = new List<Link>();
        private List<Transportplan> transportplans = new List<Transportplan>();


        public List<Transportplan> Transportplans
        {
            get { return transportplans; }
            set { SetPropertyField("Transportplans", ref transportplans, value); }
        }
        public List<Link> Links
        {
            get { return links; }
            set { SetPropertyField("Links", ref links, value); }
        }
        public List<Node> Nodes
        {
            get { return nodes; }
            set { SetPropertyField("Nodes", ref nodes, value); }
        }
        public List<Customer> Customers
        {
            get { return customers; }
            set { SetPropertyField("Customers", ref customers, value); }
        }
        public List<Warehouse> Warehouses
        {
            get { return warehouses; }
            set { SetPropertyField("Warehouses", ref warehouses, value); }
        }

        public Bitmap drawImage(double zoom = 1, int position = -1)
        {
            List<double> x = new List<double>();
            List<double> y = new List<double>();

            x.AddRange(Nodes.Select(o => o.X));
            x.AddRange(Warehouses.Select(o => o.X));
            x.AddRange(Customers.Select(o => o.X));

            y.AddRange(Nodes.Select(o => o.Y));
            y.AddRange(Warehouses.Select(o => o.Y));
            y.AddRange(Customers.Select(o => o.Y));

            int width = (int)x.Max(o => o) + 10;
            int height = (int)y.Max(o => o) + 10;

            int w = 1000;
            int h = (int)(w * height / width);

            zoom = w / width;

            Bitmap bmp = new Bitmap(w, h);
            using (Graphics graph = Graphics.FromImage(bmp))
            {
                Rectangle ImageSize = new Rectangle(0, 0, w, h);
                graph.FillRectangle(Brushes.White, ImageSize);
                // graph.PageUnit = GraphicsUnit.Millimeter;
                //  graph.PageScale = 1;

                Pen outline = new Pen(Brushes.Black, 1f);
                foreach (Node n in nodes)
                {
                    drawNode(graph, n, zoom);
                }

                foreach (Customer n in customers)
                {
                    graph.FillPath(Brushes.Blue, n.getGraphicsPath(zoom));
                    graph.DrawPath(outline, n.getGraphicsPath(zoom));
                    drawNode(graph, n, zoom);
                }

                foreach (Warehouse n in warehouses)
                {
                    if (n.IsOpen)
                    {
                        graph.FillPath(Brushes.Gray, n.getGraphicsPath(zoom));

                    }
                    else
                    {
                        graph.FillPath(Brushes.White, n.getGraphicsPath(zoom));
                    }
                    graph.DrawPath(outline, n.getGraphicsPath(zoom));
                    drawNode(graph, n, zoom);
                }

                drawTransportplan(graph, zoom, position);
                    
                addGrid(graph, zoom, w, h);
            }
            return bmp;
        }

        private void drawTransportplan(Graphics graph, double zoom, int position = -1)
        {
            Transportplan t = null;
            if (this.transportplans.Any())
            {
                if (position < 0)
                {
                    t = this.transportplans.Last();
                }
                else
                {
                    if (this.transportplans.Count > position)
                    {
                        t = this.transportplans[position];
                    }
                }
                if (t == null) throw new NullReferenceException("The given Transportplan could not be found.");
                int I = this.Warehouses.Count;
                int J = this.Customers .Count;

                for (int i = 0; i < I; i++)
                {
                    Pen p = new Pen( Brushes.Black); // TODO Color implement
                    p.EndCap = System.Drawing.Drawing2D.LineCap.ArrowAnchor;
                    Warehouse w = this.Warehouses[i];
                    
                    for (int j = 0; j < J; j++)
                    {
                        Customer c = this.Customers[j];
                        if (t.X[i, j] > 0)
                        {
                            graph.DrawLine(p, new PointF((float)(w.X * zoom), (float)(w.Y * zoom)),
                                              new PointF((float)(c.X * zoom), (float)(c.Y * zoom)));
                        }
                        else
                        {
                            Debug.Print("Nothing transported from i:" + i + " to j:" + j);
                        }
                    }
                } 
            }

             
        }



        public void addGrid(Graphics graph, double zoom, double w, double h, double dx = 10, double dy = 10)
        {
            Pen p = new Pen(Brushes.Gray, 0.5f);
            p.DashStyle = System.Drawing.Drawing2D.DashStyle.Dot;
            int i = 0;
            while (true)
            {
                bool didit = false;
                float pos_x = (float)(dx * i * zoom);
                float pos_y = (float)(dy * i * zoom);
                if (pos_x < w) { didit = true; }
                if (pos_y < h) { didit = true; }
                graph.DrawLine(p, pos_x, -10, pos_x, (float)h);
                graph.DrawLine(p, -10, pos_y, (float)w, pos_y);

                i++;
                if (didit == false) break;
            }


        }
         
        private void drawMastaab(Graphics graph, double zoom, double w, double h)
        {
            double dx = 10 * zoom;
            double dy = 10 * zoom;



        }
        private void drawNode(Graphics graph, Node node, double zoom)
        {
            graph.FillEllipse(Brushes.Black,
                     new RectangleF(
                         (float)((node.X * zoom - 5)),
                         (float)((node.Y * zoom - 5)),
                         (float)(10),
                         (float)(10)));
        }


        public void generateTPPWLPCostMatrix(Boolean integersOnly = true, double costFactor=1)
        {
            int I = this.Warehouses.Count;
            int J = this.Customers.Count; 
            this.TPP_C = new double[I, J];
            for (int i = 0; i < I; i++)
            {
                Warehouse w = this.Warehouses[i];
                for (int j = 0; j < J; j++)
                {
                    Customer c = this.Customers[j];
                    if (c.IsDummy || w.IsDummy)
                    {
                        this.TPP_C[i, j] = 0;
                    }
                    else
                    {
                        this.TPP_C[i, j] = w.getDistance(c) * costFactor;
                    }
                    if (integersOnly)
                    {
                        this.TPP_C[i, j] = Math.Round(this.TPP_C[i, j]);
                    }
                }
            }
        }



    }
}
