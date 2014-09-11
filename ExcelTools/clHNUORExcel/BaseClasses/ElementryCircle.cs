using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Drawing;
using System.Linq;
using System.Text;

namespace clHNUORExcel.BaseClasses
{

    public class ElementryCircle
    {

        #region Properties
        public int i { get; set; }
        public int j { get; set; }
        public List<Point> Nodes { get; set; }
        public Boolean Closed { get; set; }
        public double OppValue { get; set; }
        public double MaxExchangeAmount { get; set; }
        public Boolean SearchVertical { get; set; }
        public Boolean IsAnalysed { get; set; }
        public Transportplan Parent { get; set; }

        #endregion

        #region Init

        public ElementryCircle(Transportplan parent, int p, int q)
        {
            this.Parent = parent;
            if (this.Parent.IsBaseVariable(p, q))
            {
                throw new Exception("Elementry Circles can only be created for non-Base-Variables!");
            }
            this.Nodes = new List<Point>();
            this.i = p;
            this.j = q;
            this.Closed = false;
            this.IsAnalysed = false;
            this.OppValue = this.Parent.Parent.TPP_C[p, q];
            this.MaxExchangeAmount = double.MaxValue;
            this.SearchVertical = true;
            this.Nodes.Add(new Point(p, q));
        }
        public ElementryCircle(ElementryCircle value)
        {
            this.Parent = value.Parent;

            this.Nodes = new List<Point>();
            this.Nodes.AddRange(value.Nodes);
            this.Closed = value.Closed;
            this.i = value.i;
            this.j = value.j;
            this.OppValue = value.OppValue;
            this.MaxExchangeAmount = value.MaxExchangeAmount;
            // Ändere die Suchrichtung
            this.SearchVertical = !value.SearchVertical;
        }

        #endregion

        #region Methods

        /// <summary>
        /// Fügt dem Elementarzyklus eine neue (Basis)Zelle hinzu
        /// </summary>
        /// <param name="i">Zeile der Zelle</param>
        /// <param name="j">Spalte der Stelle</param>
        /// <param name="closed">Soll der Zyklus geschlossen werden?</param>
        public ElementryCircle getCandidateCircle(int i, int j)
        {
            if (this.Closed) return null; // throw new Exception("This elementry circle is closed. It is not possible to add nodes!");

            ElementryCircle o = new ElementryCircle(this);
            if (o.Nodes.Count() == 0)
            {
                // first Node 
                Debug.Print("First Node to be added");
            }
            else
            {
                if (o.SearchVertical)
                {
                    if (o.Nodes.Last().X != i)
                    {
                        Debug.Print("Node not added: Expected in row:" + o.Nodes.Last().X + " got i:" + i);
                        return null;
                    }
                }
                else
                {

                    if (o.Nodes.Last().Y != j)
                    {
                        Debug.Print("Node not added: Expected in Column:" + o.Nodes.Last().Y + " got j:" + j);
                        return null;
                    }
                }
                if (o.Parent.IsBaseVariable(i, j))
                {
                    // check if this node can be added. --> each row/column can be visited only once (1 arrival, 1 departure)
                    if (o.Nodes.Where(x => x.X == i).ToList().Count() >= 2) return null; // avoid invalid circle
                    if (o.Nodes.Where(x => x.Y == j).ToList().Count() >= 2) return null; // avoid invalid circle

                }
                else
                {
                    if ((i == o.i && j == o.j) && o.Nodes.Count() >= 3)
                    {
                        o.Closed = true; // schließe den Elem.Zyklus.
                    }
                    else
                    {
                        return null;
                    }
                }
            }
            int multiplywith = -1;
            if (o.Nodes.Count() % 2 == 0) multiplywith = 1;
            if (multiplywith == -1)
            {
                o.MaxExchangeAmount = Math.Max(0,
                    Math.Min(o.MaxExchangeAmount, o.Parent.X[i, j]));
            }
            o.Nodes.Add(new Point(i, j));
            if (o.Closed == false)
            {
                o.OppValue = o.OppValue + o.Parent.Parent.TPP_C[i, j] * multiplywith;
            }
            return o;

        }

        public String getElementryCircle()
        {
            StringBuilder s = new StringBuilder();

            for (int i = 0; i < this.Nodes.Count; i++)
            {
                Point p = this.Nodes[i];

                s.Append("(");
                s.Append(p.X);
                s.Append("/");
                s.Append(p.Y);
                s.Append(")");
                if (i < this.Nodes.Count - 1)
                {
                    s.Append("--");
                }
            }

            return s.ToString();
        }

        public override string ToString()
        {
            StringBuilder s = new StringBuilder();
            s.Append(this.getElementryCircle());
            s.Append(" Exchange amount: ");
            s.Append(this.MaxExchangeAmount);
            s.Append(" Additional Costs per Item: ");
            s.Append(this.OppValue);
            s.Append(" Total Additional Costs: ");
            s.Append(this.OppValue * this.MaxExchangeAmount);
            return s.ToString();
        }

        #endregion

    }

}
