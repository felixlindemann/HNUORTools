using clHNUORExcel.Calculation.SimpleMath;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace clHNUORExcel.BaseClasses
{
    public class Node : implNotifyPropertyChanged
    {
        private string id = "" + 1;
        private string label = "";
        private double x = 0;
        private double y = 0;

        public String Id
        {
            get { return id; }
            set { SetPropertyField("Id", ref id, value); }
        }
        public string Label
        {
            get { return label; }
            set { SetPropertyField("Label", ref label, value); }
        }
        public double X
        {
            get { return x; }
            set { SetPropertyField("X", ref x, value); }
        }
        public double Y
        {
            get { return y; }
            set { SetPropertyField("Y", ref y, value); }
        }

        public double getDistance(Node target)
        {
            return DistanceCalculation.getBeeLineDistance(this, target);
        }

        public double getPolarAngle(Node target)
        {
            return PolarangleCalculation.getPolarAngle(this, target);
        }

    }
}
