using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace clHNUORExcel.BaseClasses
{
    public class Link : implNotifyPropertyChanged

    {

        public Link(Node o, Node d)
        {
            this.OriginNode = o;
            this.DestinationNode = d;
            this.Id = (o.Id + d.Id) * o.Id * d.Id;
            this.Label = "l_" + o.Id + "_" + d.Id;
        }
        public Link(Node o, Node d, int id)
        {
            this.OriginNode = o;
            this.DestinationNode = d;
            this.Id = id;
            this.Label = "l_" + o.Id + "_" + d.Id;
        }

        public Link(Node o, Node d, int id, string label)
        {
            this.OriginNode = o;
            this.DestinationNode = d;
            this.Id = id;
            this.Label = label;
        }

        private int id = 1;
        private string label = "";
        private double distance = 0;
        private double costs = 0;
        private bool isUsed = false;
        private bool isOneWay = false;
        private Node originNode = null;
        private Node destinationNode = null;

        public int Id
        {
            get { return id; }
            set { SetPropertyField("Id", ref id, value); }
        }
        public string Label
        {
            get { return label; }
            set { SetPropertyField("Label", ref label, value); }
        }
        public double Distance
        {
            get { return distance; }
            set { SetPropertyField("Distance", ref distance, value); }
        }
        public double Costs
        {
            get { return costs; }
            set { SetPropertyField("Costs", ref costs, value); }
        }

        public bool IsUsed
        {
            get { return isUsed; }
            set { SetPropertyField("IsUsed", ref isUsed, value); }
        }
        public bool IsOneWay
        {
            get { return isOneWay; }
            set { SetPropertyField("IsOneWay", ref isOneWay, value); }
        }

        public Node OriginNode
        {
            get { return originNode; }
            set {
                if (value == null)
                {
                    throw new ArgumentNullException("Links must have an origin. A NULL-value is not allowed at this point.");
                } 
                SetPropertyField("OriginNode", ref originNode, value);
            }
        }
        public Node DestinationNode
        {
            get { return destinationNode; }
            set
            {
                if (value == null)
                {
                    throw new ArgumentNullException("Links must have a destination. A NULL-value is not allowed at this point.");
                } SetPropertyField("DestinationNode", ref destinationNode, value);
            }
        }
        public void setDistanceAndCosts(double factor = 1)
        {
            this.distance = originNode.getDistance(DestinationNode);
            this.costs = this.distance * factor;
        }

    }
}
