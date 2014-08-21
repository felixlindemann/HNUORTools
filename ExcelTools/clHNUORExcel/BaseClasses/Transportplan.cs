using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace clHNUORExcel.BaseClasses
{
    public class Transportplan : implNotifyPropertyChanged
    {

        public enum InitialMethod
        {
            NotDefined, NorthWestCornerRule, ColumnMinimumMethod1, ColumnMinimumMethod2, MatrixMinimumMethod, VogelApproximation
        }
        public enum OptimizingMethod
        {
            NotDefined, NoOptimizing, SteppingStoneMethod, MODIMethod
        }

        public Transportplan(GeoSituation value)
        {
            
            SetPropertyField("Parent", ref parent, value);
            parent = value;
            parent.Transportplans.Add(this); 
            this.x = new double[parent.Warehouses.Count , parent.Customers.Count]; 

        }
        public Transportplan(Transportplan value)
        {
            SetPropertyField("Parent", ref parent, value.parent);
            SetPropertyField("Iteration", ref iteration, value.iteration + 1);
            parent.Transportplans.Add(this);
            this.x = value.x; 
        }
        private String algorithm = InitialMethod.NotDefined.ToString();
        private GeoSituation parent;
        private int iteration = 0;
        private Boolean isOptimized = false;
        private double f = double.MinValue;
        private double costfactor = 1;
        private Boolean integersonly = true;
        private double[,] x; 

        public GeoSituation Parent
        {
            get { return parent; }

        }

        public int Iteration
        {
            get { return iteration; }
        }
        public string Algorithm
        {
            get { return algorithm; }
        }

        public double F
        {
            get
            {
                //    SetPropertyField("Iteration", ref iteration, value); 

                if (f == double.MinValue)
                {
                    double value = 0;
                    for (int i = 0; i < parent.Warehouses.Count; i++)
                    {
                        for (int j = 0; j < parent.Customers.Count; j++)
                        {
                            value = value + parent.TPP_C[i, j] * getX(i, j);
                        }
                    }
                    SetPropertyField("F", ref f, value);
                }
                return f;

            }
        }

        public Boolean IsOptimized
        {
            get
            {
                return isOptimized;
            }
        }
        public Boolean IntegersOnly
        {
            get
            {
                return integersonly;
            }
            set
            {
                SetPropertyField("IntegersOnly", ref integersonly, value);
            }
        }
        public Double CostFactor
        {
            get
            {
                return costfactor;
            }
            set
            {
                SetPropertyField("CostFactor", ref costfactor, value);
            }
        }
        public double[,] X
        {
            get { return x; }

        } 

        public void setX(int i, int j, double value)
        {
            // Debug.Print("i: " + i + " j: " + j);
            x[i, j] = value;
        } 
        public double getX(int i, int j)
        {
            return x[i, j];
        }
         


        #region Setup
         

        public void checkForDummys()
        {

            int I = parent.Warehouses.Count;
            int J = parent.Customers.Count;

            double s = parent.Warehouses.Sum(x => x.Supply);
            double d = parent.Customers.Sum(x => x.Demand);

            if (d == s) { return; }
            if (d > s)
            {
                // missing warehouse
                Warehouse w = new Warehouse() { X = 0, Y = 0, IsDummy = true, Supply = d - s, FixCosts = 0, Id = 0, IsOpen = true };
                parent.Warehouses.Add(w);
                x = new double[I + 1, J];  
            }
            else if (d < s)
            {
                // Missing Customer
                Customer o = new Customer() { X = 0, Y = 0, IsDummy = true, Demand = s - d, Id = 0 };
                parent.Customers.Add(o);
                x = new double[I, J + 1];  
            }
            parent.generateTPPWLPCostMatrix(this.integersonly, this.costfactor);
        }

        #endregion

        #region getInitialSolution

        public Transportplan Solve(InitialMethod im )
        {
            if (im == InitialMethod.NotDefined) throw new ArgumentException("The Initial Method for calculation a solution has not been set.");
            if (im == InitialMethod.NorthWestCornerRule)
            {
                return this.applyNorthWestCornerRule( );
            }

            if (im == InitialMethod.ColumnMinimumMethod1)
            {
                throw new NotImplementedException("The Initial Method 'Column-Minimum-Method 1' for calculation a solution has not been implemented yet.");
            }

            if (im == InitialMethod.ColumnMinimumMethod2)
            {
                throw new NotImplementedException("The Initial Method 'Column-Minimum-Method 2' for calculation a solution has not been implemented yet.");
            }

            if (im == InitialMethod.MatrixMinimumMethod)
            {
                throw new NotImplementedException("The Initial Method 'Matrix-Minimum-Method' for calculation a solution has not been implemented yet.");
            }

            if (im == InitialMethod.VogelApproximation)
            {
                throw new NotImplementedException("The Initial Method 'Vogelsche Approximation' for calculation a solution has not been implemented yet.");
            }
            else
            {
                throw new ArgumentException("The chosen Initial Method for calculation a solution is unknown.");
            }

        }

        #region NorthWestCornerRule

        public Transportplan applyNorthWestCornerRule(  )
        {

            SetPropertyField("Algorithm", ref algorithm, InitialMethod.NorthWestCornerRule.ToString());
          if(iteration ==1)  checkForDummys();
            List<double> demand = parent.Customers.Select(x => x.Demand).ToList();
            List<double> supply = parent.Warehouses.Select(x => x.Supply).ToList(); 
            
            for (int i = 0; i < parent.Warehouses.Count; i++)
            {
                for (int j = 0; j < parent.Customers.Count; j++)
                {
                    double tm = Math.Min(supply[i], demand[j]);
                    
                    supply[i] -= tm;
                    demand[j] -= tm; 
                     setX(i, j, tm); 
                }
            } 
            return this;
        }


        #endregion

        #endregion

        #region Optimize


        public Transportplan Optimize(OptimizingMethod om)
        {
            if (om == OptimizingMethod.NotDefined) throw new ArgumentException("The Initial Method for calculation a solution has not been set.");
            if (om == OptimizingMethod.MODIMethod)
            {
                throw new NotImplementedException("The Method 'Modi-Method' for Optimizing a solution has not been implemented yet.");
            }
            if (om == OptimizingMethod.SteppingStoneMethod)
            {
                throw new NotImplementedException("The  'Stepping-Method' for Optimizing a solution has not been implemented yet.");
            }
            else
            {
                throw new ArgumentException("The given  Method for optimizing a solution is unknown.");
            }
        }

        #endregion

    }
}
