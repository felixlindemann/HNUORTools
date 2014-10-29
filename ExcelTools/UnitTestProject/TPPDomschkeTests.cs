using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using clHNUORExcel.BaseClasses;

namespace UnitTestProject
{
    /// <summary>
    /// Taken from:
    /// Domschke, Wolfgang ; Klein, Robert ; Drexl, Andreas ; Scholl, Armin ; Voß, Stefan: 
    /// Übungen und Fallbeispiele zum Operations Research. 
    /// Berlin/Heidelberg : Springer-Verlag, 2005 
    /// p. 74ff.
    /// 
    /// 
    /// ACHTUNG: U[i] und V[j] weichen von der Musterlösung bei Domschke ab, da 
    /// in der hier verwendeten Implementierung U[0] = 0 per default gesetzt wird.
    /// </summary>
    [TestClass]
    public class TPPDomschkeTests
    {
        [TestMethod]
        public void Aufgabe_4_1()
        {
            GeoSituation geo = new GeoSituation();
            int I = 3;
            int J = 4;
            double[] a = { 6, 1, 10 };
            double[] b = { 7, 5, 3, 2 };
            double[,] c = { { 2, 3, 11, 7 }, { 1, 0, 6, 1 }, { 5, 8, 15, 9 } };
            geo.TPP_C = c;
            for (int i = 0; i < I; i++)
            {
                geo.Warehouses.Add(new Warehouse() { Id = i.ToString(), Supply = a[i] });
            }

            for (int j = 0; j < J; j++)
            {
                geo.Customers.Add(new Customer() { Id = j.ToString(), Demand = b[j] });
            }

            Transportplan tpp_a = (new Transportplan(geo)).Solve(Transportplan.InitialMethod.NorthWestCornerRule);
            Assert.AreEqual(6d, tpp_a.X[0, 0]);
            Assert.AreEqual(1d, tpp_a.X[1, 0]);
            Assert.AreEqual(0d, tpp_a.X[2, 0]);
            Assert.AreEqual(5d, tpp_a.X[2, 1]);
            Assert.AreEqual(3d, tpp_a.X[2, 2]);
            Assert.AreEqual(2d, tpp_a.X[2, 3]);

            Assert.AreEqual(116, tpp_a.F);

            // 1. Iteration
            Transportplan tpp_b = tpp_a.Optimize(Transportplan.OptimizingMethod.MODIMethod, true);

            Assert.AreEqual(0d, tpp_a.U[0]);
            Assert.AreEqual(-1d, tpp_a.U[1]);
            Assert.AreEqual(3d, tpp_a.U[2]);

            Assert.AreEqual(2d, tpp_a.V[0]);
            Assert.AreEqual(5d, tpp_a.V[1]);
            Assert.AreEqual(12d, tpp_a.V[2]);
            Assert.AreEqual(6d, tpp_a.V[3]);

            Assert.AreEqual(-2d, tpp_a.Opp[0, 1]);
            Assert.AreEqual(-1d, tpp_a.Opp[0, 2]);
            Assert.AreEqual(1d, tpp_a.Opp[0, 3]);
            Assert.AreEqual(-4d, tpp_a.Opp[1, 1]);
            Assert.AreEqual(-5d, tpp_a.Opp[1, 2]);
            Assert.AreEqual(-4d, tpp_a.Opp[1, 3]);

            Assert.AreEqual(6d, tpp_b.X[0, 0]);
            Assert.AreEqual(1d, tpp_b.X[1, 2]);
            Assert.AreEqual(1d, tpp_b.X[2, 0]);
            Assert.AreEqual(5d, tpp_b.X[2, 1]);
            Assert.AreEqual(2d, tpp_b.X[2, 2]);
            Assert.AreEqual(2d, tpp_b.X[2, 3]);

            Assert.AreEqual(111, tpp_b.F);

            // 2. Iteration
            tpp_a = tpp_b.Optimize(Transportplan.OptimizingMethod.MODIMethod);

            Assert.AreEqual(0d, tpp_b.U[0]);
            Assert.AreEqual(-6d, tpp_b.U[1]);
            Assert.AreEqual(3d, tpp_b.U[2]);

            Assert.AreEqual(2d, tpp_b.V[0]);
            Assert.AreEqual(5d, tpp_b.V[1]);
            Assert.AreEqual(12d, tpp_b.V[2]);
            Assert.AreEqual(6d, tpp_b.V[3]);

            Assert.AreEqual(-2d, tpp_b.Opp[0, 1]);
            Assert.AreEqual(-1d, tpp_b.Opp[0, 2]);
            Assert.AreEqual(1d, tpp_b.Opp[0, 3]);
            Assert.AreEqual(5d, tpp_b.Opp[1, 0]);
            Assert.AreEqual(1d, tpp_b.Opp[1, 1]);
            Assert.AreEqual(1d, tpp_b.Opp[1, 3]);

            Assert.AreEqual(1d, tpp_a.X[0, 0]);
            Assert.AreEqual(5d, tpp_a.X[0, 1]);
            Assert.AreEqual(1d, tpp_a.X[1, 2]);
            Assert.AreEqual(6d, tpp_a.X[2, 0]);
            Assert.AreEqual(2d, tpp_a.X[2, 2]);
            Assert.AreEqual(2d, tpp_a.X[2, 3]);


            Assert.AreEqual(101, tpp_a.F);

            // 3. Iteration
            tpp_b = tpp_a.Optimize(Transportplan.OptimizingMethod.MODIMethod);

            Assert.AreEqual(0d, tpp_a.U[0]);
            Assert.AreEqual(-6d, tpp_a.U[1]);
            Assert.AreEqual(3d, tpp_a.U[2]);

            Assert.AreEqual(2d, tpp_a.V[0]);
            Assert.AreEqual(3d, tpp_a.V[1]);
            Assert.AreEqual(12d, tpp_a.V[2]);
            Assert.AreEqual(6d, tpp_a.V[3]);


            Assert.AreEqual(-1d, tpp_a.Opp[0, 2]);
            Assert.AreEqual(1d, tpp_a.Opp[0, 3]);
            Assert.AreEqual(5d, tpp_a.Opp[1, 0]);
            Assert.AreEqual(3d, tpp_a.Opp[1, 1]);
            Assert.AreEqual(1d, tpp_a.Opp[1, 3]);
            Assert.AreEqual(2d, tpp_a.Opp[2, 1]);

            Assert.AreEqual(5d, tpp_b.X[0, 1]);
            Assert.AreEqual(1d, tpp_b.X[0, 2]);
            Assert.AreEqual(1d, tpp_b.X[1, 2]);
            Assert.AreEqual(7d, tpp_b.X[2, 0]);
            Assert.AreEqual(1d, tpp_b.X[2, 2]);
            Assert.AreEqual(2d, tpp_b.X[2, 3]);
             
            Assert.AreEqual(100, tpp_b.F);
             
           tpp_a= tpp_b.Optimize(Transportplan.OptimizingMethod.MODIMethod);

           Assert.AreEqual(0d, tpp_b.U[0]);
           Assert.AreEqual(-5d, tpp_b.U[1]);
           Assert.AreEqual(4d, tpp_b.U[2]);

           Assert.AreEqual(1d, tpp_b.V[0]);
           Assert.AreEqual(3d, tpp_b.V[1]);
           Assert.AreEqual(11d, tpp_b.V[2]);
           Assert.AreEqual(5d, tpp_b.V[3]);

            Assert.AreEqual(1d, tpp_b.Opp[0, 0]);
            Assert.AreEqual(2d, tpp_b.Opp[0, 3]);
            Assert.AreEqual(5d, tpp_b.Opp[1, 0]);
            Assert.AreEqual(2d, tpp_b.Opp[1, 1]);
            Assert.AreEqual(1d, tpp_b.Opp[1, 3]);
            Assert.AreEqual(1d, tpp_b.Opp[2, 1]);

            Assert.IsTrue(tpp_a.IsOptimized);
        }
    }
}
