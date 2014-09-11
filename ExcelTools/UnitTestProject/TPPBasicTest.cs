using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using clHNUORExcel.BaseClasses;
using System.Diagnostics;

namespace UnitTestProject
{
    [TestClass]
    public class TPPBasicTest
    {

        private GeoSituation SetupGeoSituation1()
        {
            GeoSituation geo = new GeoSituation();
            int I = 3;
            int J = 4;
            double[] supply = { 18, 22, 10 };
            double[] demand = { 10, 13, 14, 13 };
            double[,] costs = new double[3, 4] { { 2, 6, 5, 7 }, { 2, 7, 9, 4 }, { 1, 3, 4, 2 } };

            for (int i = 0; i < I; i++)
            {
                geo.Warehouses.Add(new Warehouse() { Id = i + 1, Supply = supply[i] });
            }
            for (int j = 0; j < J; j++)
            {
                geo.Customers.Add(new Customer() { Id = j + 1, Demand = demand[j] });
            }
            geo.TPP_C = costs;

            return geo;
        }

        /// <summary>
        /// vgl. Domschke Einf. in das Operations Research 2005 Aufl. 6 S. 85
        /// </summary>
        /// <returns></returns>
        private GeoSituation SetupGeoSituation2()
        {
            GeoSituation geo = new GeoSituation();
            int I = 3;
            int J = 4;
            double[] supply = { 10, 8, 7 };
            double[] demand = { 6, 5, 8, 6 };
            double[,] costs = new double[3, 4] { { 7, 7, 4, 7 }, { 9, 5, 3, 3 }, { 7, 2, 6, 4 } };

            for (int i = 0; i < I; i++)
            {
                geo.Warehouses.Add(new Warehouse() { Id = i + 1, Supply = supply[i] });
            }
            for (int j = 0; j < J; j++)
            {
                geo.Customers.Add(new Customer() { Id = j + 1, Demand = demand[j] });
            }
            geo.TPP_C = costs;
            return geo;
        }

        [TestMethod]
        public void TestNorthWestCornerRule()
        {
            GeoSituation geo = SetupGeoSituation2();

            Transportplan tpp = new Transportplan(geo);
            Transportplan tpp_nw = tpp.Solve(Transportplan.InitialMethod.NorthWestCornerRule);
            geo.Transportplans.Add(tpp_nw);

            Assert.AreEqual(10, tpp_nw.Parent.Warehouses[0].Supply);
            Assert.AreEqual(08, tpp_nw.Parent.Warehouses[1].Supply);
            Assert.AreEqual(07, tpp_nw.Parent.Warehouses[2].Supply);


            Assert.AreEqual(6, tpp_nw.Parent.Customers[0].Demand);
            Assert.AreEqual(5, tpp_nw.Parent.Customers[1].Demand);
            Assert.AreEqual(8, tpp_nw.Parent.Customers[2].Demand);
            Assert.AreEqual(6, tpp_nw.Parent.Customers[3].Demand);

            Assert.IsFalse(tpp_nw.IsBaseVariable(0, 2), "tpp_nw.IsBaseVariable(0, 2)");
            Assert.IsFalse(tpp_nw.IsBaseVariable(0, 3), "tpp_nw.IsBaseVariable(0, 3)");
            Assert.IsFalse(tpp_nw.IsBaseVariable(1, 0), "tpp_nw.IsBaseVariable(1, 0)");
            Assert.IsFalse(tpp_nw.IsBaseVariable(1, 3), "tpp_nw.IsBaseVariable(1, 3)");
            Assert.IsFalse(tpp_nw.IsBaseVariable(2, 0), "tpp_nw.IsBaseVariable(2, 0)");
            Assert.IsFalse(tpp_nw.IsBaseVariable(2, 1), "tpp_nw.IsBaseVariable(2, 1)");

            Assert.IsTrue(tpp_nw.IsBaseVariable(0, 0), "tpp_nw.IsBaseVariable(0, 0)");
            Assert.AreEqual(6d, tpp_nw.X[0, 0]);

            Assert.IsTrue(tpp_nw.IsBaseVariable(0, 1), "tpp_nw.IsBaseVariable(0, 1)");
            Assert.AreEqual(4d, tpp_nw.X[0, 1]);

            Assert.IsTrue(tpp_nw.IsBaseVariable(1, 1), "tpp_nw.IsBaseVariable(1, 1)");
            Assert.AreEqual(1d, tpp_nw.X[1, 1]);

            Assert.IsTrue(tpp_nw.IsBaseVariable(1, 2), "tpp_nw.IsBaseVariable(1, 2)");
            Assert.AreEqual(7d, tpp_nw.X[1, 2]);

            Assert.IsTrue(tpp_nw.IsBaseVariable(2, 2), "tpp_nw.IsBaseVariable(2, 2)");
            Assert.AreEqual(1d, tpp_nw.X[2, 2]);

            Assert.AreEqual(6d, tpp_nw.X[2, 3]);
            Assert.IsTrue(tpp_nw.IsBaseVariable(2, 3), "tpp_nw.IsBaseVariable(2, 3)");

            Assert.AreEqual(126d, tpp_nw.F);
        }
         
        [TestMethod]
        public void TestVogelApproximation()
        {
            GeoSituation geo = SetupGeoSituation2();

            Transportplan tpp = new Transportplan(geo);
            Transportplan tpp_vogel = tpp.Solve(Transportplan.InitialMethod.VogelApproximation);
            geo.Transportplans.Add(tpp_vogel);

            Assert.AreEqual(10, tpp_vogel.Parent.Warehouses[0].Supply);
            Assert.AreEqual(08, tpp_vogel.Parent.Warehouses[1].Supply);
            Assert.AreEqual(07, tpp_vogel.Parent.Warehouses[2].Supply);


            Assert.AreEqual(6, tpp_vogel.Parent.Customers[0].Demand);
            Assert.AreEqual(5, tpp_vogel.Parent.Customers[1].Demand);
            Assert.AreEqual(8, tpp_vogel.Parent.Customers[2].Demand);
            Assert.AreEqual(6, tpp_vogel.Parent.Customers[3].Demand);

            Assert.AreEqual(7, tpp_vogel.Parent.TPP_C[0, 0]);
            Assert.AreEqual(7, tpp_vogel.Parent.TPP_C[0, 1]);
            Assert.AreEqual(4, tpp_vogel.Parent.TPP_C[0, 2]);
            Assert.AreEqual(7, tpp_vogel.Parent.TPP_C[0, 3]);

            Assert.AreEqual(9, tpp_vogel.Parent.TPP_C[1, 0]);
            Assert.AreEqual(5, tpp_vogel.Parent.TPP_C[1, 1]);
            Assert.AreEqual(3, tpp_vogel.Parent.TPP_C[1, 2]);
            Assert.AreEqual(3, tpp_vogel.Parent.TPP_C[1, 3]);

            Assert.AreEqual(7, tpp_vogel.Parent.TPP_C[2, 0]);
            Assert.AreEqual(2, tpp_vogel.Parent.TPP_C[2, 1]);
            Assert.AreEqual(6, tpp_vogel.Parent.TPP_C[2, 2]);
            Assert.AreEqual(4, tpp_vogel.Parent.TPP_C[2, 3]);


            Assert.IsFalse(tpp_vogel.IsBaseVariable(0, 1));
            Assert.IsFalse(tpp_vogel.IsBaseVariable(0, 3));
            Assert.IsFalse(tpp_vogel.IsBaseVariable(1, 1));
            Assert.IsFalse(tpp_vogel.IsBaseVariable(1, 2));
            Assert.IsFalse(tpp_vogel.IsBaseVariable(2, 2));
            Assert.IsFalse(tpp_vogel.IsBaseVariable(2, 3));


            Assert.IsTrue(tpp_vogel.IsBaseVariable(0, 2));
            Assert.AreEqual(8d, tpp_vogel.X[0, 2]);

            Assert.IsTrue(tpp_vogel.IsBaseVariable(1, 3));
            Assert.AreEqual(6d, tpp_vogel.X[1, 3]);

            Assert.IsTrue(tpp_vogel.IsBaseVariable(2, 1));
            Assert.AreEqual(5d, tpp_vogel.X[2, 1]);

            Assert.IsTrue(tpp_vogel.IsBaseVariable(0, 0));
            Assert.AreEqual(2d, tpp_vogel.X[0, 0]);

            Assert.IsTrue(tpp_vogel.IsBaseVariable(1, 0));
            Assert.AreEqual(2d, tpp_vogel.X[1, 0]);
            Assert.IsTrue(tpp_vogel.IsBaseVariable(2, 0));
            Assert.AreEqual(2d, tpp_vogel.X[2, 0]);

            Assert.AreEqual(106, tpp_vogel.F);

        }

        [TestMethod]
        public void TestColumnMinimumMehtod1()
        {
            GeoSituation geo = SetupGeoSituation2();

            Transportplan tpp = new Transportplan(geo);
            Transportplan tpp_cmm1 = tpp.Solve(Transportplan.InitialMethod.ColumnMinimumMethod1);
            geo.Transportplans.Add(tpp_cmm1);

            Assert.AreEqual(10, tpp_cmm1.Parent.Warehouses[0].Supply);
            Assert.AreEqual(08, tpp_cmm1.Parent.Warehouses[1].Supply);
            Assert.AreEqual(07, tpp_cmm1.Parent.Warehouses[2].Supply);


            Assert.AreEqual(6, tpp_cmm1.Parent.Customers[0].Demand);
            Assert.AreEqual(5, tpp_cmm1.Parent.Customers[1].Demand);
            Assert.AreEqual(8, tpp_cmm1.Parent.Customers[2].Demand);
            Assert.AreEqual(6, tpp_cmm1.Parent.Customers[3].Demand);

            Assert.IsFalse(tpp_cmm1.IsBaseVariable(0, 1), "tpp_cmm1.IsBaseVariable(0, 1)");
            Assert.IsFalse(tpp_cmm1.IsBaseVariable(0, 2), "tpp_cmm1.IsBaseVariable(0, 2)");
            Assert.IsFalse(tpp_cmm1.IsBaseVariable(1, 0), "tpp_cmm1.IsBaseVariable(1, 0)");
            Assert.IsFalse(tpp_cmm1.IsBaseVariable(1, 1), "tpp_cmm1.IsBaseVariable(1, 1)");
            Assert.IsFalse(tpp_cmm1.IsBaseVariable(2, 0), "tpp_cmm1.IsBaseVariable(2, 0)");
            Assert.IsFalse(tpp_cmm1.IsBaseVariable(2, 2), "tpp_cmm1.IsBaseVariable(2, 2)");

            Assert.IsTrue(tpp_cmm1.IsBaseVariable(0, 0), "tpp_cmm1.IsBaseVariable(0, 0)");
            Assert.AreEqual(6d, tpp_cmm1.X[0, 0]);

            Assert.IsTrue(tpp_cmm1.IsBaseVariable(0, 3), "tpp_cmm1.IsBaseVariable(0, 3)");
            Assert.AreEqual(4d, tpp_cmm1.X[0, 3]);

            Assert.IsTrue(tpp_cmm1.IsBaseVariable(1, 2), "tpp_cmm1.IsBaseVariable(1, 2)");
            Assert.AreEqual(8d, tpp_cmm1.X[1, 2]);

            Assert.IsTrue(tpp_cmm1.IsBaseVariable(1, 3), "tpp_cmm1.IsBaseVariable(1, 3)");
            Assert.AreEqual(0d, tpp_cmm1.X[1, 3]);

            Assert.IsTrue(tpp_cmm1.IsBaseVariable(2, 1), "tpp_cmm1.IsBaseVariable(2, 1)");
            Assert.AreEqual(5d, tpp_cmm1.X[2, 1]);

            Assert.IsTrue(tpp_cmm1.IsBaseVariable(2, 3), "tpp_cmm1.IsBaseVariable(2, 3)");
            Assert.AreEqual(2d, tpp_cmm1.X[2, 3]);

            Assert.AreEqual(112d, tpp_cmm1.F);
        }

        [TestMethod]
        public void TestColumnMinimumMehtod2()
        {
            GeoSituation geo = SetupGeoSituation2();

            Transportplan tpp = new Transportplan(geo);
            Transportplan tpp_cmm2 = tpp.Solve(Transportplan.InitialMethod.ColumnMinimumMethod2);
            geo.Transportplans.Add(tpp_cmm2);

            Assert.AreEqual(10, tpp_cmm2.Parent.Warehouses[0].Supply);
            Assert.AreEqual(08, tpp_cmm2.Parent.Warehouses[1].Supply);
            Assert.AreEqual(07, tpp_cmm2.Parent.Warehouses[2].Supply);


            Assert.AreEqual(6, tpp_cmm2.Parent.Customers[0].Demand);
            Assert.AreEqual(5, tpp_cmm2.Parent.Customers[1].Demand);
            Assert.AreEqual(8, tpp_cmm2.Parent.Customers[2].Demand);
            Assert.AreEqual(6, tpp_cmm2.Parent.Customers[3].Demand);

            Assert.IsFalse(tpp_cmm2.IsBaseVariable(0, 1), "tpp_mmm.IsBaseVariable(0, 1)");
            Assert.IsFalse(tpp_cmm2.IsBaseVariable(1, 0), "tpp_mmm.IsBaseVariable(1, 0)");
            Assert.IsFalse(tpp_cmm2.IsBaseVariable(1, 1), "tpp_mmm.IsBaseVariable(1, 1)");
            Assert.IsFalse(tpp_cmm2.IsBaseVariable(1, 3), "tpp_mmm.IsBaseVariable(1, 3)");
            Assert.IsFalse(tpp_cmm2.IsBaseVariable(2, 0), "tpp_mmm.IsBaseVariable(2, 0)");
            Assert.IsFalse(tpp_cmm2.IsBaseVariable(2, 2), "tpp_mmm.IsBaseVariable(2, 2)");

            Assert.IsTrue(tpp_cmm2.IsBaseVariable(0, 0), "tpp_mmm.IsBaseVariable(0, 0)");
            Assert.AreEqual(6d, tpp_cmm2.X[0, 0]);

            Assert.IsTrue(tpp_cmm2.IsBaseVariable(0, 2), "tpp_mmm.IsBaseVariable(0, 2)");
            Assert.AreEqual(0d, tpp_cmm2.X[0, 2]);

            Assert.IsTrue(tpp_cmm2.IsBaseVariable(0, 3), "tpp_mmm.IsBaseVariable(0, 3)");
            Assert.AreEqual(4d, tpp_cmm2.X[0, 3]);

            Assert.IsTrue(tpp_cmm2.IsBaseVariable(1, 2), "tpp_mmm.IsBaseVariable(1, 2)");
            Assert.AreEqual(8d, tpp_cmm2.X[1, 2]);

            Assert.IsTrue(tpp_cmm2.IsBaseVariable(2, 1), "tpp_mmm.IsBaseVariable(2, 1)");
            Assert.AreEqual(5d, tpp_cmm2.X[2, 1]);

            Assert.IsTrue(tpp_cmm2.IsBaseVariable(2, 3), "tpp_mmm.IsBaseVariable(2, 3)");
            Assert.AreEqual(2d, tpp_cmm2.X[2, 3]);

            Assert.AreEqual(112d, tpp_cmm2.F);
        }
         
        /// <summary>
        /// TODO: FIXME
        /// Matrix Minimum-Methode wurde aus dem Gedächtnis implementiert. 
        /// Markierung Spalte/Zeile kann ggf. zu anderen Ergebnissen führen, 
        /// als nach Original-Quelle.
        /// </summary>
        [TestMethod]
        public void TestMatrixMinimumMehtod()
        {
            GeoSituation geo = SetupGeoSituation2();

            Transportplan tpp = new Transportplan(geo);
            Transportplan tpp_mmm = tpp.Solve(Transportplan.InitialMethod.MatrixMinimumMethod);
            geo.Transportplans.Add(tpp_mmm); 

            Assert.AreEqual(6d, tpp_mmm.X[0, 0]);
            Assert.AreEqual(0d, tpp_mmm.X[0, 2]);
            Assert.AreEqual(4d, tpp_mmm.X[0, 3]);
            Assert.AreEqual(8d, tpp_mmm.X[1, 2]);
            Assert.AreEqual(5d, tpp_mmm.X[2, 1]);
            Assert.AreEqual(2d, tpp_mmm.X[2, 3]);
             
            Assert.AreEqual(112d, tpp_mmm.F);
        }
         
        /// <summary>
        /// vgl. Beispiel S. 89
        /// in Einführung in Operations Research.
        /// </summary>
        [TestMethod]
        public void TestModiMethod()
        {
            GeoSituation geo = SetupGeoSituation2();
            Transportplan tpp = new Transportplan(geo);
            tpp = tpp.Solve(Transportplan.InitialMethod.ColumnMinimumMethod2); 
            Assert.AreEqual(6d, tpp.X[0, 0]);
            Assert.AreEqual(0d, tpp.X[0, 2]);
            Assert.AreEqual(4d, tpp.X[0, 3]);
            Assert.AreEqual(8d, tpp.X[1, 2]);
            Assert.AreEqual(5d, tpp.X[2, 1]);
            Assert.AreEqual(2d, tpp.X[2, 3]);
            Assert.AreEqual(112d, tpp.F);
            Transportplan tpp2 = tpp.Optimize(Transportplan.OptimizingMethod.MODIMethod);
            Assert.AreEqual(0d, tpp.U[0]);
            Assert.AreEqual(-1d, tpp.U[1]);
            Assert.AreEqual(-3d, tpp.U[2]);
            Assert.AreEqual(7d, tpp.V[0]);
            Assert.AreEqual(5d, tpp.V[1]);
            Assert.AreEqual(4d, tpp.V[2]);
            Assert.AreEqual(7d, tpp.V[3]);

            Assert.AreEqual(2d, tpp.Opp[0, 1]);
            Assert.AreEqual(3d, tpp.Opp[1, 0]);
            Assert.AreEqual(1d, tpp.Opp[1, 1]);
            Assert.AreEqual(2d, tpp.Opp[0, 1]);
            Assert.AreEqual(3d, tpp.Opp[2, 0]);
            Assert.AreEqual(5d, tpp.Opp[2, 2]);
              
            Assert.AreEqual(6d, tpp2.X[0, 0]);
            Assert.AreEqual(4d, tpp2.X[0, 2]);
            Assert.AreEqual(4d, tpp2.X[1, 2]);
            Assert.AreEqual(4d, tpp2.X[1, 3]);
            Assert.AreEqual(2d, tpp2.X[2, 3]);
            Assert.AreEqual(5d, tpp2.X[2, 1]);

            Assert.AreEqual(100d, tpp2.F);
            tpp2.Optimize(Transportplan.OptimizingMethod.MODIMethod);
            Assert.AreEqual(5d, tpp2.Opp[0, 1]);
            Assert.AreEqual(3d, tpp2.Opp[0, 3]);
            Assert.AreEqual(3d, tpp2.Opp[1, 0]);
            Assert.AreEqual(4d, tpp2.Opp[1, 1]);
            Assert.AreEqual(0d, tpp2.Opp[2, 0]);
            Assert.AreEqual(2d, tpp2.Opp[2, 2]);


        }

    }
}
