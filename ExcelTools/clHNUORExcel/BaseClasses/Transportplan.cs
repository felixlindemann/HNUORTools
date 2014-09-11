using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Drawing;
using System.Linq;
using System.Text;

namespace clHNUORExcel.BaseClasses
{
    public class Transportplan : implNotifyPropertyChanged
    {

        #region init

        private void initOpp()
        {
            int I = parent.Warehouses.Count;
            int J = parent.Customers.Count;
            this.opp = new double[I, J];
            this.x = new double[I, J];
            for (int i = 0; i < I; i++)
            {
                for (int j = 0; j < J; j++)
                {
                    this.opp[i, j] = double.NaN;
                    this.x[i, j] = double.NaN;
                }
            }
            SetPropertyField("Opp", ref opp, opp);
            SetPropertyField("X", ref x, x);
        }

        public Transportplan(GeoSituation value)
        {

            SetPropertyField("Parent", ref parent, value);
            parent = value;
            parent.Transportplans.Add(this);
            initOpp();
        }

        public Transportplan(Transportplan value)
        {
            SetPropertyField("Parent", ref parent, value.parent);
            SetPropertyField("Iteration", ref iteration, value.iteration + 1);
            parent.Transportplans.Add(this);
            initOpp();
            SetPropertyField("X", ref x, (double[,])value.x.Clone());

        }

        #endregion

        #region Properties

        public enum InitialMethod
        {
            NotDefined, NorthWestCornerRule, ColumnMinimumMethod1, ColumnMinimumMethod2, MatrixMinimumMethod, VogelApproximation
        }

        public enum OptimizingMethod
        {
            NotDefined, NoOptimizing, SteppingStoneMethod, MODIMethod
        }

        #region lokale Variablen

        private StringBuilder log = new StringBuilder();
        private String algorithm = InitialMethod.NotDefined.ToString();
        private GeoSituation parent;
        private int iteration = 0;
        private Boolean isOptimized = false;
        private double f = double.MinValue;
        private double costfactor = 1;
        private Boolean integersonly = true;
        private double[,] x;
        private double[,] opp;
        private List<ElementryCircle> elementryCircles = new List<ElementryCircle>();
        private double[] u;
        private double[] v;
        #endregion

        public GeoSituation Parent
        {
            get { return parent; }

        }
        public String Log
        {
            get { return log.ToString(); }

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
                    f = 0;
                    for (int i = 0; i < parent.Warehouses.Count; i++)
                    {
                        for (int j = 0; j < parent.Customers.Count; j++)
                        {
                            if (this.IsBaseVariable(i, j))
                            {
                                f = f + parent.TPP_C[i, j] * this.x[i, j];
                            }
                        }
                    }
                    SetPropertyField("F", ref f, f);
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

        /// <summary>
        /// für Modi-Methode benötigt.
        /// </summary>
        public Double[] U
        {
            get
            {
                return u;
            }
            set
            {
                SetPropertyField("U", ref u, value);
            }
        }
        /// <summary>
        /// für Modi-Methode benötigt.
        /// </summary>
        public Double[] V
        {
            get
            {
                return v;
            }
            set
            {
                SetPropertyField("V", ref v, value);
            }
        }

        public List<ElementryCircle> ElementryCircles { get { return elementryCircles; } }

        public Boolean IsBaseVariable(int i, int j)
        {
            return double.IsNaN(x[i, j]) == false;

        }

        public double[,] X
        {
            get { return x; }
            set
            {
                SetPropertyField("X", ref x, value);
            }

        }

        public double[,] Opp
        {
            get { return opp; }
            set
            {
                SetPropertyField("Opp", ref opp, value);
            }
        }

        #endregion

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

        public Transportplan Solve(InitialMethod im)
        {
            if (im == InitialMethod.NotDefined) throw new ArgumentException("The Initial Method for calculation a solution has not been set.");
            if (im == InitialMethod.NorthWestCornerRule)
            {
                return this.applyNorthWestCornerRule();
            }

            if (im == InitialMethod.ColumnMinimumMethod1)
            {
                // throw new NotImplementedException("The Initial Method 'Column-Minimum-Method 1' for calculation a solution has not been implemented yet.");
                return this.applyColumnMinimumMethod1();
            }

            if (im == InitialMethod.ColumnMinimumMethod2)
            {
                // throw new NotImplementedException("The Initial Method 'Column-Minimum-Method 2' for calculation a solution has not been implemented yet.");
                return this.applyColumnMinimumMethod2();
            }

            if (im == InitialMethod.MatrixMinimumMethod)
            {
                //   throw new NotImplementedException("The Initial Method 'Matrix-Minimum-Method' for calculation a solution has not been implemented yet.");
                return this.applyMatrixMinimumMethod();
            }

            if (im == InitialMethod.VogelApproximation)
            {
                //  throw new NotImplementedException("The Initial Method 'Vogelsche Approximation' for calculation a solution has not been implemented yet.");
                return applyVogelApproximation();
            }
            else
            {
                throw new ArgumentException("The chosen Initial Method for calculation a solution is unknown.");
            }

        }
        /// <summary>
        /// Vgl. Domschke, Wolfgang ; Drexl, Andreas: Einführung in Operations Research.  
        /// 6., überarb. und erw. Aufl.  Berlin : Springer, 2005 
        /// S. 83 
        /// Sowie UML Diagramm
        /// </summary>
        /// <returns></returns>
        private Transportplan applyNorthWestCornerRule()
        {




            SetPropertyField("Algorithm", ref algorithm, InitialMethod.NorthWestCornerRule.ToString());
            if (iteration == 1) checkForDummys();
            List<double> supply = parent.Warehouses.Select(x => x.Supply).ToList(); // ai
            List<double> demand = parent.Customers.Select(x => x.Demand).ToList(); // bj

            int I = supply.Count();
            int J = demand.Count();
            int i = 0;
            int j = 0;

            // Start: i := j := 1.
            while (true)    // Iteration:
            {
                this.x[i, j] = Math.Min(supply[i], demand[j]); // xij := min{ai;bj}
                supply[i] -= this.x[i, j]; //  ai := ai – xij,
                demand[j] -= this.x[i, j]; //  bj := bj – xij;

                // if ai = 0 then i := i + 1 else j := j + 1;
                // Abbruch: Falls i = m und j = n gilt, wird nach Zeile 1 der Iteration abgebrochen.
                if ((i == I - 1) && (j == J - 1))
                {
                    break;
                }
                if (supply[i] == 0)
                {
                    i++;
                }
                else
                {
                    j++;
                }
                // gehe zur nächsten Iteration.
            }
            SetPropertyField("X", ref this.x, this.x);
            return this;
            // Ergebnis: Eine zulässige Basislösung mit m + n – 1 Basisvariablen.
        }

        /// <summary>
        /// Vgl. S.106 Domschke (2007).
        /// Transport: Grundlagen, lineare Transport- und Umladeprobleme. 5., überarb. Aufl. Bd. Bd. 1. 
        /// Oldenbourgs Lehr- und Handbücher der Wirtschafts- und Sozialwissenschaften. TPP siehe Online Doc 
        /// im Moodle. München: Oldenbourg. isbn: 9783486582901 
        /// 
        /// Voraussetzung: 
        /// Angebotsvektor a, 
        /// Nachfragevektor b, 
        /// Kostenmatrix C = (cij ).
        /// Start: Alle Zeilen und Spalten sind unmarkiert, alle xij=0.
        /// Iteration: Überprüfe die Spalten des Transporttableaus in der Reihenfolge j = 1, 2, . . . , n. 
        /// Wird eine unmarkierte Spalte k gefunden, so sind folgende Schritte auszuführen: 
        /// Bestimme das Element chk := min{Cik|Zeile i ist unmarkiert};
        /// Setze xhk := min{ah, bk}; ah := ah − xhk; bk := bk − xhk 
        /// Markiere:
        /// • Spalte k, falls bk = 0 und mindestens noch eine weitere Spalte unmarkiert ist, 
        /// • Zeile h, falls 
        ///     1. ah = 0 und bk 6= 0 oder
        ///     2. ah = bk = 0 und Spalte k nicht markiert wurde.
        /// Nach überprüfung der n-ten Spalte beginnt die Iteration erneut mit j = 1.
        /// Abbruch: Alle Zeilen sind markiert, nur noch eine Spalte ist unmarkiert.
        /// Ergebnis: Eine zulässige Basislösung.
        /// </summary>
        /// <returns></returns>
        private Transportplan applyColumnMinimumMethod1()
        {

            SetPropertyField("Algorithm", ref algorithm, InitialMethod.ColumnMinimumMethod1.ToString());
            if (iteration == 1) checkForDummys();
            List<double> demand = parent.Customers.Select(c => c.Demand).ToList();
            List<double> supply = parent.Warehouses.Select(w => w.Supply).ToList();
            int I = supply.Count();
            int J = demand.Count();
            Boolean[] MarkedI = new Boolean[I];
            Boolean[] MarkedJ = new Boolean[J];
            int iter = 0;
            double minCosts = double.MaxValue;
            while (true)
            {

                for (int j = 0; j < J; j++)
                {

                    int k = j;
                    int h = -1;
                    if (MarkedJ[j] == false)
                    {
                        minCosts = double.MaxValue;
                        for (int i = 0; i < I; i++)
                        {
                            if (MarkedI[i] == false && this.parent.TPP_C[i, j] < minCosts)
                            {
                                minCosts = this.parent.TPP_C[i, j];
                                h = i;
                            }
                        }


                        this.x[h, k] = Math.Min(supply[h], demand[k]);
                        iter++;
                        demand[k] = demand[k] - this.x[h, k];
                        supply[h] = supply[h] - this.x[h, k];
                        if (demand[k] == 0 && (J - MarkedJ.Where(o => o).Count() >= 2))
                        {
                            MarkedJ[k] = true;
                        }
                        else
                        {
                            if (supply[h] == 0)
                            {
                                MarkedI[h] = true;
                            }
                        }
                    }
                    //  Nach Überprüfung der n-ten Spalte beginnt die Iteration erneut mit j = 1. 
                }
                // Abbruch: Alle Zeilen sind markiert, 
                // nur noch eine Spalte ist unmarkiert.
                if (MarkedI.All(o => o) && MarkedJ.Where(o => o).Count() == J - 1)
                {
                    break;
                }
                else if (iter >= I + J - 1)
                {
                    throw new OverflowException("This process should have terminated.");
                }
            }

            SetPropertyField("X", ref this.x, this.x);
            if (demand.Sum(o => o) + supply.Sum(o => o) > 0) throw new Exception("Not all widgets have been transported.");

            return this;
        }

        /// <summary>
        /// Vgl. Domschke 2005 - Einf. Operations Research S. 86.
        /// </summary>
        /// <returns></returns>
        private Transportplan applyColumnMinimumMethod2()
        {

            SetPropertyField("Algorithm", ref algorithm, InitialMethod.ColumnMinimumMethod2.ToString());
            if (this.iteration == 1) checkForDummys();
            List<double> demand = parent.Customers.Select(c => c.Demand).ToList();
            List<double> supply = parent.Warehouses.Select(w => w.Supply).ToList();
            int I = supply.Count();
            int J = demand.Count();
            int iter = 0;
            // Start: Alle Zeilen sind unmarkiert, 
            // Achtung: Markierung ist Notwendig, da Restangebot >0 ggf. zu degenerierten Lösungen führen kann.
            // --> Durch Markierung werden Transportmengen ggf. auch 0 gesetzt (sonst NAN), so dass M+N-1 gilt.
            Boolean[] MarkedI = new Boolean[I];
            // alle xij := 0. --> hier NAN
            for (int i = 0; i < I; i++)
            {
                MarkedI[i] = false;
                for (int j = 0; j < J; j++)
                {
                    this.x[i, j] = double.NaN;
                }
            }

            // Iteration j = 1,...,n:  
            for (int j = 0; j < J; j++)
            {

                // 1. Suche in Spalte j unter denjenigen Kostenelementen cij, die in einer nicht markierten Zeile
                // stehen, das kleinste5 Element chj .

                // für alle Läger 
                while (true)
                {
                    int h = -1;
                    double chj = double.MaxValue;

                    for (int i = 0; i < I; i++)
                    {
                        if (MarkedI[i] == false && this.parent.TPP_C[i, j] < chj)
                        {
                            chj = this.parent.TPP_C[i, j];
                            h = i;
                        }
                    }
                    if (h == -1)
                    {
                        throw new IndexOutOfRangeException("No warehouse with lowest Transportcosts has been found for Customer " + (j + 1) + ".");

                    }
                    // 2. Nimm die Variable xhj mit dem Wert xhj := min {ah,bj} in die Basis auf 
                    this.x[h, j] = Math.Min(supply[h], demand[j]);
                    iter++;
                    // und reduziere die zugehörigen Angebots- und Nachfragemengen 
                    supply[h] = supply[h] - this.x[h, j]; // ah := ah – xhj 
                    demand[j] = demand[j] - this.x[h, j]; // sowie bj := bj – xhj.


                    if (supply[h] == 0)
                    {
                        // Falls danach ah = 0 ist, markiere die Zeile h und beginne erneut bei 1.;
                        MarkedI[h] = true;
                    }
                    else
                    {
                        break;
                    }
                    if
                        (j == J - 1 &&
                        (iter == (I) + (J) - 1))
                    {
                        break;
                    }

                    if (iter >= I + J - 1) throw new OverflowException("This process should have terminated.");


                }
            }






            SetPropertyField("X", ref this.x, this.x);
            if (demand.Sum() + supply.Sum() > 0) throw new Exception("Not all widgets have been transported.");

            return this;
        }

        /// <summary>
        /// Original Quelle fehlt. --> Aus dem Gedächtnis modelliert. 
        /// </summary>
        /// <returns></returns>
        private Transportplan applyMatrixMinimumMethod()
        {

            SetPropertyField("Algorithm", ref algorithm, InitialMethod.MatrixMinimumMethod.ToString());
            if (iteration == 1) checkForDummys();
            List<double> demand = parent.Customers.Select(c => c.Demand).ToList();
            List<double> supply = parent.Warehouses.Select(w => w.Supply).ToList();
            int I = supply.Count();
            int J = demand.Count();
            Boolean[] MarkedI = new Boolean[I];
            Boolean[] MarkedJ = new Boolean[J];

            for (int z = 0; z < I + J - 1; z++)
            {
                double chk = double.MaxValue;
                int h = -1;
                int k = -1;
                for (int i = 0; i < I; i++)
                {
                    if (MarkedI[i] == false)
                    {
                        for (int j = 0; j < J; j++)
                        {
                            if (MarkedJ[j] == false)
                            {
                                if (this.parent.TPP_C[i, j] < chk)
                                {
                                    h = i;
                                    k = j;
                                    chk = this.parent.TPP_C[i, j];
                                }
                            }
                        }
                    }
                }
                this.x[h, k] = Math.Min(supply[h], demand[k]);
                supply[h] = supply[h] - this.x[h, k];
                demand[k] = demand[k] - this.x[h, k];
                // TODO: Check for correct implementation!
                if (supply[h] == 0)
                {
                    MarkedI[h] = true;
                }
                else
                {
                    MarkedJ[k] = true;
                }
            }
            SetPropertyField("X", ref this.x, this.x);
            if (demand.Sum(o => o) + supply.Sum(o => o) > 0) throw new Exception("Not all widgets have been transported.");

            return this;
        }

        /// <summary>
        /// Vgl. Domschke 2005 - Einf. Operations Research S. 84f.
        /// </summary>
        /// <returns></returns>
        private Transportplan applyVogelApproximation()
        {
            List<double> demand = parent.Customers.Select(c => c.Demand).ToList();
            List<double> supply = parent.Warehouses.Select(w => w.Supply).ToList();
            int I = supply.Count();
            int J = demand.Count();
            Boolean[] MarkedI = new Boolean[I];
            Boolean[] MarkedJ = new Boolean[J];

            while (true) // Iteration
            {
                // Berechne für jede unmarkierte Zeile i die Differenz dzi := cih – cik zwischen dem zweitkleinsten
                // Element cih und dem kleinsten Element cik aller in einer noch unmarkierten
                // Spalte (und in Zeile i) stehenden Elemente der Kostenmatrix.
                Double[] dz = new Double[I];
                int[] dzp = new int[I];
                int[] dzq = new int[I];
                for (int i = 0; i < I; i++)
                {
                    dz[i] = double.NaN;
                    if (MarkedI[i] == false)
                    {
                        double cih = double.MaxValue; // zweitkleinstes
                        double cik = double.MaxValue; // kleinstes

                        for (int j = 0; j < J; j++)
                        {
                            if (MarkedJ[j] == false)    // ist die spalte j unmarkiert
                            {
                                if (cik == double.MaxValue) // ist dies die erste Iteration.
                                {
                                    dzp[i] = i;
                                    dzq[i] = j;
                                    cik = parent.TPP_C[i, j];
                                }
                                else // wurde cik bereits mind. 1x belegt
                                {
                                    if (cih > parent.TPP_C[i, j]) // ist cij < cih
                                    {
                                        if (cik > parent.TPP_C[i, j]) // und ist cik < cij
                                        {
                                            dzp[i] = i;
                                            dzq[i] = j;    // markiere j als neustes kleinstes Element
                                            cih = cik;
                                            cik = parent.TPP_C[i, j];
                                        }
                                        else
                                        {
                                            cih = parent.TPP_C[i, j];
                                        }
                                    }
                                }
                            }
                        }
                        dz[i] = cih - cik;
                    }
                }

                // Berechne für jede unmarkierte Spalte j die Differenz dsj := chj – ckj zwischen dem zweitkleinsten
                // Element chj und dem kleinsten Element ckj aller in einer noch unmarkierten Zeile
                // (und in Spalte j) stehenden Elemente der Kostenmatrix.
                Double[] ds = new Double[J];
                int[] dsp = new int[J];
                int[] dsq = new int[J];
                for (int j = 0; j < J; j++)
                {
                    ds[j] = double.NaN;
                    if (MarkedJ[j] == false)
                    {
                        double chj = double.MaxValue;
                        double ckj = double.MaxValue;
                        for (int i = 0; i < I; i++)
                        {
                            if (MarkedI[i] == false)
                            {
                                if (ckj == double.MaxValue)
                                {
                                    dsp[j] = i;
                                    dsq[j] = j;
                                    ckj = parent.TPP_C[i, j];
                                }
                                else
                                {
                                    if (chj > parent.TPP_C[i, j])
                                    {
                                        if (ckj > parent.TPP_C[i, j])
                                        {
                                            dsp[j] = i;
                                            dsq[j] = j;
                                            chj = ckj;
                                            ckj = parent.TPP_C[i, j];
                                        }
                                        else
                                        {
                                            chj = parent.TPP_C[i, j];
                                        }
                                    }
                                }
                            }
                        }
                        ds[j] = chj - ckj;
                    }
                }
                // Wähle unter allen unmarkierten Zeilen und Spalten diejenige Zeile oder Spalte, welche die
                // größte Differenz dzi oder dsj aufweist. Das bei der Differenzbildung berücksichtigte
                // kleinste Kostenelement der Zeile oder Spalte sei cpq.
                int p = int.MinValue;
                int q = int.MinValue;
                if (dz.Max() >= ds.Max())
                {
                    p = dzp[dz.ToList().IndexOf(dz.Max())];
                    q = dzq[dz.ToList().IndexOf(dz.Max())];
                }
                else
                {
                    p = dsp[ds.ToList().IndexOf(ds.Max())];
                    q = dsq[ds.ToList().IndexOf(ds.Max())];
                }

                // Nimm die Variable xpq mit dem Wert xpq := min {ap, bq} in die Basis auf und reduziere die
                // zugehörigen Angebots- und Nachfragemengen ap := ap – xpq sowie bq := bq – xpq. Falls
                // danach ap = 0 ist, markiere die Zeile p, ansonsten markiere die Spalte q und beginne erneut
                // mit der Iteration.
                this.x[p, q] = Math.Min(supply[p], demand[q]);
                supply[p] = supply[p] - this.x[p, q];
                demand[q] = demand[q] - this.x[p, q];
                if (supply[p] == 0)
                {
                    MarkedI[p] = true;
                }
                else
                {
                    MarkedJ[q] = true;
                }
                // Abbruch: n–1 Spalten oder m–1 Zeilen sind markiert. Den in einer unmarkierten Zeile und
                // einer unmarkierten Spalte stehenden Variablen werden die verbliebenen Restmengen zugeordnet.
                if (MarkedI.Where(x => x).Count() == I - 1)
                {
                    for (int i = 0; i < I; i++)
                    {
                        if (MarkedI[i] == false)
                        {
                            for (int j = 0; j < J; j++)
                            {
                                if (MarkedJ[j] == false)
                                {
                                    this.x[i, j] = Math.Min(supply[i], demand[j]);
                                    supply[i] = supply[i] - this.x[i, j];
                                    demand[j] = demand[j] - this.x[i, j];
                                }
                            }
                            break;
                        }
                    }
                    break;
                }
                else if (MarkedJ.Where(x => x).Count() == J - 1)
                {
                    for (int j = 0; j < J; j++)
                    {
                        if (MarkedJ[j] == false)
                        {
                            for (int i = 0; i < I; i++)
                            {
                                if (MarkedI[i] == false)
                                {
                                    this.x[i, j] = Math.Min(supply[i], demand[j]);
                                    supply[i] = supply[i] - this.x[i, j];
                                    demand[j] = demand[j] - this.x[i, j];

                                }
                            }
                            break;
                        }
                    }
                    break;
                }
            }

            return this;
        }

        #endregion

        #region Optimize

        public Transportplan Optimize(OptimizingMethod om)
        {
            if (om == OptimizingMethod.NotDefined) throw new ArgumentException("The Initial Method for calculation a solution has not been set.");
            if (om == OptimizingMethod.MODIMethod)
            {
                //  throw new NotImplementedException("The Method 'Modi-Method' for Optimizing a solution has not been implemented yet.");
                Transportplan tpp = this.ApplyModiMethodIteration();
                this.parent.Transportplans.Add(tpp);
                /*
                Boolean doit = tpp.IsOptimized;
                while (doit != true)
                {
                    tpp = this.ApplyModiMethodIteration();
                    tpp.parent.Transportplans.Add(tpp);
                    doit = tpp.IsOptimized;
                }
                 */
                return tpp;
            }
            if (om == OptimizingMethod.SteppingStoneMethod)
            {
                // throw new NotImplementedException("The 'Stepping-Method' for Optimizing a solution has not been implemented yet.");
                Transportplan tpp = this.ApplySteppingStoneIteration();
                this.parent.Transportplans.Add(tpp);
                Boolean doit = tpp.IsOptimized;
                while (doit != true)
                {
                    tpp = this.ApplySteppingStoneIteration();
                    tpp.parent.Transportplans.Add(tpp);
                    doit = tpp.IsOptimized;
                }
                return tpp;
            }
            else
            {
                throw new ArgumentException("The given  Method for optimizing a solution is unknown.");
            }
        }

        #region Stepping Stone


        /// <summary>
        /// vgl. Domschke Logisitk: transport 4. Auflg. S. 127f.
        /// Mathematische Symbole in Latex-Syntax
        /// </summary>
        /// <returns></returns>
        private Transportplan ApplySteppingStoneIteration()
        {
            //Voraussetzung: Eine zulässige Basislösung mit dem Zielfunktionswert F; 
            // Kostenmatrix C = (cij). --> Hier this.parent. TPP_C
            // Iteration:
            // 1. Berechne die Opportunitätskosten \bar{c}_{ij} für jede Nichtbasiszelle (i,j) gemäß Satz 6.4. 
            calcOppCosts();
            SetPropertyField("Opp", ref opp, opp);

            // 2. Ist \bar{c}_{ij} \geq 0 für alle Nichtbasiszellen (i,j), so ist die gegenwärtige Basislösung optimal.
            //    Ansonsten bestimme die (oder eine) Nichtbasiszelle (s,t) für die
            //   \bar{c}_{st}  = min { \bar{c}_{ij} | (i,j) ist Nichtbasiszelle} gilt.
            ElementryCircle elem = this.elementryCircles.Where(x => x.Closed).OrderBy(x => x.OppValue).Last();

            if (elem.OppValue >= 0)
            {
                // Ergebnis: Die (bzw. eine) optimale Basislösung des klassischen TPPs.
                SetPropertyField("IsOptimized", ref isOptimized, true);

            }
            else
            {


                // 3. Betrachte den elementaren Kreis, dem Zelle (s, t) zugehört, und berechne \Delta = \min { x_{pq}}
                //    über alle Basisvariablen x_{pq}, deren Kostenwert c_{pq} bei der Berechnung von \bar{c}_{st} mit dem 
                //    Faktor -1 eingeht. Diejenige dieser Basisvariablen, deren Wert gleich \Delta ist, wird aus der
                //    Basis eliminiert.
                // Implementierungs-Anmerkung:
                // - Zelle (s, t) ist hier (elem.i / elem.j)
                // - \Delta wurde bereits bei der Ermittlung des Elementaren Kreis' ermittelt.
                Boolean BaseExchange = false;
                int m = 1; // Multiplikator für abwechselndes Addieren und subtrahieren
                for (int i = 0; i < elem.Nodes.Count() - 1; i++)
                {
                    Point p = elem.Nodes[i];

                    //    Setze x_{st}: = \Delta. und subtrahiere bzw. addiere von bzw. zu den Werten der Basisvariablen im
                    //    Kreis abwechselnd den Wert \Delta.
                    this.x[p.X, p.Y] = this.x[p.X, p.Y] + m * elem.MaxExchangeAmount;
                    //  Diejenige dieser Basisvariablen, deren Wert gleich \Delta ist, wird aus der
                    //  Basis eliminiert.
                    if (this.x[p.X, p.Y] == 0 && BaseExchange == false)
                    {
                        this.x[p.X, p.Y] = double.NaN;
                        BaseExchange = true;
                    }
                    m = m * (-1);
                }
                // Damit ist eine neue, verbesserte Lösung 
                SetPropertyField("X", ref x, x);
                // mit dem Zielfunktionswert F : = F + \Delta • \bar{c}_{st} bestimmt;
                SetPropertyField("F", ref f, this.F + elem.OppValue * elem.MaxExchangeAmount);
                // beginne erneut mit der Iteration.

            }
            return this;
        }

        /// <summary>
        /// Iteriere über alle Nicht-Basisvariablen und ermittle für diese die Opp-Kosten.
        /// </summary>
        private void calcOppCosts()
        {
            for (int i = 0; i < this.parent.Warehouses.Count; i++)
            {
                for (int j = 0; j < this.parent.Customers.Count; j++)
                {
                    // calc bestPolygon
                    if (IsBaseVariable(i, j) == false)
                    {
                        // vgl. Satz 6.4 Domschke S. 126
                        ElementryCircle elem = getBestElementryCircle(i, j);
                        this.elementryCircles.Add(elem);
                        this.opp[i, j] = elem.OppValue;
                    }
                    else
                    {
                        this.opp[i, j] = double.NaN;
                    }
                }
            }
        }

        /// <summary>
        /// vgl. Domschke Logisitk: transport 4. Auflg. S. 126 --> Fehler im Syntax resultieren aus Texterkennung.
        /// Satz 6.4: Gegeben sei eine zulässige Basislösung eines TPPs. Sei (i,j) eine Nichtbasiszelle, 
        /// die mit Basiszellen den elementaren Kreis [(i,j), (i,r), (u,r) , ... , (s,w), (s,j), (i,j)] bildet. p .. bis p .
        /// seien die den Variablen x„ bis x . im Simplextableau zugeordneten Spaltenvektoren.
        /// IJ SJ
        ///     a) Es gilt p .. = p. - p + ... - p + p . oder 0 = p„ - p. + p - ... + p - p . ; d.h. der
        ///        IJ Ir ur sw SJ IJ Ir ur sw SJ
        ///        Nichtbasisvektor p.. läßt sich mit Hilfe der den Basiszellen des elementaren Kreises 
        ///        IJ zugehörigen Basisvektoren auf sehr einfache Weise linear kombinieren. 
        ///     b) c .. : = c„ - c. + c - ... + c - c . sind die Opportunitätskosten der Nichtbasis variablen
        ///        IJ IJ Ir ur sw SJ x. . . Ist c.. kleiner 0, so läßt sich der Zielfunktionswert der bisherigen 
        ///        Basislösung pro ME, die IJ IJ von A. nach B. geliefert wird, um 1 c .. 1 GE verbessern (verringern).
        /// </summary>
        /// <param name="I">Zeile der Zelle</param>
        /// <param name="J">Spalte der Zelle</param>
        /// <returns>Best(einzigen) Elementarzyklus für die nicht Basiszelle I,J</returns>
        private ElementryCircle getBestElementryCircle(int p, int q)
        {
            ElementryCircle result = new ElementryCircle(this, p, q);
            List<ElementryCircle> candidates = new List<ElementryCircle>();
            candidates.Add(result);
            int iter = 0;
            int I = this.parent.Warehouses.Count();
            int J = this.parent.Customers.Count();
            // Die ist:
            // 0.1 Wähle eine Startzelle hier p,q
            // 0.2 Erstelle einen ersten Elementarzyklus mit der Startzelle 
            // 0.3 Setze die Suchrichtung des aktuellen Zyklus auf Vertical
            // 0.4 Füge den ersten Elementarzyklus der Liste der Kandidaten hinzu
            // 1.  Wähle den ersten Elementarzyklus der Liste der Kandidaten, der noch nicht analysiert wurde; 
            //     Sollte hier kein Elementarzyklus gefunden worden sein, gehe zu 6.
            // 2.  Suche in der Suchrichtung nach Basiszellen
            // 3.a Wenn eine Basisvariable gefunden wurde, dann erstelle einen neuen Zyklus, 
            //     Kopiere dazu den aktuell analysierten und füge die gefundene Basiszelle dem neuen 
            //     Zyklus als nächste Zelle hinzu.
            // 3.b Ändere die Suchrichtung des neuen Zyklus
            // 3.b Füge den neuen Zyklus der Liste der pot. Kandidaten hinzu
            // 4   Ist die Suche in der Suchrichtung abgeschlossen, markiere den aktuellen Zyklus als analysiert.
            // 5.  gehe zu 1.
            // 6.  Wähle aus der Liste der Kandidaten den ersten elementarzyklus aus, der
            //     a) geschlossen ist und
            //     b) die minimalen Opportuntitätskosten aufweist
            //     Anmerkungen zu 6. 
            //          A.6.1 Es ist zu erwarten, dass die Liste der Kandidaten genau(!) 
            //                einen Elementarzyklus aufweist, der geschlossen ist. 
            //          A.6.2 In der Funktion CloneCircle wird geprüft, ob die Zelle i,j 
            //                dem Zyklus hinzugefügt werden darf, oder nicht.
            while (true)
            {
                iter++;

                ElementryCircle elem = candidates.First(x => x.IsAnalysed == false);
                ElementryCircle elemCandidate = null;
                if (elem.SearchVertical)
                {
                    int j = elem.Nodes.Last().Y;

                    for (int i = 0; i < I; i++)
                    {

                        elemCandidate = elem.getCandidateCircle(i, j);
                        if (elemCandidate == null)
                        {

                        }
                        else
                        {
                            candidates.Add(elemCandidate);
                        }
                        elemCandidate = null;
                    }
                }
                else
                {
                    int i = elem.Nodes.Last().X;

                    for (int j = 0; j < J; j++)
                    {
                        elemCandidate = elem.getCandidateCircle(i, j);
                        if (elemCandidate == null)
                        {

                        }
                        else
                        {
                            candidates.Add(elemCandidate);
                        }
                        elemCandidate = null;
                    }
                }
                elem.IsAnalysed = true;
                // candidates.RemoveAll(x=> x.IsAnalysed && !x.Closed);
                if (candidates.Any(x => x.Closed)) break;
                if (iter > I * J * 10) throw new Exception("Should have terminated");
            }
            ///candidates.RemoveAll(x => !x.Closed);
            // choose Element with minimum-Opp-Costs from closed circles
            result = candidates.First(x => x.Closed);
           // Debug.Print("returning result " + result.ToString());
            return result;

        }

        /// <summary>
        /// Erstellt einen neuen Kandidaten.
        /// Falls dieser Kandidat ungültig sein sollte, return NULL
        /// </summary>
        /// <param name="i">Zeile der neuen BasisZelle</param>
        /// <param name="j">Spalte der neuen BasisZelle</param>
        /// <param name="I">Zeile der Ausgangs-Nicht-BasisZelle</param>
        /// <param name="J">Spalte der Ausgangs-Nicht-BasisZelle</param>
        /// <param name="elem">Kandidat, dem eine neue Zelle hinzugfügt werden soll</param>
        /// <returns>einen neuen ElementryCircle = der neue Kandidat oder null</returns>
        private ElementryCircle CreateNewElementryCircleCandidate(int i, int j, int I, int J, ElementryCircle elem)
        {
            ElementryCircle elemClone = null;

            // is this the first Node, that should be added
            Boolean closing = I == i && J == j;
            if (this.IsBaseVariable(i, j) || closing)
            {


            }
            return elemClone;
        }

        #endregion

        #region MODI Methode

        /// <summary>
        /// Wendet die Modi-Methode auf einen Transportplan an.
        /// ACHTUNG: Bei der Berechnung wird IMMER u[0] =0 gesetzt. 
        ///          Dies kann zur Folge haben, dass Lösungen, die sich in der 
        ///          Literatur befinden unterschiedliche Vektoren für U und V aufweisen!
        ///          Die Verwendung von u[0] hat allerdings KEINE Auswirkungen auf die 
        ///          berechneten Opportunitätskosten!
        /// </summary>
        /// <returns></returns>
        private Transportplan ApplyModiMethodIteration()
        {
            Transportplan tpp = new Transportplan(this);
            int I = this.parent.Warehouses.Count();
            int J = this.parent.Customers.Count();
            u = new Double[I];
            v = new Double[J];
            Boolean xRichtung = false;
            for (int i = 0; i < I; i++)
            {
                u[i] = Double.NaN;
            }
            for (int j = 0; j < J; j++)
            {
                v[j] = Double.NaN;
            }
            int iter = 0;
            // Bestimmung von Dualvariablenwerten: Ausgehend von einer zulässigen Basislösung x,
            // bildet man, um die Bedingung (4.9) zu erfüllen, ein lineares Gleichungssystem
            // für alle i und j, deren xij Basisvariable ist.
            // Es enthält m + n Variablen ui und vj sowie m + n – 1 Gleichungen (= Anzahl der Basisvariablen).
            // Nutzt man den vorhandenen Freiheitsgrad, indem man einer der Dualvariablen den
            // Wert 0 zuordnet, so lässt sich das verbleibende System sukzessive leicht lösen.
            u[0] = 0; // vgl. S. 88
            while (true)
            {
                //use formula
                // u(i) <- cij - vj
                // v(j) <- cij - ui
                if (xRichtung)
                {
                    for (int j = 0; j < J; j++)
                    {
                        if (Double.IsNaN(v[j]))
                        {
                            for (int i = 0; i < I; i++)
                            {
                                if (this.IsBaseVariable(i, j) && !Double.IsNaN(u[i]))
                                {
                                    v[j] = this.parent.TPP_C[i, j] - u[i];
                                    break;
                                }
                            }
                        }
                    }
                }
                else
                {
                    for (int i = 0; i < I; i++)
                    {
                        if (Double.IsNaN(u[i]))
                        {
                            for (int j = 0; j < J; j++)
                            {
                                if (this.IsBaseVariable(i, j) && !Double.IsNaN(v[j]))
                                {
                                    u[i] = this.parent.TPP_C[i, j] - v[j];
                                    break;
                                }
                            }
                        }
                    }
                }
                xRichtung = !xRichtung;
                iter++;
                if ((u.Where(o => Double.IsNaN(o)).ToList().Any() ||
                     v.Where(o => Double.IsNaN(o)).ToList().Any()) == false) break;
                if (iter >= (I * J + 1))
                {
                    throw new OverflowException("Should have terminated.");
                }
            }
            double mopp = 0;
            int p = -1;
            int q = -1;
            for (int i = 0; i < I; i++)
            {
                for (int j = 0; j < J; j++)
                {

                    if (this.IsBaseVariable(i, j))
                    {
                        this.opp[i, j] = double.NaN;
                    }
                    else
                    {
                        this.opp[i, j] = this.parent.TPP_C[i, j] - u[i] - v[j];
                        if (this.opp[i, j] < mopp)
                        {
                            mopp = this.opp[i, j];
                            p = i;
                            q = j;
                        }
                    }
                }
            }
            if (mopp < 0)
            {

                ElementryCircle elem = getBestElementryCircle(p, q);
                if (elem.OppValue != mopp)
                {
                    throw new Exception("Opportunitätskosten der Modi Methode haben unerwarteten Wert.");
                }
                double[,] nx = (double[,])this.x.Clone();

                int m = 1; // Multiplikator für abwechselndes Addieren und subtrahieren
                Boolean baseChanged = false;
                for (int i = 0; i < elem.Nodes.Count() - 1; i++)
                {
                    Point pp = elem.Nodes[i];

                    if (i == 0)
                    {
                        nx[pp.X, pp.Y] = elem.MaxExchangeAmount;
                    }
                    else
                    {

                        //    Setze x_{st}: = \Delta. und subtrahiere bzw. addiere von bzw. zu den Werten der Basisvariablen im
                        //    Kreis abwechselnd den Wert \Delta.
                        nx[pp.X, pp.Y] = this.x[pp.X, pp.Y] + m * elem.MaxExchangeAmount;
                        //  Diejenige dieser Basisvariablen, deren Wert gleich \Delta ist, wird aus der
                        //  Basis eliminiert.

                        if (nx[pp.X, pp.Y] == 0 && m == -1 && baseChanged == false)
                        {
                            nx[pp.X, pp.Y] = double.NaN;
                            baseChanged = true;
                        }
                    }
                     
                    m = m * (-1);
                }
                tpp.X = nx;
            }
            SetPropertyField("U", ref this.u, this.u);
            SetPropertyField("V", ref this.v, this.v);
            SetPropertyField("Opp", ref this.opp, this.opp);
            return tpp;
        }


        #endregion

        #endregion

    }
}
