using clHNUORExcel.BaseClasses;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace clHNUORExcel.Calculation.SimpleMath
{
   public class DistanceCalculation
    {
       public static double getBeeLineDistance(double x1, double y1, double x2 = 0, double y2 = 0)
       {

           double distance = 0;

           try
           {
               distance = Math.Sqrt(Math.Pow(x1-x2,2)+Math.Pow(y1-y2,2));
           }
           catch (Exception ex)
           { 
               throw ex;
           }

           return distance;

       }
       public static double getBeeLineDistance(Node n1, Node n2 = null)
       {
           if (n2 == null)
           {
               n2 = new Node { X = 0, Y = 0 };
           }
           return getBeeLineDistance(n1.X, n1.Y, n2.X, n2.Y);
       }

    }
}
