using clHNUORExcel.BaseClasses;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace clHNUORExcel.Calculation.SimpleMath
{
    public class PolarangleCalculation
    {
        public static double getPolarAngle(double x1, double y1, double x2 = 0, double y2 = 0)
        {
            double result = 0;
            double dx = x1 - x2;
            double dy = y1 - y2;

            try
            {
                if (dx == 0)
                {
                    if (dy == 0)
                    {
                        throw new ArithmeticException("Wrong value: angle can be calculated for different nodes only!");
                    }
                    if (dy > 0)
                    {
                        result = Math.PI / 2;
                    }
                    else
                    {
                        result = Math.PI *3/ 2; 
                    }
                }
                else
                {
                    if (dx > 0)
                    {
                        result = Math.Atan(dy / dx);   
                    }
                    else
                    {
                        result = Math.Atan(dy / dx) + Math.PI;  

                    }
                }

                if (result < 0)
                {
                    result += Math.PI * 2;
                }
            }
            catch (Exception ex)
            {
                
                throw ex;
            }

            return result;
        }
       
        
        /// <summary>
        /// Calculates the angle
        /// </summary>
        /// <param name="n1">is the target-Node e.g. a customer</param>
        /// <param name="n2">is the Base-Node e.g. a warehouse</param>
        /// <returns></returns>
        public static double getPolarAngle(Node n1, Node n2 = null)
        {
            if (n2 == null)
            {
                n2 = new Node { X = 0, Y = 0 };
            }
            return getPolarAngle(n1.X, n1.Y, n2.X, n2.Y);
        }
    }
}
