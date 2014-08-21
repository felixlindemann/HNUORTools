using Extensibility;
using Microsoft.Win32;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;

namespace ExcelTools.Worksheetfunctions
{
    public interface IWorkSheetFunction : IDTExtensibility2
    {
        void RegisterFunction(Type type);
        void UnregisterFunction(Type type);

        bool IsVersionNum(string s);
    }
   
}
