// paths.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// PERQemu is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with PERQemu.  If not, see <http://www.gnu.org/licenses/>.
//

using System.IO;

namespace PERQemu
{
    /// <summary>
    /// Defines the paths pointing to various resources used by PERQemu
    /// and provides helper functions for building paths.
    /// </summary>
    public static class Paths
    {
        public static string Disk
        {
            get { return _disk; }
        }

        public static string PROM
        {
            get { return _prom; }
        }

        public static string BuildDiskPath(string file)
        {
            return Path.Combine(_disk, file);
        }

        public static string BuildScriptsPath(string file)
        {
            return Path.Combine(_scripts, file);
        }

        public static string BuildPROMPath(string file)
        {
            return Path.Combine(_prom, file);
        }

        private static string _disk = "Disks";
        private static string _scripts = "Scripts";
        private static string _prom = "PROM";

    }
}

