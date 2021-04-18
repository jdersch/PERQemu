// entrypoint.cs - Copyright 2006-2018 Josh Dersch (derschjo@gmail.com)
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

using System;
using System.Reflection;

namespace PERQemu
{
    class EntryPoint
    {
        static void Main(string[] args)
        {
            EntryPoint p = new EntryPoint();
            p.Run(args);
        }

        public EntryPoint()
        {
            CreateSystem();
            Console.CancelKeyPress += new ConsoleCancelEventHandler(OnCtrlC);
        }

        void OnCtrlC(object sender, ConsoleCancelEventArgs e)
        {
            // Cancel the event since we don't actually want to kill the emulator,
            // just break into the execution.
            e.Cancel = true;

            if (_system != null)
            {
                _system.Break();
            }
        }

        public void Run(string[] args)
        {
            PrintBanner();
            _system.Execute(args);
        }

        private void CreateSystem()
        {
            _system = new PERQSystem();
        }

        private void PrintBanner()
        {
            Version currentVersion = Assembly.GetCallingAssembly().GetName().Version;
            Console.WriteLine("PERQemu v{0}.{1}.{2} ('As the sparks fly upwards.')",
                              currentVersion.Major, currentVersion.Minor, currentVersion.Build);
            Console.WriteLine("Copyright (c) 2006-2018, J. Dersch (derschjo@gmail.com).");
            Console.WriteLine("                     and S. Boondoggle (skeezicsb@gmail.com).");
            Console.WriteLine("Type 'go' to start the machine; hit 'tab' key for a list of command completions.");
            Console.WriteLine();
#if DEBUG
            Console.WriteLine("DEBUG version.");
#endif
#if TRACING_ENABLED
            Console.WriteLine("Tracing is available.");
#endif
        }

        private PERQSystem _system;
    }
}

