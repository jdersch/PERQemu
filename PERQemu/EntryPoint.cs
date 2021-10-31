// entrypoint.cs - Copyright 2006-2021 Josh Dersch (derschjo@gmail.com)
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
        [STAThread]
        static void Main(string[] args)
        {
            EntryPoint p = new EntryPoint();
            p.Run(args);
        }

        public EntryPoint()
        {
            //
            // When running under the Profiler, our working directory is off in some
            // app wrapper directory, so we can't locate the PROM/ or Scripts/ folders
            // and initialization fails.  Try to set our working directory to the
            // location of PERQemu.exe.  We have to do this before instantiating the
            // EntryPoint, since that creates PERQSystem.  Oof.
            //
            var path = new Uri(Assembly.GetExecutingAssembly().CodeBase).LocalPath;
            Environment.CurrentDirectory = System.IO.Path.GetDirectoryName(path);

            Console.CancelKeyPress += OnCtrlC;
        }

        void OnCtrlC(object sender, ConsoleCancelEventArgs e)
        {
            // Cancel the event since we don't actually want to kill the
            // emulator;  just break into the execution.  TODO: this should
            // actually interrupt the ExecutionController which will stop the
            // machine if it is configured and running.
            e.Cancel = true;
            Console.Write("^C");

            if (_system != null)
            {
                _system.Break();
            }
        }

        public void Run(string[] args)
        {
            PrintBanner();

            // TODO: move this into the execution controller.  Instead, here we'll
            // start up the CLI and let the user configure a machine or parse the
            // command line (run a script arg) to load a saved config.  The machine
            // will then be dynamically instantiated, run, and torn down at runtime.
            _system = new PERQSystem();
            _system.Execute(args);
        }

        public static void PrintBanner()
        {
            //
            // "Man is born to trouble, as the sparks fly upwards" - Job Ch.5
            //      -- Change log in Layered/multibus/micro/Real.High
            //
            Version currentVersion = Assembly.GetCallingAssembly().GetName().Version;
            Console.WriteLine("PERQemu v{0}.{1}.{2} ('As the sparks fly upwards.')",
                              currentVersion.Major, currentVersion.Minor, currentVersion.Build);
            Console.WriteLine("Copyright (c) 2006-2021, J. Dersch (derschjo@gmail.com)");
            Console.WriteLine("                     and S. Boondoggle (skeezicsb@gmail.com)");
            Console.WriteLine("Type 'go' to start the machine; hit 'tab' key for a list of command completions.");
            Console.WriteLine();
#if DEBUG
            Console.WriteLine("DEBUG version.");
            Console.WriteLine("[Working directory is {0}]", Environment.CurrentDirectory);
            Console.WriteLine("[Host is configured for {0} processor(s)]", Environment.ProcessorCount);
#endif
#if TRACING_ENABLED
            Console.WriteLine("Tracing is available.");
#endif
        }

        private PERQSystem _system;
    }
}
