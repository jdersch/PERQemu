//
// Program.cs - Copyright (c) 2019-2021 S. Boondoggle (skeezicsb@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// PERQemu is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
// See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with PERQemu.  If not, see <http://www.gnu.org/licenses/>.
//

// TODO
// Integrate this as the new top-level entry point as the project is
// restructured.  Move from the PERQolator namespace into PERQemu.

using System;
using System.Reflection;

//using PERQemu.UI.Forms;
//using PERQemu.Config;

namespace PERQemu
{
    static class PERQemu
    {
        static PERQemu()
        {
            // Locate our startup directory and make it our working directory.  This
            // is necessary for running under the Mono profiler app, but also makes
            // it easier to locate other resources like ROM images, graphics, etc.
            _baseDir = new Uri(Assembly.GetExecutingAssembly().CodeBase).LocalPath;
            _baseDir = System.IO.Path.GetDirectoryName(_baseDir);
            Environment.CurrentDirectory = _baseDir;

            //
            // "Man is born to trouble, as the sparks fly upwards" - Job Ch.5
            //      -- Change log in Layered/mulReal.High
            //
            Version vers = Assembly.GetCallingAssembly().GetName().Version;
            _version = String.Format("PERQemu v{0}.{1}.{2} ('As the sparks fly upwards.')",
                                    vers.Major, vers.Minor, vers.Build);

            // Set a platform flag
            _hostIsUnix = (Environment.OSVersion.Platform == PlatformID.Unix ||
                           Environment.OSVersion.Platform == PlatformID.MacOSX);

            // Since we use reflection to build up our CLI and GUI, let the
            // user know this might take a while.  It's the polite thing to do.
            Console.WriteLine("Initializing, please wait...");
            Console.Out.Flush();
        }

        [STAThread]
        public static void Main(string[] args)
        {
            // Parse command line arguments
            _switches = ParseArgs(args);

            if (_switches.printVersion)
            {
                Console.WriteLine(Version);
                return;
            }

            if (_switches.printHelp)
            {
                Console.WriteLine("Usage:  PERQemu [-h] [-v] [-g] [<script>]");
                Console.WriteLine();
                Console.WriteLine("\t-h\tprint this help message");
                Console.WriteLine("\t-v\tprint version information");
                Console.WriteLine("\t-g\tstart up in GUI mode");
                Console.WriteLine("\tscript\tread startup commands from file");
                return;
            }

            if (_switches.debug)
            {
                Log.Level = Severity.Debug;
                Log.Categories = Category.All;
            }

            // Set up our command-line parser and GUI manager
            _cli = new CommandProcessor();
            _gui = new FormsManager();

            // Create our main objects
            _config = new Configurator();
            _controller = new ExecutionController();

            // Read user settings file, or set defaults if it doesn't yet exist
            Settings.Load();
            Console.WriteLine(Settings.Reason);     // DEBUG

            // Initialize the ExecutionController
            _controller.Initialize(_config.Current);

            // Start 'er up!
            Run();

            // Save the settings if they've changed
            Settings.Save();
            Console.WriteLine(Settings.Reason);     // DEBUG
        }

        public static void Run()
        {
            PrintBanner();

            // If the user requested a start-up script, read it now
            if (!string.IsNullOrEmpty(_switches.runScript))
            {
                _cli.ReadScript(_switches.runScript);
            }

            // If the GUI is requested, start up the FrontPanel display
            if (_switches.startGUI)
            {
                _gui.Run("FrontPanel");
            }
            else
            {
                // Run the CLI
                _cli.Run();
            }
        }

        public static void PrintBanner()
        {
            Console.WriteLine(Version);
            Console.WriteLine(Copyright);
#if DEBUG
            Console.WriteLine();
            Console.WriteLine("[DEBUG version]");
            Console.WriteLine("[Working directory is {0}]", Environment.CurrentDirectory);
            Console.WriteLine("[Host is configured for {0} processor(s)]", Environment.ProcessorCount);
#endif
#if TRACING_ENABLED
            Console.WriteLine("[Tracing is available]");
#endif
            if (HostIsUnix && !_switches.startGUI)
            {
                Console.WriteLine("Type 'help' for console commands, or 'gui' to start the GUI.");
            }
            else
            {
                Console.WriteLine("[Debug logging output]");
            }
        }

        public static string Copyright = "Copyright (c) 2006-2021, J. Dersch (derschjo@gmail.com)";

        public static string Version
        {
            get { return _version; }
        }

        public static string BaseDir
        {
            get { return _baseDir; }
        }

        public static bool HostIsUnix
        {
            get { return _hostIsUnix; }
        }

        public static FormsManager GUI
        {
            get { return _gui; }
        }

        public static CommandProcessor CLI
        {
            get { return _cli; }
        }

        public static PERQSystem Sys
        {
            get { return _controller.System; }
        }

        public static Configurator Config
        {
            get { return _config; }
        }

        public static ExecutionController Controller
        {
            get { return _controller; }
        }

        /// <summary>
        /// Quick and dirty command line parsing.
        /// </summary>
        private static CmdLineArgs ParseArgs(string[] args)
        {
            CmdLineArgs sw = new CmdLineArgs();

            for (int i = 0; i < args.Length; i++)
            {
                if (args[i] == "-h")
                {
                    sw.printHelp = true;
                }
                else if (args[i] == "-v")
                {
                    sw.printVersion = true;
                }
                else if (args[i] == "-g")
                {
                    sw.startGUI = true;
                }
                else if (args[i] == "-d")
                {
                    sw.debug = true;
                }
                else if (!args[i].StartsWith("-", StringComparison.InvariantCulture) &&
                         string.IsNullOrEmpty(sw.runScript))
                {
                    sw.runScript = args[i];
                }
                else
                {
                    Console.WriteLine("Unknown argument: " + args[i]);
                    sw.printHelp = true;
                }
            }
            return sw;
        }

        internal struct CmdLineArgs
        {
            public bool printHelp;
            public bool printVersion;
            public bool startGUI;
            public bool debug;
            public string runScript;
        }

        private static string _version;
        private static string _baseDir;
        private static bool _hostIsUnix;

        private static CmdLineArgs _switches;
        private static FormsManager _gui;
        private static CommandProcessor _cli;

        private static Configurator _config;
        private static ExecutionController _controller;
    }
}
