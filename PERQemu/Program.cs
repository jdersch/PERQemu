//
// Program.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

using System;
using System.Reflection;

using PERQemu.UI;
using PERQemu.Config;

namespace PERQemu
{
    static class PERQemu
    {
        static PERQemu()
        {
            //
            // Locate our startup directory and make it our working directory.  This
            // is necessary for running under the Mono profiler app, but also makes
            // it easier to locate other resources like ROM images, graphics, etc.
            //
            _baseDir = new Uri(Assembly.GetExecutingAssembly().CodeBase).LocalPath;
            _baseDir = System.IO.Path.GetDirectoryName(_baseDir);
            Environment.CurrentDirectory = _baseDir;

            // Set a platform flag
            _hostIsUnix = (Environment.OSVersion.Platform == PlatformID.Unix ||
                           Environment.OSVersion.Platform == PlatformID.MacOSX);

            //
            // "Man is born to trouble, as the sparks fly upwards" - Job Ch.5
            //      -- Change log in Layered/mulReal.High
            //
            Version vers = Assembly.GetCallingAssembly().GetName().Version;
            _version = string.Format("PERQemu v{0}.{1}.{2} ('As the sparks fly upwards.')",
                                    vers.Major, vers.Minor, vers.Build);

            _initialized = false;
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

            // Since we use reflection to build up our CLI and GUI, let the
            // user know this might take a while.  It's the polite thing to do.
            Console.Title = "PERQemu";
            Console.WriteLine("Initializing, please wait...");
            Console.Out.Flush();

            if (_switches.debug)
            {
                Log.Level = Severity.Debug;
                Log.Categories = Category.All;
            }

            // Set up command-line parser and GUI manager
            _cli = new CommandProcessor();

            //_gui = new FormsManager();
            _gui = new EventLoop();

            // Create main objects
            _controller = new ExecutionController();

            _config = new Configurator();
            _config.Initialize();

            // Read user settings file, or set defaults if it doesn't yet exist
            Settings.Load();
            Log.Info(Category.All, Settings.Reason);

            _initialized = true;

            // Start 'er up!
            Run();

            // Close up shop
            HighResolutionTimer.Shutdown();

            // Save the settings if they've changed
            Settings.Save();
            Log.Info(Category.All, Settings.Reason);
            Log.Shutdown();
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
                // _gui.Run("FrontPanel");          // Sigh.
                Console.WriteLine("No GUI for you!");
            }

            // Run the CLI
            _cli.Run();
        }

        public static void PrintBanner()
        {
            Console.WriteLine();
            Console.WriteLine(Version);
            Console.WriteLine(Copyright);
#if DEBUG
            Console.WriteLine();
            Console.WriteLine("[DEBUG version]");
            Console.WriteLine("[Working directory is {0}]", Environment.CurrentDirectory);
            Console.WriteLine("[Host is configured for {0} processor(s)]", Environment.ProcessorCount);
            Console.WriteLine("[High resolution timer {0} available]",
                              HighResolutionTimer.IsHighResolution ? "is" : "is not");
            Console.WriteLine("[Console is {0}x{1}]", Console.BufferWidth, Console.BufferHeight);
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

        public static string Copyright = "Copyright (c) 2006-2022, J. Dersch (derschjo@gmail.com)\n" +
                                         "Feebly assisted by S. Boondoggle (skeezicsb@gmail.com)";

        public static string Version => _version;
        public static string BaseDir => _baseDir;
        public static bool HostIsUnix => _hostIsUnix;
        public static bool Initialized => _initialized;

        public static EventLoop GUI => _gui;
        public static CommandProcessor CLI => _cli;
        public static Configurator Config => _config;
        public static ExecutionController Controller => _controller;
        public static PERQSystem Sys => _controller.System;


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
        private static bool _initialized;

        private static CmdLineArgs _switches;
        private static EventLoop _gui;
        private static CommandProcessor _cli;

        private static Configurator _config;
        private static ExecutionController _controller;
    }
}
