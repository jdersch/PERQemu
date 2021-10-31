//
//  Log.cs 
//
//  Copyright (c) 2019, S. Boondoggle <skeezicsb@gmail.com>
//
//  This file is a part of PERQemu, a refactoring of PERQemu originally
//  written by Josh Dersch.
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

using System;
using System.Collections.Generic;

namespace PERQemu
{
    /// <summary>
    /// Filter for the level of verbosity: higher the number, the less that
    /// we pester the user with.  Normal is the default; higher numbers weed
    /// out abnormal conditions, lower numbers increase the amount of detail.
    /// </summary>
    public enum Severity
    {
        All = 0,
        Debug = 1,
        Verbose = 2,
        Info = 3,
        Normal = 4,
        Warning = 5,
        Error = 6,
        Heresy = 7,
        None = 8
    }

    /// <summary>
    /// Bitmap to filter what categories of output to log.  Default is None.
    /// </summary>
    [Flags]
    public enum Category
    {
        None        = 0x0,
        EmuState    = 0x1,
        CPUState    = 0x2,
        Microstore  = 0x4,
        Instruction = 0x8,
        Registers   = 0x10,
        EStack      = 0x20,
        OpFile      = 0x40,
        QCode       = 0x80,
        Shifter     = 0x100,
        ALUState    = 0x200,
        MulDiv      = 0x400,
        MemState    = 0x800,
        MemCycle    = 0x1000,
        RasterOp    = 0x2000,
        Display     = 0x4000,
        IOState     = 0x8000,
        Interrupt   = 0x10000,
        Link        = 0x20000,
        HardDisk    = 0x40000,
        Ethernet    = 0x80000,
        Z80State    = 0x100000,
        Floppy      = 0x200000,
        Keyboard    = 0x400000,
        Tablet      = 0x800000,
        GPIB        = 0x1000000,
        RS232       = 0x2000000,
        Speech      = 0x4000000,
        Canon       = 0x8000000,
        Streamer    = 0x10000000,
        Scheduler   = 0x20000000,
        Timer       = 0x40000000,
        All         = 0x7fffffff
    }

    /// <summary>
    /// Log messages to the console or a file, with filters for the level of
    /// verbosity and other nice features (to aid in debugging, mostly).
    /// </summary>
    /// <design>
    /// By default, formats and writes messages on the console (using colors
    /// for each category).  Can also write to a directory, which the user
    /// can specify (along with a file name pattern and limits on the amount
    /// of data to collect).
    /// </design>
    public static class Log
    {
        static Log()
        {
            _level = Severity.Normal;           // This is normal
            _categories = Category.None;        // Nothing selected
            _logToConsole = true;               // On by default
            _logToFile = false;                 // Log only to the console
            _logSize = 1048576;                 // Default log size is 1MB?
            _logLimit = 9;                      // Keep only 10 files (0..9)
            _logFilePattern = "debug{0}.log";   // Default log file name
            _logDirectory = Paths.OutputDir;    // Default log directory

            SetColors();

#if DEBUG
            // for debugging
            _level = Severity.All;
            _categories = Category.All;

            _loggingAvailable = true;
#else
            _loggingAvailable = false;
#endif
            Write("Log: constructor called.");
        }

        public static Severity Level
        {
            get { return _level; }
            set { _level = value; }
        }

        public static Category Categories
        {
            get { return _categories; }
            set { _categories = value; }
        }

        public static bool ToConsole => _logToConsole;

        public static bool ToFile => _logToFile;

        public static bool LoggingAvailable => _loggingAvailable;

#if DEBUG
        /// <summary>
        /// A plain Write() is always displayed.
        /// </summary>
        public static void Write(string msg, params object[] args)
        {
            Write(Severity.None, Category.All, msg, args);
        }

        /// <summary>
        /// A shortcut for logging debugging output.  Makes a slow process just
        /// that much slower but saves a tiny amount of typing.  Feh.
        /// </summary>
        public static void Debug(Category c, string msg, params object[] args)
        {
            Write(Severity.Debug, c, msg, args);
        }

        public static void Write(Category c, string msg, params object[] args)
        {
            Write(Severity.Normal, c, msg, args);
        }

        public static void Write(Severity s, Category c, string msg, params object[] args)
        {
            // Apply filters before we do the work to format the output
            if ((s >= _level) && ((c & _categories) != 0))
            {
                var output = String.Format((c == Category.All ? "" : c.ToString() + ": ") + msg, args);

                if (_logToConsole)
                {
                    // Set the text color
                    Console.ForegroundColor = _colors[c];

                    // Set the background color
                    // In severe cases, force the foreground for readability
                    switch (s)
                    {
                        case Severity.Warning:
                            Console.BackgroundColor = ConsoleColor.Yellow;
                            Console.ForegroundColor = ConsoleColor.Black;
                            break;

                        case Severity.Error:
                        case Severity.Heresy:
                            Console.BackgroundColor = ConsoleColor.Red;
                            Console.ForegroundColor = ConsoleColor.White;
                            break;

                        default:
                            Console.BackgroundColor = ConsoleColor.Black;
                            break;
                    }

                    Console.WriteLine(output);

                    // Reset to default colors  TODO: configurable
                    Console.BackgroundColor = ConsoleColor.Black;
                    Console.ForegroundColor = ConsoleColor.Green;
                }

                if (_logToFile)
                {
                    // TODO:
                    // check the length of the current file
                    //      if curlen + len(output) > _logSize
                    //          RotateFile();
                    //_stream.WriteLine(datestamp + output);
                }
            }
        }
#else
        // No-ops
        public static void Write(string msg, params object[] args) { }
        public static void Debug(Category c, string msg, params object[] args) { }
        public static void Write(Category c, string msg, params object[] args) { }
        public static void Write(Severity s, Category c, string msg, params object[] args) { }
#endif

        /// <summary>
        /// Sets up a dictionary for mapping Categories to console colors.
        /// It might be nice if this was a user configurable mapping!
        /// </summary>
        private static void SetColors()
        {
            _colors = new Dictionary<Category, ConsoleColor>();

            //
            // TODO: actually put some thought into these defaults and test 'em
            // out on both Mac terminal and Win console
            //
            _colors.Add(Category.None, ConsoleColor.Black);     // HeartOfGold theme
            _colors.Add(Category.EmuState, ConsoleColor.White);
            _colors.Add(Category.CPUState, ConsoleColor.Cyan);
            _colors.Add(Category.Microstore, ConsoleColor.Magenta);
            _colors.Add(Category.Instruction, ConsoleColor.Magenta);
            _colors.Add(Category.Registers, ConsoleColor.Red);
            _colors.Add(Category.EStack, ConsoleColor.Red);
            _colors.Add(Category.OpFile, ConsoleColor.Magenta);
            _colors.Add(Category.QCode, ConsoleColor.Magenta);
            _colors.Add(Category.Shifter, ConsoleColor.DarkGreen);
            _colors.Add(Category.ALUState, ConsoleColor.Green);
            _colors.Add(Category.MulDiv, ConsoleColor.DarkGreen);
            _colors.Add(Category.MemState, ConsoleColor.Cyan);
            _colors.Add(Category.MemCycle, ConsoleColor.DarkCyan);
            _colors.Add(Category.RasterOp, ConsoleColor.Green);
            _colors.Add(Category.Display, ConsoleColor.Gray);
            _colors.Add(Category.IOState, ConsoleColor.Cyan);
            _colors.Add(Category.Interrupt, ConsoleColor.Red);
            _colors.Add(Category.Link, ConsoleColor.DarkGreen);
            _colors.Add(Category.HardDisk, ConsoleColor.DarkGray);
            _colors.Add(Category.Ethernet, ConsoleColor.DarkGray);
            _colors.Add(Category.Z80State, ConsoleColor.Green);
            _colors.Add(Category.Floppy, ConsoleColor.Blue);
            _colors.Add(Category.Keyboard, ConsoleColor.Blue);
            _colors.Add(Category.Tablet, ConsoleColor.Blue);
            _colors.Add(Category.GPIB, ConsoleColor.Blue);
            _colors.Add(Category.RS232, ConsoleColor.Blue);
            _colors.Add(Category.Speech, ConsoleColor.DarkBlue);
            _colors.Add(Category.Canon, ConsoleColor.DarkGray);
            _colors.Add(Category.Streamer, ConsoleColor.DarkGray);
            _colors.Add(Category.Scheduler, ConsoleColor.Green);
            _colors.Add(Category.Timer, ConsoleColor.Green);
            _colors.Add(Category.All, ConsoleColor.White);
        }

#if DEBUG
        public static void ShowColors()
        {
            foreach (Severity s in Enum.GetValues(typeof(Severity)))
            {
                foreach (Category c in Enum.GetValues(typeof(Category)))
                {
                    Write(s, c, "Test!");
                }
            }
        }
#endif

        private static Severity _level;
        private static Category _categories;
        private static Dictionary<Category, ConsoleColor> _colors;

        private static bool _loggingAvailable;
        private static bool _logToConsole;
        private static bool _logToFile;
        private static string _logDirectory;
        private static string _logFilePattern;
        private static int _logSize;
        private static int _logLimit;
    }
}
