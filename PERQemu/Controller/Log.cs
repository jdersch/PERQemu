//
// Log.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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
using System.Diagnostics;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

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
    /// Bitmap to filter what categories of output to log.
    /// </summary>
    [Flags]
    public enum Category : ulong
    {
        None        = 0x0,
        Emulator    = 0x1,
        Controller  = 0x2,
        Timer       = 0x4,
        Scheduler   = 0x8,
        CPU         = 0x10,
        Sequencer   = 0x20,
        Microstore  = 0x40,
        Instruction = 0x80,
        Registers   = 0x100,
        EStack      = 0x200,
        OpFile      = 0x400,
        QCode       = 0x800,
        Shifter     = 0x1000,
        ALU         = 0x2000,
        MulDiv      = 0x4000,
        Memory      = 0x8000,
        MemCycle    = 0x10000,
        RasterOp    = 0x20000,
        Display     = 0x40000,
        IO          = 0x80000,
        Interrupt   = 0x100000,
        DMA         = 0x200000,
        FIFO        = 0x400000,
        DDS         = 0x800000,
        Z80         = 0x1000000,
        Z80IRQ      = 0x2000000,
        Z80DMA      = 0x4000000,
        CTC         = 0x8000000,
        SIO         = 0x10000000,
        RTC         = 0x20000000,
        FloppyDisk  = 0x40000000,
        Keyboard    = 0x80000000,
        Tablet      = 0x100000000,
        GPIB        = 0x200000000,
        RS232       = 0x400000000,
        Speech      = 0x800000000,
        Link        = 0x1000000000,
        HardDisk    = 0x2000000000,
        Ethernet    = 0x4000000000,
        Canon       = 0x8000000000,
        Streamer    = 0x10000000000,
        Multibus    = 0x20000000000,
        SMD         = 0x40000000000,
        All         = 0xffffffffffffffff
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

            _currentFile = string.Empty;
            _lastOutput = string.Empty;
            _repeatCount = 0;

            SetColors();

#if DEBUG
            // for debugging
            _level = Severity.All;
            _categories = Category.All;
#endif

#if TRACING_ENABLED
            _loggingAvailable = true;
#else
            _loggingAvailable = false;
#endif
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

        public static bool ToFile
        {
            get { return _logToFile; }
            set { _logToFile = value; }
        }

        public static bool ToConsole
        {
            get { return _logToConsole; }
            set { _logToConsole = value; }
        }

        public static bool LoggingAvailable => _loggingAvailable;
        public static string OutputFile => _currentFile;

        /// <summary>
        /// A plain Write() is always displayed.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static void Write(string fmt, params object[] args)
        {
            WriteInternal(Severity.None, Category.All, fmt, args);
        }

        /// <summary>
        /// Shortcut for displaying informational messages (verbose mode,
        /// not in performance-critical situations).
        /// </summary>
        public static void Info(Category c, string fmt, params object[] args)
        {
            WriteInternal(Severity.Info, c, fmt, args);
        }

        /// <summary>
        /// Shortcut for displaying a serious error.
        /// </summary>
        public static void Error(Category c, string fmt, params object[] args)
        {
            WriteInternal(Severity.Error, c, fmt, args);
        }

        /// <summary>
        /// A shortcut for logging debugging output.  Makes a slow Debug process
        /// that much slower but is compiled out entirely in Release builds.
        /// </summary>
        [Conditional("DEBUG")]
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static void Debug(Category c, string fmt, params object[] args)
        {
            WriteInternal(Severity.Debug, c, fmt, args);
        }

        /// <summary>
        /// Shortcut for debug warnings that should stand out (but are non-fatal
        /// and can be ignored in Release builds).
        /// </summary>
        [Conditional("DEBUG")]
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static void Warn(Category c, string fmt, params object[] args)
        {
            WriteInternal(Severity.Warning, c, fmt, args);
        }

        [Conditional("TRACING_ENABLED")]
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static void Write(Category c, string fmt, params object[] args)
        {
            WriteInternal(Severity.Normal, c, fmt, args);
        }

        [Conditional("TRACING_ENABLED")]
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static void Write(Severity s, Category c, string fmt, params object[] args)
        {
            WriteInternal(s, c, fmt, args);
        }

        private static void WriteInternal(Severity s, Category c, string fmt, params object[] args)
        {
            // Apply filters before we do the work to format the output
            if ((s >= _level) && ((c & _categories) != 0))
            {
                var output = string.Format((c == Category.All ? "" : c.ToString() + ": ") + fmt, args);

                // Cut down on the noise: things like the processor looping to
                // check an I/O status byte spews a lot... summarize that.
                if (output == _lastOutput && _repeatCount < 100)
                {
                    _repeatCount++;
                    return;
                }

                if (_logToConsole)
                {
                    // Was the last message repeated?
                    if (_repeatCount > 0)
                    {
                        if (_repeatCount == 1)
                            Console.WriteLine(_lastOutput);     // one repeat is ok :-)
                        else
                            Console.WriteLine("[Last message repeated {0} times.]", _repeatCount);
                    }

                    // Set the text color; in severe cases, override them to standout
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
                            Console.ForegroundColor = _colors[c];
                            break;
                            
                    }

                    Console.WriteLine(output);

                    Console.BackgroundColor = _defaultBackground;
                    Console.ForegroundColor = _defaultForeground;

                    System.Threading.Thread.Sleep(0);   // Give it a rest why dontcha
                }

                if (_logToFile)
                {
                    // TODO:
                    // check the length of the current file
                    //      if curlen + len(output) > _logSize
                    //          RotateFile();
                    // check for repeats (as above)
                    //_stream.WriteLine(datestamp + output);
                }

                _lastOutput = output;
                _repeatCount = 0;
            }
        }

        /// <summary>
        /// Sets up a dictionary for mapping Categories to console colors,
        /// to aid in distinguishing output when debugging.
        /// </summary>
        private static void SetColors()
        {
            _colors = new Dictionary<Category, ConsoleColor>();

            //
            // TODO: actually put some thought into these defaults and test 'em
            // out on both Mac terminal and Win console.  Better still, make
            // them user preferences and save 'em in the Settings file. :-)
            //

            // General messages
            _colors.Add(Category.All, ConsoleColor.White);
            _colors.Add(Category.None, ConsoleColor.Black);     // HeartOfGold theme
            _colors.Add(Category.Emulator, ConsoleColor.White);
            _colors.Add(Category.Controller, ConsoleColor.Yellow);
            _colors.Add(Category.Scheduler, ConsoleColor.Green);
            _colors.Add(Category.Timer, ConsoleColor.Green);
            _colors.Add(Category.DDS, ConsoleColor.Yellow);

            // CPU components
            _colors.Add(Category.CPU, ConsoleColor.Cyan);
            _colors.Add(Category.Sequencer, ConsoleColor.DarkCyan);
            _colors.Add(Category.Microstore, ConsoleColor.Magenta);
            _colors.Add(Category.Instruction, ConsoleColor.Magenta);
            _colors.Add(Category.Registers, ConsoleColor.Red);
            _colors.Add(Category.EStack, ConsoleColor.Red);
            _colors.Add(Category.OpFile, ConsoleColor.Magenta);
            _colors.Add(Category.QCode, ConsoleColor.Magenta);
            _colors.Add(Category.ALU, ConsoleColor.Green);
            _colors.Add(Category.MulDiv, ConsoleColor.DarkGreen);
            _colors.Add(Category.Shifter, ConsoleColor.DarkGreen);

            // Memory and Video
            _colors.Add(Category.Memory, ConsoleColor.Cyan);
            _colors.Add(Category.MemCycle, ConsoleColor.DarkCyan);
            _colors.Add(Category.RasterOp, ConsoleColor.Green);
            _colors.Add(Category.Display, ConsoleColor.DarkGreen);

            // Z80 and I/O controllers
            _colors.Add(Category.IO, ConsoleColor.Gray);
            _colors.Add(Category.DMA, ConsoleColor.Yellow);
            _colors.Add(Category.Interrupt, ConsoleColor.Red);
            _colors.Add(Category.FIFO, ConsoleColor.DarkYellow);
            _colors.Add(Category.Z80, ConsoleColor.Green);
            _colors.Add(Category.Z80DMA, ConsoleColor.DarkGreen);
            _colors.Add(Category.Z80IRQ, ConsoleColor.DarkRed);
            _colors.Add(Category.CTC, ConsoleColor.Cyan);
            _colors.Add(Category.SIO, ConsoleColor.DarkCyan);
            _colors.Add(Category.RTC, ConsoleColor.DarkBlue);
            _colors.Add(Category.GPIB, ConsoleColor.Blue);
            _colors.Add(Category.RS232, ConsoleColor.DarkBlue);
            _colors.Add(Category.Speech, ConsoleColor.DarkBlue);

            // Peripherals
            _colors.Add(Category.Tablet, ConsoleColor.Blue);
            _colors.Add(Category.Keyboard, ConsoleColor.Blue);
            _colors.Add(Category.FloppyDisk, ConsoleColor.DarkCyan);
            _colors.Add(Category.Multibus, ConsoleColor.DarkBlue);
            _colors.Add(Category.HardDisk, ConsoleColor.DarkGreen);
            _colors.Add(Category.Ethernet, ConsoleColor.Gray);
            _colors.Add(Category.Streamer, ConsoleColor.Gray);
            _colors.Add(Category.Canon, ConsoleColor.Gray);
            _colors.Add(Category.Link, ConsoleColor.Gray);
            _colors.Add(Category.SMD, ConsoleColor.DarkBlue);
        }

        [Conditional("DEBUG")]
        public static void ShowColors()
        {
            foreach (Severity s in Enum.GetValues(typeof(Severity)))
            {
                foreach (Category c in Enum.GetValues(typeof(Category)))
                {
                    WriteInternal(s, c, "Test!");
                }
            }
        }

        private static Severity _level;
        private static Category _categories;
        private static Dictionary<Category, ConsoleColor> _colors;

        private static ConsoleColor _defaultForeground = Console.ForegroundColor;
        private static ConsoleColor _defaultBackground = Console.BackgroundColor;

        private static bool _loggingAvailable;
        private static bool _logToConsole;
        private static bool _logToFile;
        private static string _logDirectory;
        private static string _logFilePattern;
        private static string _currentFile;
        private static string _lastOutput;
        private static int _repeatCount;
        private static int _logSize;
        private static int _logLimit;
    }
}
