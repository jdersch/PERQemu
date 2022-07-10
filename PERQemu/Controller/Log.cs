//
// Log.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
using System.IO;
using System.Text;
using System.Threading;
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
        Verbose = 1,
        Debug = 2,
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
    /// <remarks>
    /// Did you forget to add your new category to the colors dictionary?
    /// </remarks>
    [Flags]
    public enum Category : ulong
    {
        None        = 0x0,
        Emulator    = 0x1,
        Controller  = 0x2,
        Debugger    = 0x4,
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
        Z80Inst     = 0x2000000,
        Z80IRQ      = 0x4000000,
        Z80DMA      = 0x8000000,
        CTC         = 0x10000000,
        SIO         = 0x20000000,
        RTC         = 0x40000000,
        FloppyDisk  = 0x80000000,
        Keyboard    = 0x100000000,
        Tablet      = 0x200000000,
        GPIB        = 0x400000000,
        RS232       = 0x800000000,
        Speech      = 0x1000000000,
        Link        = 0x2000000000,
        HardDisk    = 0x4000000000,
        Ethernet    = 0x8000000000,
        Canon       = 0x10000000000,
        Streamer    = 0x20000000000,
        Multibus    = 0x40000000000,
        SMD         = 0x80000000000,
        MediaLoader = 0x100000000000000,
        UI          = 0x1000000000000000,
        Timer       = 0x2000000000000000,
        All         = 0xffffffffffffffff
    }

    /// <summary>
    /// Log messages to the console and/or a file, with filters for the level of
    /// verbosity and other nice features (to aid in debugging, mostly).
    /// </summary>
    /// <design>
    /// By default, formats and writes messages on the console (using colors for
    /// each category).  Can also write to a directory, specified as OutputDir
    /// (user settable); currently the filename pattern, number and size of the
    /// files are fixed and not exposed as Settings.  Note that enabling All
    /// categories in Verbose mode can fill 100MB of logs before VFY even starts
    /// up, so *selective* logging is key.  Bumped the default log size to 10MB.
    /// </design>
    /// <remarks>
    /// While indispensible in debugging, logging is _painfully_ slow, so most
    /// of the methods are compiled out completely in Release builds.
    /// </remarks>
    public static class Log
    {
        static Log()
        {
            _categories = Category.None;        // Nothing selected

            _consLevel = Severity.Normal;       // This is normal
            _logToConsole = true;               // On by default

            _fileLevel = Severity.Info;         // A bit more info to the file
            _logToFile = false;                 // Log only to the console

            _logSize = 1024 * 1024 * 10;        // Default log size is 10MB?
            _logLimit = 99;                     // Keep 100 files? (0..99)
            _logDirectory = Paths.OutputDir;
            _currentFile = string.Empty;

            _lastOutput = string.Empty;
            _repeatCount = 0;

            SetColors();

#if DEBUG
            // Set a _reasonable_ default for debugging
            _consLevel = Severity.Debug;
            _categories = Category.Emulator | Category.Controller | Category.UI;
#endif


#if TRACING_ENABLED
            _loggingAvailable = true;

            _log = null;
            _turnstile = -1;
            _logFilePattern = "debug{0:d2}.log";
            _currentFileNum = 0;

            Initialize();
#else
            _loggingAvailable = false;
#endif
        }


        public static Category Categories
        {
            get { return _categories; }
            set { _categories = value; }
        }

        public static Severity Level
        {
            get { return _consLevel; }
            set { _consLevel = value; SetMinLevel(); }
        }

        public static Severity FileLevel
        {
#if TRACING_ENABLED
            get { return _fileLevel; }
            set { _fileLevel = value; SetMinLevel(); }
#else
            get { return Severity.None; }
            set { _fileLevel = value; }
#endif
        }

        public static bool ToFile
        {
#if TRACING_ENABLED
            get { return _logToFile; }
            set { _logToFile = Enable(value); }
#else
            get { return false; }
            set { _logToFile = value; }
#endif
        }

        public static bool ToConsole
        {
            get { return _logToConsole; }
            set { _logToConsole = value; }
        }

        public static bool LoggingAvailable => _loggingAvailable;
        public static string OutputFile => _currentFile;


        private static void SetMinLevel()
        {
            _minLevel = (Severity)Math.Min((int)_consLevel, (int)_fileLevel);
        }

        /// <summary>
        /// A shortcut for logging debugging output.  Makes a slow Debug process
        /// that much slower but is compiled out entirely in Release builds.
        /// </summary>
        [Conditional("DEBUG")]
        [Conditional("TRACING_ENABLED")]
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static void Debug(Category c, string fmt, params object[] args)
        {
            WriteInternal(Severity.Debug, c, fmt, args);
        }

        /// <summary>
        /// A shortcut for logging extra detailed debugging output.  Like, stuff
        /// that's reeeally verbose.  Like Debug, adds a ton of overhead but is
        /// compiled out entirely in Release builds.
        /// </summary>
        [Conditional("DEBUG")]
        [Conditional("TRACING_ENABLED")]
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static void Detail(Category c, string fmt, params object[] args)
        {
            WriteInternal(Severity.Verbose, c, fmt, args);
        }

        /// <summary>
        /// Shortcut for displaying informational messages (verbose mode,
        /// not in performance-critical situations).
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static void Info(Category c, string fmt, params object[] args)
        {
            WriteInternal(Severity.Info, c, fmt, args);
        }

        /// <summary>
        /// Shortcut for debug warnings that should stand out (but are non-fatal
        /// and can be ignored in Release builds).
        /// </summary>
        public static void Warn(Category c, string fmt, params object[] args)
        {
            WriteInternal(Severity.Warning, c, fmt, args);
        }

        /// <summary>
        /// Shortcut for displaying a serious error.
        /// </summary>
        /// <remarks>
        /// Gosh, it's a shame Console doesn't support <blink>attributes</blink>.
        /// </remarks>
        public static void Error(Category c, string fmt, params object[] args)
        {
            WriteInternal(Severity.Error, c, fmt, args);
        }

        /// <summary>
        /// A plain Write() is always displayed.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static void Write(string fmt, params object[] args)
        {
            WriteInternal(Severity.None, Category.All, fmt, args);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static void Write(Category c, string fmt, params object[] args)
        {
            WriteInternal(Severity.Normal, c, fmt, args);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static void Write(Severity s, Category c, string fmt, params object[] args)
        {
            WriteInternal(s, c, fmt, args);
        }

        private static void WriteInternal(Severity s, Category c, string fmt, params object[] args)
        {
            // Apply filters before we do the work to format the output
#if DEBUG
            if ((s >= Severity.Warning) || ((s >= _minLevel) && ((c & _categories) != 0)))
#else
            if ((s >= _minLevel) && ((c & _categories) != 0) || (c == Category.All))
#endif
            {
                var output = string.Format((c == Category.All ? "" : c.ToString() + ": ") + fmt, args);

                // Cut down on the noise: things like the processor looping to
                // check an I/O status byte spews a lot... summarize that (but
                // not endlessly to show progress)
                if (output == _lastOutput && _repeatCount < 256)
                {
                    _repeatCount++;
                    return;
                }

                if (_logToConsole && s >= _consLevel)
                {
                    // Was the last message repeated?
                    if (_repeatCount > 0)
                    {
                        if (_repeatCount == 1)
                            Console.WriteLine(_lastOutput);     // one repeat is ok :-)
                        else
                            Console.WriteLine($"[Last message repeated {_repeatCount} times]");
                    }

                    // Set the text color; in severe cases, override them to standout
                    // Since the Mac Terminal blats the background color across the
                    // entire output line, fudge the output to look a little better...
                    switch (s)
                    {
                        case Severity.Warning:
                            Console.BackgroundColor = ConsoleColor.Yellow;
                            Console.ForegroundColor = ConsoleColor.Black;

                            Console.Write(output);
                            Console.BackgroundColor = _defaultBackground;
                            Console.WriteLine();
                            break;

                        case Severity.Error:
                        case Severity.Heresy:
                            Console.BackgroundColor = ConsoleColor.Red;
                            Console.ForegroundColor = ConsoleColor.White;

                            Console.Write(output);
                            Console.BackgroundColor = _defaultBackground;
                            Console.WriteLine();
                            break;

                        default:
                            Console.ForegroundColor = _colors[c];
                            Console.WriteLine(output);
                            break;
                    }

                    Console.ForegroundColor = _defaultForeground;

                    Thread.Sleep(0);   // Give it a rest why dontcha
                }

                _lastOutput = output;
                _repeatCount = 0;

#if TRACING_ENABLED
                if (_logToFile && s >= _fileLevel)
                {
                    var me = Thread.CurrentThread.ManagedThreadId;

                    // Does the file need to be rotated?
                    if (Interlocked.Read(ref _turnstile) < 0 && _log.BaseStream.Length > _logSize)
                    {
                        // Make sure only one thread does it!
                        if (Interlocked.Exchange(ref _turnstile, _currentFileNum) < 0)
                        {
                            RotateFile();

                            // While we're here...
                            _log.WriteLine("{0:yyyyMMdd HHmmss.ffff} [{1}]: {2}",
                                           DateTime.Now, me, output);

                            // Reset for next time!
                            Interlocked.Exchange(ref _turnstile, -1);

                            return;
                        }
                    }

                    // If the rotation is in progress, hold up here
                    if (Interlocked.Read(ref _turnstile) >= 0)
                    {
                        while (Interlocked.Read(ref _turnstile) >= 0)
                        {
                            Thread.SpinWait(10);
                        }
                    }

                    // Write it, lazy & slow (_log is Synchronized)
                    // But this is still wrong, results in corrupt log entries,
                    // and someday I should fix it but right now it's enough to
                    // help with the debugging.  At least until (under heavy
                    // load) it blows up Mono and crashes.  So much for relying
                    // on the "synchronized" stream.  Time for a more methodical
                    // locking approach, or per-thread streams and no locking?
                    _log.WriteLine("{0:yyyyMMdd HHmmss.ffff} [{1}]: {2}",
                                   DateTime.Now, me, output);
                }
#endif
            }
        }


#if TRACING_ENABLED

        /// <summary>
        /// Enumerate the existing log files in the output directory.  Since this
        /// can take some time, we do it once at startup.
        /// </summary>
        /// <remarks>
        /// Sets _currentFile and _currentFileNum, and if necessary creates the
        /// OutputDir.
        /// </remarks>
        public static void Initialize()
        {
            // Create the output directory if necessary
            if (!Directory.Exists(_logDirectory))
            {
                Directory.CreateDirectory(_logDirectory);
                Console.WriteLine("Created output directory: " + Paths.Canonicalize(_logDirectory));

                _currentFileNum = 0;
                _currentFile = string.Format(_logFilePattern, _currentFileNum);
                return;
            }

            // Now look for the newest file and make that our current log
            _currentFile = string.Empty;
            _currentFileNum = 0;
            var latest = DateTime.MinValue;

            foreach (var file in Directory.GetFiles(_logDirectory, "debug??.log"))
            {
                if (File.GetLastWriteTime(file) > latest)
                {
                    _currentFile = file;
                    latest = File.GetLastWriteTime(file);
                }
            }

            if (!string.IsNullOrEmpty(_currentFile))
            {
                _currentFileNum = GetFileNum(_currentFile);
            }

            // Ready for if/when file logging is enabled
            _currentFile = Paths.BuildOutputPath(string.Format(_logFilePattern, _currentFileNum));
        }

        private static int GetFileNum(string filename)
        {
            // For now, assume the fixed pattern "debugNN.log".  Yuck...
            return Convert.ToInt32(Path.GetFileName(_currentFile).Substring(5, 2));
        }

        /// <summary>
        /// Enable or disable logging to file.  When enabling, always opens an
        /// existing file in Append mode, or creates it anew.
        /// </summary>
        private static bool Enable(bool enable)
        {
            if (enable == _logToFile)
            {
                return enable;      // No change
            }

            if (enable)
            {
                _log = File.AppendText(_currentFile);
                Stream.Synchronized(_log.BaseStream);
                return true;
            }

            if (_log != null)
            {
                _log.Flush();       // A bit of overkill, hmm?
                _log.Close();
                _log.Dispose();
                _log = null;
            }
            return false;
        }

        /// <summary>
        /// Rotates the current log file.
        /// </summary>
        private static void RotateFile()
        {
            // Prep the next filename
            _currentFileNum = (_currentFileNum < _logLimit ? _currentFileNum + 1 : 0);
            _currentFile = Paths.BuildOutputPath(string.Format(_logFilePattern, _currentFileNum));

            // Do the switcheroo
            _log.Flush();
            _log.Close();
            _log.Dispose();

            // On rotate, overwrite rather than append
            _log = File.CreateText(_currentFile);
            Stream.Synchronized(_log.BaseStream);
        }
#endif

        /// <summary>
        /// Last one out, turn off the lights.
        /// </summary>
        [Conditional("TRACING_ENABLED")]
        public static void Shutdown()
        {
            ToConsole = false;
            WriteInternal(Severity.None, Category.All, "PERQemu shutting down at {0}", DateTime.Now);
            ToFile = false;
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
            _colors.Add(Category.MediaLoader, ConsoleColor.DarkYellow);
            _colors.Add(Category.Controller, ConsoleColor.Yellow);
            _colors.Add(Category.Debugger, ConsoleColor.DarkYellow);
            _colors.Add(Category.Scheduler, ConsoleColor.Green);
            _colors.Add(Category.Timer, ConsoleColor.Green);
            _colors.Add(Category.DDS, ConsoleColor.Yellow);
            _colors.Add(Category.UI, ConsoleColor.Gray);

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
            _colors.Add(Category.Z80Inst, ConsoleColor.Gray);
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

        private static Severity _minLevel;
        private static Severity _consLevel;
        private static Severity _fileLevel;
        private static Category _categories;

        private static Dictionary<Category, ConsoleColor> _colors;

        private static ConsoleColor _defaultForeground = Console.ForegroundColor;
        private static ConsoleColor _defaultBackground = Console.BackgroundColor;

        private static bool _loggingAvailable;
        private static bool _logToConsole;
        private static bool _logToFile;
        private static string _logDirectory;
        private static string _currentFile;
        private static string _lastOutput;
        private static int _repeatCount;
        private static int _logSize;
        private static int _logLimit;

#if TRACING_ENABLED
        private static string _logFilePattern;
        private static int _currentFileNum;

        private static long _turnstile;
        private static StreamWriter _log;
#endif
    }
}
