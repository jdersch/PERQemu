// trace.cs - Copyright 2006-2018 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu
{
    /// <summary>
    /// Specifies the category of Trace message. Trace messages can be limited
    /// to a certain set, this defines individual entries in those sets.
    /// </summary>
    [Flags]
    public enum LogType : ulong
    {
        None =              0x0,
        Errors =            0x1,
        Warnings =          0x2,
        DDS =               0x4,            // could be reassigned
        EmuState =          0x8,
        CpuState =          0x10,
        AluState =          0x20,
        RegisterAssignment = 0x40,
        IOState =           0x80,
        MemoryState =       0x100,
        Z80State =          0x200,
        MicrocodeStore =    0x400,
        Shifter =           0x800,
        HardDisk =          0x1000,
        MemoryFetch =       0x2000,         // consolidate?
        MemoryStore =       0x4000,         // -> Canon?
        FloppyDisk =        0x8000,
        OpFile =            0x10000,
        Interrupt =         0x20000,
        Display =           0x40000,
        QCode =             0x80000,        // could be synonym for OpFile, reused?
        EStack =            0x100000,
        RasterOp =          0x200000,
        GPIB =              0x400000,
        Z80FIFO =           0x800000,
        Speech =            0x1000000,
        RS232 =             0x2000000,
        Tablet =            0x4000000,
        Link =              0x8000000,
        Instruction =       0x10000000,
        Keyboard =          0x20000000,
        MulDiv =            0x40000000,
        Z80SIO =            0x80000000,
        All =               0xffffffffffffffff,
    }

#if TRACING_ENABLED
    public static class Trace
    {
        public static void Log(LogType level, string format, params object[] args)
        {
            if ((_level & level) == level)
            {
                // Sleep so we give the console time to breathe.
                System.Threading.Thread.Sleep(0);
                // OK to trace
                SetColor(level);
                Console.WriteLine(format, args);
                Console.ForegroundColor = ConsoleColor.Gray;
            }
        }

        public static void Log(LogType level, int pad, string format, params object[] args)
        {
            if ((_level & level) == level)
            {
                // Sleep so we give the console time to breathe.
                System.Threading.Thread.Sleep(0);
                // OK to trace
                SetColor(level);
                Console.Write("".PadRight(pad));
                Console.WriteLine(format, args);
                Console.ForegroundColor = ConsoleColor.Gray;
            }
        }

        public static LogType TraceLevel
        {
            get { return _level; }
            set
            {
                _level = value;
                if (_level == LogType.None)
                {
                    Trace.TraceOn = false;
                }
                else
                {
                    Trace.TraceOn = true;
                }
            }
        }

        /// <summary>
        /// Selects a color for the given trace type.
        /// </summary>
        private static void SetColor(LogType level)
        {
            ConsoleColor color = ConsoleColor.Gray;
            switch (level)
            {
                case LogType.Errors:
                    color = ConsoleColor.Red;
                    break;

                case LogType.DDS:
                case LogType.Warnings:
                    color = ConsoleColor.Yellow;
                    break;

                case LogType.RasterOp:
                case LogType.MulDiv:
                    color = ConsoleColor.Green;
                    break;

                case LogType.MemoryFetch:
                case LogType.MemoryStore:
                case LogType.MemoryState:
                    color = ConsoleColor.Cyan;
                    break;

                case LogType.QCode:
                case LogType.Instruction:
                    color = ConsoleColor.Magenta;
                    break;

                case LogType.RegisterAssignment:
                case LogType.Shifter:
                    color = ConsoleColor.DarkRed;
                    break;

                case LogType.OpFile:
                case LogType.EStack:
                    color = ConsoleColor.DarkYellow;
                    break;

                case LogType.HardDisk:
                    color = ConsoleColor.DarkGreen;
                    break;

                case LogType.Z80State:
                case LogType.Display:
                    color = ConsoleColor.DarkCyan;
                    break;

                case LogType.Z80FIFO:
                    color = ConsoleColor.DarkRed;
                    break;

                case LogType.Link:
                case LogType.GPIB:
                    color = ConsoleColor.DarkMagenta;
                    break;

                default:
                    // No change
                    break;
            }

            Console.ForegroundColor = color;
        }

        private static LogType _level;
        public static bool TraceOn = false;
    }
#endif
}

