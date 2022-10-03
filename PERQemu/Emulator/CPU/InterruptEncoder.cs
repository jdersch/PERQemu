//
// InterruptEncoder.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
using System.Threading;

namespace PERQemu.Processor
{

    /// <summary>
    /// PERQ hardware interrupts, listed from lowest to highest priority.
    /// </summary>
    public enum InterruptSource
    {
        // How they're wired into the priority encoder
        Parity = 0,
        X = 1,
        LineCounter = 2,
        Z80DataIn = 3,
        Network = 4,
        HardDisk = 5,
        Y = 6,
        Z80DataOut = 7
    }

    /// <summary>
    /// In this format, group them all together for display.  Cheese.
    /// </summary>
    [Flags]
    public enum InterruptFlag
    {
        None = 0x00,
        Parity = 0x01,
        X = 0x02,
        LineCounter = 0x04,
        Z80DataInReady = 0x08,
        Network = 0x10,
        HardDisk = 0x20,
        Y = 0x40,
        Z80DataOutReady = 0x80
    }

    /// <summary>
    /// InterruptEncoder manages the mostly thread-safe raising and lowering of
    /// interrupt flags by the peripheral controllers.
    /// </summary>
    /// <remarks>
    /// We want this to be fast, while still being reasonably safe, so a few
    /// liberties are taken (like, we only protect the raise/clear but don't
    /// use Interlocked.Read). Preliminary benchmarks/tests indicate that we
    /// shave a few nanoseconds off the microcycle time compared to other lock
    /// methods, but a test with NO Interlocks also shows that just setting/
    /// clearing the array values seems to run without conflicts or threading
    /// issues (since generally the only code that raises an interrupt also
    /// clears it, on the same thread).
    /// </remarks>
    public class InterruptEncoder
    {
        public InterruptEncoder()
        {
            _intr = new long[8];
        }

        public void Reset()
        {
            for (var i = 0; i < _intr.Length; i++)
                _intr[i] = 0;

            Log.Debug(Category.Interrupt, "Priority encoder reset");
        }

        public long Raise(InterruptSource i)
        {
            return Interlocked.Exchange(ref _intr[(int)i], (1 << (int)i));
        }

        public long Clear(InterruptSource i)
        {
            return Interlocked.Exchange(ref _intr[(int)i], 0);
        }

        /// <summary>
        /// Returns the integer vector of the highest priority interrupt
        /// currently signaled.
        /// </summary>
        public int Priority
        {
            get
            {   // Marginally faster than the stacked trinary block?
                if (_intr[(int)InterruptSource.Z80DataOut] > 0) return 7;
                if (_intr[(int)InterruptSource.Y] > 0) return 6;
                if (_intr[(int)InterruptSource.HardDisk] > 0) return 5;
                if (_intr[(int)InterruptSource.Network] > 0) return 4;
                if (_intr[(int)InterruptSource.Z80DataIn] > 0) return 3;
                if (_intr[(int)InterruptSource.LineCounter] > 0) return 2;
                if (_intr[(int)InterruptSource.X] > 0) return 1;
                return 0;   // InterruptSource.Parity
            }
        }

        /// <summary>
        /// Gets the current status of all the interrupts in one "flag word"
        /// like the original implementation.
        /// </summary>
        public InterruptFlag Flag
        {
            get
            {
                return (InterruptFlag)(_intr[(int)InterruptSource.Parity] |
                                       _intr[(int)InterruptSource.X] |
                                       _intr[(int)InterruptSource.LineCounter] |
                                       _intr[(int)InterruptSource.Z80DataIn] |
                                       _intr[(int)InterruptSource.Network] |
                                       _intr[(int)InterruptSource.HardDisk] |
                                       _intr[(int)InterruptSource.Y] |
                                       _intr[(int)InterruptSource.Z80DataOut]);
            }
        }

        private long[] _intr;
    }
}
