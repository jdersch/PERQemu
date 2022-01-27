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
    	Z80DataOut = 0,
    	Y = 1,
    	HardDisk = 2,
    	Network = 3,
    	Z80DataIn = 4,
    	LineCounter = 5,
    	X = 6,
    	Parity = 7
    }

    /// <summary>
    /// In this format, group them all together for display.  Cheese.
    /// </summary>
    [Flags]
    public enum InterruptFlag
    {
        None = 0x00,
        Z80DataOutReady = 0x01,
        Y = 0x02,
        HardDisk = 0x04,
        Network = 0x08,
        Z80DataInReady = 0x10,
        LineCounter = 0x20,
        X = 0x40,
        Parity = 0x80
    }

    /// <summary>
    /// InterruptEncoder manages the thread-safe raising and lowering of
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
    /// clears it).  But it seems to be just as fast WITH the interlocks, so...
    /// </remarks>
    public class InterruptEncoder
    {
        public InterruptEncoder()
        {
            _intr = new long[(int)InterruptSource.Parity + 1];
        }

        public void Reset()
        {
            _intr.Initialize();
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
        /// currently signalled.
        /// </summary>
        /// <remarks>
        /// Tests show it's safe to avoid Interlocked.Read() on all these
        /// since the only possible ill effect is if Z80DataIn/Out changes,
        /// but the protocol itself should cope just fine (the state machine
        /// can wait a blown cycle *or* deal with a read from an empty FIFO).
        /// I'm probably just handwaving away potential thread-safety issues
        /// but feh, if something blows up we can revisit it.  La la la la la...
        /// </remarks>
        public int Priority
        {
            get
            {
                return ((_intr[(int)InterruptSource.Parity] > 0) ? 7 :
                        (_intr[(int)InterruptSource.X] > 0) ? 6 :
                        (_intr[(int)InterruptSource.LineCounter] > 0) ? 5 :
                        (_intr[(int)InterruptSource.Z80DataIn] > 0) ? 4 :
                        (_intr[(int)InterruptSource.Network] > 0) ? 3 :
                        (_intr[(int)InterruptSource.HardDisk] > 0) ? 2 :
                        (_intr[(int)InterruptSource.Y] > 0) ? 1 :
                        0);
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
