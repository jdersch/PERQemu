//
//  DevicePerformance.cs
//
//  Author:  S. Boondoggle <skeezicsb@gmail.com>
//
//  Copyright (c) 2022, Boondoggle Heavy Industries, Ltd.
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

namespace PERQmedia
{
    /// <summary>
    /// Device performance characteristics that the emulator cares about.
    /// </summary>
    public struct DevicePerformance
    {
        public DevicePerformance(int rpm, int pulse, int delay, int seekMin, int seekMax, int settle, int xfer)
        {
            RPM = rpm;
            IndexPulse = pulse;
            StartupDelay = delay;
            MinimumSeek = seekMin;
            MaximumSeek = seekMax;
            HeadSettling = settle;
            TransferRate = xfer;
        }

        public override string ToString()
        {
            return string.Format("[RPM {0}, Index {1:n}us, Delay {2}sec, Rate {3:n}KB/sec]\n" +
                                 "[Min seek {4}ms, Max seek {5}ms, Settling {6}ms]", RPM,
                                 (IndexPulse / 1000.0),     // ns -> usec
                                 (StartupDelay / 1000),     // msec -> sec
                                 (TransferRate / 1000.0),   // bytes -> Kbytes
                                 MinimumSeek, MaximumSeek, HeadSettling);
        }

        public readonly int RPM;
        public readonly int IndexPulse;      // in nsec
        public readonly int StartupDelay;    // in msec
        public readonly int MinimumSeek;     // in msec
        public readonly int MaximumSeek;     // in msec
        public readonly int HeadSettling;    // in msec
        public readonly int TransferRate;    // in bytes/sec

        //
        // Shugart SA851 8" floppy drive
        //
        public static DevicePerformance SA851 = new DevicePerformance(360, 1800000, 90, 3, 231, 15, 62500);

        //
        // Shugart SA4000 series 14" hard drives
        //
        public static DevicePerformance SA4000 = new DevicePerformance(2964, 1100, 90000, 20, 140, 20, 888375);

        //
        // Micropolis 1200 series 8" hard drives
        //
        public static DevicePerformance M1200 = new DevicePerformance(3600, 10000, 25000, 4, 96, 8, 992000);

        //
        // Micropolis 1300 series 5.25" MFM hard drives
        //
        public static DevicePerformance M1300 = new DevicePerformance(3600, 200000, 25000, 7, 66, 1, 625000);

        //
        // Toshiba MK-50FB series 5.25" MFM hard drives
        // Toshiba MK-134FA 3.5" MFM hard drive
        //
        public static DevicePerformance MK50 = new DevicePerformance(3600, 10000, 25000, 6, 45, 1, 625000);
        public static DevicePerformance MK134 = new DevicePerformance(3600, 10000, 25000, 7, 50, 1, 625000);

        //
        // Vertex (Priam) V100 series 5.25" MFM drives
        //
        public static DevicePerformance V100 = new DevicePerformance(3600, 200000, 25000, 5, 60, 3, 625000);

        //
        // Maxtor XT-1000 series 5.25" MFM hard drives
        // Maxtor XT-2000 series 5.25" MFM hard drives
        //
        public static DevicePerformance XT1000 = new DevicePerformance(3600, 5340, 20000, 4, 420, 3, 625000);

        //
        // CDC 9760 series SMD hard drives (removable pack)
        //
        public static DevicePerformance CDC9760 = new DevicePerformance(3600, 2500, 30000, 6, 55, 1, 1209600);

        //
        // Fujitsu Eagle series SMD hard drives (wicked cool)
        //
        public static DevicePerformance M2351A = new DevicePerformance(3961, 1076, 30000, 5, 35, 1, 1859000);
        public static DevicePerformance M2361A = new DevicePerformance(3600, 1627, 40000, 5, 35, 1, 2458000);

        //
        // Archive Sidewinder 3000 series QIC tape drives
        //
        // Notes: Assuming 3RCC only shipped the 30ips drive, not the 90ips?
        // Only the max transfer rate figure is accurate; the other specs will
        // have to be computed or defined in some useful way based on how the
        // streaming controller is devised.
        //
        public static DevicePerformance A3020I = new DevicePerformance(30, 0, 10000, 0, 0, 0, 200000);
    }
}
