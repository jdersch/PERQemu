//
//  StorageDevice.cs
//
//  Author:  S. Boondoggle <skeezicsb@gmail.com>
//
//  Copyright (c) 2022-2024, Boondoggle Heavy Industries, Ltd.
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
using System.Runtime.CompilerServices;

namespace PERQmedia
{
    /// <summary>
    /// Encapsulates a PERQ storage device: hard disk, floppy disk, or tape.
    /// Provides a block interface for clients to access the underlying data,
    /// but leaves all higher-level control and status functions up to the
    /// derived classes and the Controllers they attach to.
    /// </summary>
    /// <remarks>
    /// Also captures all of the relevant metadata from the media file to more
    /// fully characterize a device's performance.  If the PERQmedia storage
    /// format isn't used, some of the fields will be empty or set to defaults.
    /// </remarks>
    public class StorageDevice
    {
        public StorageDevice()
        {
            IsLoaded = false;
            IsModified = false;
            Filename = string.Empty;

            FileInfo = new MediaInfo();

            Info = new DeviceInfo();
            Specs = new DevicePerformance();
            Geometry = new DeviceGeometry();
            Sectors = null;
        }

        public StorageDevice(string filename) : this()
        {
            Filename = filename;
        }

        public StorageDevice(DeviceGeometry geom) : this()
        {
            Geometry = geom;
        }

        public StorageDevice(DeviceInfo info, DeviceGeometry geom, DevicePerformance perf) : this()
        {
            Info = info;
            Geometry = geom;
            Specs = perf;
        }


        /// <summary>
        /// Read the specified cyl, head and sec without validation.  Will throw
        /// an exception if out of range.  Override or Validate() if you want
        /// error checking, ya big chicken.
        /// </summary>
        public virtual Sector Read(ushort cyl, byte head, ushort sec)
        {
            try
            {
                return Sectors[cyl, head, sec];
            }
            catch (IndexOutOfRangeException e)
            {
                throw new IndexOutOfRangeException($"Read sector {cyl}/{head}/{sec} out of range", e);
            }
        }

        /// <summary>
        /// Unchecked fast write of a sector.  Sets IsModified for the device.
        /// </summary>
        /// <remarks>
        /// We do NOT check the IsWritable flag here to prevent writes; that's
        /// up to the controller to prevent changes to read-only devices and
        /// report the status to the PERQ/Z80 appropriately?  Will throw an
        /// exception if given illegal C/H/S.  See above.
        /// </remarks>
        public virtual void Write(Sector sec)
        {
            try
            {
                Sectors[sec.CylinderID, sec.HeadID, sec.SectorID] = sec;
                IsModified = true;
            }
            catch (IndexOutOfRangeException e)
            {
                throw new IndexOutOfRangeException($"Write {sec} out of range", e);
            }
        }

        /// <summary>
        /// Validate that a sector write is within bounds AND the device is writable.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public virtual bool WriteCheck(Sector sec)
        {
            return Info.IsWritable && Validate(sec.CylinderID, sec.HeadID, sec.SectorID);
        }

        /// <summary>
        /// Validate that a given cyl/head/sec are within the bounds.
        /// </summary>
        /// <remarks>
        /// Y'know, if we're gonna throw an exception anyway, it's probably
        /// faster to just try/catch rather than do the extra method call!
        /// Exceptions ought to be extremely rare given that the PERQ I/O code
        /// kind of went out of its way to not scribble outside the lines.
        /// </remarks>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool Validate(ushort cyl, byte head, ushort sec)
        {
            return ((cyl < Geometry.Cylinders) &&
                    (head < Geometry.Heads) &&
                    (sec < Geometry.Sectors));
        }

        /// <summary>
        /// Allocates the Data array based on the device's geometry.  (To be
        /// filled in by the Reader.)
        /// </summary>
        public void CreateSectors()
        {
            Sectors = new Sector[Geometry.Cylinders, Geometry.Heads, Geometry.Sectors];
            IsModified = false;
        }

        /// <summary>
        /// Creates a new empty image from the device geometry.  Because it's a
        /// new virtual image, all the sectors are marked good.  We could add a
        /// few random defects just for fun.  It wouldn't be an authentic PERQ
        /// experience without a few random address errors now and then.
        /// </summary>
        public void Format()
        {
            if (Sectors == null)
            {
                CreateSectors();
            }

            for (ushort c = 0; c < Geometry.Cylinders; c++)
            {
                for (byte h = 0; h < Geometry.Heads; h++)
                {
                    for (ushort s = 0; s < Geometry.Sectors; s++)
                    {
                        Sectors[c, h, s] = new Sector(c, h, s, Geometry.SectorSize, Geometry.HeaderSize);
                    }
                }
            }
        }

        /// <summary>
        /// A private hook for subclasses to be notified when a Load() is
        /// successfully completed.  This allows drives with removable media
        /// to reinitialize themselves, for instance.
        /// </summary>
        public virtual void OnLoad()
        {
            IsLoaded = true;
            IsModified = false;
        }

        /// <summary>
        /// Unload the data and mark the drive as unloaded.
        /// </summary>
        public virtual void Unload()
        {
            Sectors = null;
            IsLoaded = false;
            IsModified = false;
            Filename = string.Empty;
        }

        
        public string Filename;
        public bool IsLoaded;
        public bool IsModified;

        public MediaInfo FileInfo;

        public DeviceInfo Info;
        public DevicePerformance Specs;
        public DeviceGeometry Geometry;

        // Should be protected from direct access...
        public Sector[,,] Sectors;
    }
}
