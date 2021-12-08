//
// FloppyDrive.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.PhysicalDisk;

namespace PERQemu.IO
{
    public class FloppyDrive
    {
        public FloppyDrive()
        {
            Reset();
        }

        public FloppyDisk Disk
        {
            get { return _disk; }
        }

        public int Track
        {
            get { return _track; }
        }

        public bool IsLoaded
        {
            get { return _disk != null; }
        }

        public bool IsWriteProtected
        {
            get { return _disk != null ? _disk.IsWriteProtected : false; }
        }

        public bool IsSingleSided
        {
            get { return _singleSided; }
        }

        public bool Track0
        {
            get { return _track == 0; }
        }

        public bool DiskChange
        {
            get { return _diskChange; }
        }

        public bool DriveSelect
        {
            get { return _driveSelect; }
            set
            {
                _driveSelect = value;

                //
                // The Disk Change signal is reset when
                // Drive Select goes low.
                //
                if (!_driveSelect)
                {
                    _diskChange = false;
                }
            }
        }

        public void Reset()
        {
            _track = 0;
            _singleSided = false;
            _diskChange = false;
            _driveSelect = false;
        }

        public void LoadDisk(FloppyDisk disk)
        {
            _disk = disk;
            _singleSided = _disk.IsSingleSided;
            _diskChange = true;
        }

        public void UnloadDisk()
        {
            if (_disk != null && _disk.IsModified)
            {
                _disk.Save();
            }

            _disk = null;
            _diskChange = true;
        }

        public void SeekTo(int track)
        {
            // Clip into range.
            _track = Math.Max(0, track);
            _track = Math.Min(76, _track);
        }

        private bool _singleSided;
        private int _track;
        private bool _diskChange;
        private bool _driveSelect;
        private FloppyDisk _disk;
    }
}
