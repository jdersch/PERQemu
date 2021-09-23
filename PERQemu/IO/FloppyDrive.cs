

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
