//
// PERQLink.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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
using System.Runtime.InteropServices;

namespace PERQemu.IO
{
    /// <summary>
    /// Implements logic for the PERQLink interface.
    /// </summary>
    public sealed class PERQLink
    {
        public PERQLink()
        {
#if PERQLINK
            _linkData = new SharedLinkData("PERQLink");
#endif
        }

        public void Reset()
        {
        }

        public void Clock()
        {

#if PERQLINK
            //
            // Synchronize the two processes in lockstep here.
            // This may not be sufficient, if the execution speed
            // of the two emulation processes differs significantly.
            //
            int bufCyc = 0;
            if (_linkData.EndPoint == Endpoint.Debugger)
            {
                _linkData.DebuggerEvent.WaitOne();
                _linkData.TargetEvent.Set();
                bufCyc = _linkData.ControlTarget;
            }
            else
            {
                _linkData.DebuggerEvent.Set();
                _linkData.TargetEvent.WaitOne();
                bufCyc = _linkData.ControlDebugger;
            }

            if ((bufCyc & 0x2) != 0)
            {
                _bufCycLatch = 0x2;
            }
#endif
        }

        public int ReadCommandStatus()
        {
#if PERQLINK
            int value = 0;

            if (_linkData.EndPoint == Endpoint.Debugger)
            {
                value = _linkData.ControlTarget;
            }
            else
            {
                value = _linkData.ControlDebugger;
            }

            int mangledValue = ((value) & 0x1);
            mangledValue |= ((~_bufCycLatch) & 0x2);
            mangledValue |= (value & 0x4);
            mangledValue |= (value & 0x8);

            value = mangledValue;

            // reset latch
            _bufCycLatch = 0;

#if TRACING_ENABLED
           // if (Trace.TraceOn) Log.Debug(Category.Link, "PERQLink CSR read {0:x1}.", (value & 0xf));
#endif

            return value;
#else
            return 0xff;    // Just indicate that nothing's there...
#endif
        }

        public int ReadData()
        {
#if PERQLINK
            int value = 0;

            if (_linkData.EndPoint == Endpoint.Debugger)
            {
                value = _linkData.DataTarget;
            }
            else
            {
                value = _linkData.DataDebugger;
            }

#if TRACING_ENABLED
                Log.Debug(Category.Link, "PERQLink Data debugger {0:x4}.", _linkData.DataDebugger);
                Log.Debug(Category.Link, "PERQLink Data target {0:x4}.", _linkData.DataTarget);
                Log.Debug(Category.Link, "PERQLink Data read {0:x4}.", value);

            if (value == 0x14e5)        // Hello
            {
                Log.Debug(Category.Link, "Read HELLO!", value);
            }
#endif

            return value;
#else
            return 0;
#endif
        }

        public void WriteCommandStatus(int value)
        {
#if PERQLINK
#if TRACING_ENABLED
            // Log.Debug(Category.Link, "PERQLink CSR write {0:x1}.", (value & 0xf));
#endif

            if (_linkData.EndPoint == Endpoint.Debugger)
            {
                _linkData.ControlDebugger = value;
            }
            else
            {
                _linkData.ControlTarget = value;
            }
#endif
        }

        public void WriteData(int value)
        {
#if PERQLINK
#if TRACING_ENABLED

            if (value == 0x14e5)        // Hello
            {
                Log.Debug(Category.Link, "Wrote HELLO!", value);
            }
#endif

            if (_linkData.EndPoint == Endpoint.Debugger)
            {
                _linkData.DataDebugger = value;
                // Log.Debug(Category.Link, "PERQLink Data write {0:x4}.", _linkData.DataDebugger);
            }
            else
            {
                _linkData.DataTarget = value;
               // Log.Debug(Category.Link, "PERQLink Data write {0:x4}.", _linkData.DataTarget);
            }
#endif
        }

#if PERQLINK
        private SharedLinkData _linkData;
        private int _bufCycLatch;
#endif
        }

    /// <summary>
    /// Data marshalled to and from shared memory via SharedLinkData
    /// </summary>
    [StructLayout(LayoutKind.Sequential)]
    public struct LinkData
    {
        public int DataTarget;
        public int DataDebugger;
        public int ControlTarget;
        public int ControlDebugger;
    }

    public enum Endpoint
    {
        Target,
        Debugger
    }

#if PERQLINK
    /// <summary>
    /// Encapsulates win32 shared memory used to allow PERQLink to communicate between two processes,
    /// as well as a shared event for interlocking execution of the two processes.
    ///
    /// The shared memory contains a LinkData object (as above).
    ///
    /// NOTE: This is not portable.  Grumble.
    ///
    /// </summary>
    public sealed class SharedLinkData
    {
        public SharedLinkData(string linkName)
        {
            _linkName = linkName;

            OpenSharedMemory();
            OpenOrCreateEvents();

            DataTarget = 0;
            DataDebugger = 0;
            ControlTarget = 0;
            ControlDebugger = 0;
        }

        ~SharedLinkData()
        {
            //
            // Release our handle to the shared memory, if valid.
            //
            if (_pBuffer != IntPtr.Zero)
            {
                UnmapViewOfFile(_pBuffer);
            }

            if (_hMapFile != IntPtr.Zero)
            {
                CloseHandle(_hMapFile);
            }
        }

        public Endpoint EndPoint
        {
            get { return _endpoint; }
        }

        public int DataTarget
        {
            get { return GetData().DataTarget; }

            set { SetData(value, "DataTarget"); }
        }

        public int DataDebugger
        {
            get { return GetData().DataDebugger; }

            set { SetData(value, "DataDebugger"); }
        }

        public int ControlTarget
        {
            get { return GetData().ControlTarget; }

            set { SetData(value, "ControlTarget"); }
        }

        public int ControlDebugger
        {
            get { return GetData().ControlDebugger; }

            set { SetData(value, "ControlDebugger"); }
        }

        public EventWaitHandle TargetEvent
        {
            get { return _targetEvent; }
        }

        public EventWaitHandle DebuggerEvent
        {
            get { return _debuggerEvent; }
        }

        private LinkData GetData()
        {
            LinkData data = (LinkData)Marshal.PtrToStructure(_pBuffer, typeof(LinkData));
            return data;
        }

        private void SetData(int value, string fieldName)
        {
            IntPtr offset = Marshal.OffsetOf(typeof(LinkData), fieldName);

            IntPtr bufOffset = new IntPtr(_pBuffer.ToInt64() + offset.ToInt64());
            Marshal.WriteInt32(bufOffset, value);
        }

        private void OpenSharedMemory()
        {
            _endpoint = Endpoint.Debugger;

            //
            // First, we see if someone else has created a shared memory object
            // with the link name we were given.  If not, we create it.
            // otherwise we just open the existing one.
            //
            _hMapFile = OpenFileMapping(FILE_MAP_ALL_ACCESS, false, _linkName);

            if (_hMapFile == IntPtr.Zero)
            {
                _endpoint = Endpoint.Target;

                //
                // No luck opening an existing mapping.
                // Create a new one.
                //
                _hMapFile = CreateFileMapping(_invalidHandle,
                                              IntPtr.Zero,
                                              FileMapProtection.PageReadWrite,
                                              0,
                                              (uint)Marshal.SizeOf(_linkData),
                                             _linkName);

                if (_hMapFile == IntPtr.Zero)
                {
                    //
                    // Something is wrong.
                    //
                    throw new InvalidOperationException(
                                String.Format("Unable to create shared memory for PERQLink!  Error {0:x}",
                                               Marshal.GetLastWin32Error()));
                }
            }

            //
            // At this point we should have a handle to the shared memory, we just need to map a view to it.
            //
            _pBuffer = MapViewOfFile(_hMapFile, FILE_MAP_ALL_ACCESS, 0, 0, (uint)Marshal.SizeOf(_linkData));

            Console.WriteLine("I AM THE {0}, I CAN DO ANYTHING.", _endpoint);
        }

        private void OpenOrCreateEvents()
        {
            try
            {
                _targetEvent = EventWaitHandle.OpenExisting("PERQLinkTargetEvent");
            }
            catch (WaitHandleCannotBeOpenedException e)
            {
                // Event cannot be opened, we are the first process, so create one.
                _targetEvent = new EventWaitHandle(false, EventResetMode.AutoReset, "PERQLinkTargetEvent");
            }

            try
            {
                _debuggerEvent = EventWaitHandle.OpenExisting("PERQLinkDebuggerEvent");
            }
            catch (WaitHandleCannotBeOpenedException e)
            {
                // Event cannot be opened, we are the first process, so create one.
                _debuggerEvent = new EventWaitHandle(false, EventResetMode.AutoReset, "PERQLinkDebuggerEvent");
            }
        }

        private string _linkName;
        private IntPtr _hMapFile;
        private IntPtr _pBuffer;
        private LinkData _linkData;
        private Endpoint _endpoint;
        private EventWaitHandle _targetEvent;
        private EventWaitHandle _debuggerEvent;

        private static IntPtr _invalidHandle = new IntPtr(-1);

        //
        // Win32 specific imports we need to do p/invoke for this magical stuff.
        //
        [DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Auto)]
        private static extern IntPtr OpenFileMapping(uint dwDesiredAccess, bool bInheritHandle, string lpName);

        [DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Auto)]
        private static extern IntPtr CreateFileMapping(IntPtr hFile, IntPtr lpFileMappingAttributes, FileMapProtection flProtect, uint dwMaximumSizeHigh, uint dwMaximumSizeLow, string lpName);

        [DllImport("kernel32.dll", SetLastError = true)]
        private static extern IntPtr MapViewOfFile(IntPtr hFileMappingObject, UInt32 dwDesiredAccess, uint dwFileOffsetHigh, uint dwFileOffsetLow, uint dwNumberOfBytesToMap);

        [DllImport("kernel32.dll", SetLastError = true)]
        private static extern bool UnmapViewOfFile(IntPtr lpBaseAddress);

        [DllImport("kernel32.dll", SetLastError = true)]
        static extern bool CloseHandle(IntPtr hObject);

        //
        // Useful win32 constants
        //
        const UInt32 INVALID_HANDLE_VALUE = 0xffffffff;
        const UInt32 STANDARD_RIGHTS_REQUIRED = 0x000f0000;
        const UInt32 SECTION_QUERY = 0x0001;
        const UInt32 SECTION_MAP_WRITE = 0x0002;
        const UInt32 SECTION_MAP_READ = 0x0004;
        const UInt32 SECTION_MAP_EXECUTE = 0x0008;
        const UInt32 SECTION_EXTEND_SIZE = 0x0010;
        const UInt32 SECTION_ALL_ACCESS = (STANDARD_RIGHTS_REQUIRED | SECTION_QUERY |
            SECTION_MAP_WRITE |
            SECTION_MAP_READ |
            SECTION_MAP_EXECUTE |
            SECTION_EXTEND_SIZE);
        const UInt32 FILE_MAP_ALL_ACCESS = SECTION_ALL_ACCESS;

        [Flags]
        enum FileMapProtection : uint
        {
            PageReadonly = 0x02,
            PageReadWrite = 0x04,
            PageWriteCopy = 0x08,
            PageExecuteRead = 0x20,
            PageExecuteReadWrite = 0x40,
            SectionCommit = 0x8000000,
            SectionImage = 0x1000000,
            SectionNoCache = 0x10000000,
            SectionReserve = 0x4000000,
        }

    }
#endif
}

