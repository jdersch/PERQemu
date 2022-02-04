//
// CPUBoard.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
using System.Runtime.CompilerServices;

using PERQemu.Config;

namespace PERQemu.Processor
{
    /// <summary>
    /// The CPU Board contains the central processor and scheduler.  These
    /// always execute in lock step, in either synchronous or asynchronous
    /// mode.  The processor is also responsible for clocking the memory.
    /// </summary>
    public sealed class CPUBoard
    {
        public CPUBoard(PERQSystem sys)
        {
            _system = sys;

            // Instantiate our CPU
            switch (sys.Config.CPU)
            {
                case CPUType.PERQ1:
                    _processor = new PERQ1(sys);
                    break;

                case CPUType.PERQ1A:
                    _processor = new PERQ1A(sys);
                    break;

                case CPUType.PERQ24:
                    _processor = new PERQ24(sys);
                    break;

                case CPUType.PERQ24A:
                    throw new UnimplementedHardwareException("Sorry, PERQ24A CPU is not implemented");

                default:
                    throw new InvalidConfigurationException($"No such CPU board type '{sys.Config.CPU}'");
            }

            // Create the system scheduler
            _scheduler = new Scheduler(CPU.MicroCycleTime);

            // Rate limiter
            _heartbeat = new SystemTimer(20f);

            // Compute how often (in CPU cycles) to sync the emulated processor
            _adjustInterval = (int)(_heartbeat.Interval / (CPU.MicroCycleTime * Conversion.NsecToMsec));
            Log.Info(Category.Emulator, "[CPU rate adjust every {0} cycles]", _adjustInterval);
        }

        public CPU Processor => _processor;
        public Scheduler Scheduler => _scheduler;

        public bool SupportsAsync => true;

        public void Reset()
        {
            _heartbeat.Reset();
            _scheduler.Reset();
            _processor.Reset();

            Log.Debug(Category.CPU, "Board reset");
            //_scheduler.DumpEvents("CPU");
        }

        /// <summary>
        /// Run the CPU and Scheduler (synchronous mode).
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Run(int clocks = 1)
        {
            do
            {
                _processor.Execute();
                _scheduler.Clock();
                clocks--;
            }
            while (clocks > 0);
        }

        /// <summary>
        /// Start the thread for the CPU and Scheduler (asynch mode).
        /// </summary>
        public void RunAsync()
        {
            if (_asyncThread != null)
            {
                throw new InvalidOperationException("CPU thread is already running; Stop first.");
            }

            // Fire off the CPU thread
            _stopAsyncThread = false;
            _asyncThread = new Thread(AsyncThread) { Name = "CPU" };
            _asyncThread.Start();
        }

        /// <summary>
        /// The thread proc for asynchronous CPU and Scheduler execution.
        /// </summary>
        private void AsyncThread()
        {
            // Catch events from the controller
            PERQemu.Controller.RunStateChanged += OnRunStateChange;

            _heartbeat.Enable(true);
            Console.WriteLine("[CPU thread starting]");

            do
            {
                //try
                //{
                    Run(_adjustInterval);

                    if (_stopAsyncThread) break;

                    // Do rate limiting if configured (and we haven't left run mode)
                    if (Settings.Performance.HasFlag(RateLimit.AccurateCPUSpeedEmulation))
                    {
                        _heartbeat.WaitForHeartbeat();
                    }
                //}
                //catch (Exception e)
                //{
                //    _system.Halt(e);
                //}
            }
            while (!_stopAsyncThread);

            _heartbeat.Enable(false);
            Console.WriteLine("[CPU thread stopped]");

            // Detach
            PERQemu.Controller.RunStateChanged -= OnRunStateChange;
        }

        /// <summary>
        /// Stop the background thread.
        /// </summary>
        public void Stop()
        {
            if (_asyncThread == null)
            {
                Console.WriteLine("[CPU Stop called on null thread]");
                return;
            }

            Console.WriteLine("[Stop() CPU thread]");
            _stopAsyncThread = true;

            if (Thread.CurrentThread != _asyncThread)
            {
                Console.WriteLine("[CPU thread join called on {0}...]", Thread.CurrentThread.ManagedThreadId);
                // Waaaaait for it
                //_asyncThread.Join();
                _asyncThread = null;
                Console.WriteLine("[CPU thread exited]");
            }
        }

        private void OnRunStateChange(RunStateChangeEventArgs s)
        {
            Console.WriteLine("[CPU state change event -> {0}]", s.State);
            _stopAsyncThread = (s.State != RunState.Running);
        }

        /// <summary>
        /// Load the Boot ROM image appropriate for this CPU and I/O Board type.
        /// </summary>
        public void LoadBootROM(string file)
        {
            try
            {
                _processor.LoadROM(Paths.BuildPROMPath(file));
            }
            catch
            {
                Log.Error(Category.CPU, "Could not load boot ROM from {0}!", file);
                throw;
            }
        }

        private CPU _processor;
        private Scheduler _scheduler;
        private PERQSystem _system;

        private SystemTimer _heartbeat;
        private int _adjustInterval;

        private Thread _asyncThread;
        private volatile bool _stopAsyncThread;
    }
}
