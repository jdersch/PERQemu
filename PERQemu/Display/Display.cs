// display.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.HostInterface;

using System;
using System.Windows.Forms;
using System.Drawing;
using System.Drawing.Imaging;

namespace PERQemu.Display
{
    /// <summary>
    /// This implements only the bits necessary to blit a chunk of memory to a window and
    /// do keyboard/mouse input. The actual interrupt/IO/Rendering logic is handled by the
    /// VideoController class, which is responsible for invoking Refresh() when a display
    /// frame is ready.
    ///
    /// Since we only ever want to have one of these it makes sense to have this
    /// be a singleton.
    /// </summary>
    public sealed class Display
    {
        public Display(PERQSystem system)
        {
            _system = system;
            Initialize();
        }

        public void Refresh()
        {
            // Lazy init the display on first refresh.
            if (_display == null)
            {
                StartDisplayThread();

                // Wait for the display thread to finish initialization.
                _initDoneEvent.WaitOne();
            }

            // Might be nice to make this configurable, or add frameskipping, etc...
            if (Environment.ProcessorCount == 1)
            {
                // We force the display to refresh here.  This waits until the display is
                // done blitting before returning.  On single CPU systems, it makes no sense
                // to render on a separate thread, as it offers no improvement.
                SyncRefresh();
                System.Threading.Thread.Sleep(0);
            }
            else
            {
                // If we have more than one CPU at our disposal, we can blit asynchronously.
                // This means the video is slightly out of sync with the CPU, but this is
                // rarely an important issue (and makes a pretty decent perf gain).
                AsyncRefresh();
                System.Threading.Thread.Sleep(0);
            }
        }

        private delegate void RenderDelegate();

        public void SyncRefresh()
        {
            _display.Invoke(new RenderDelegate(SyncRefreshInternal));
        }

        public void AsyncRefresh()
        {
            _display.BeginInvoke(new RenderDelegate(SyncRefreshInternal));
        }

        private void SyncRefreshInternal()
        {
            _dispBox.Refresh();
        }

        public void DrawWord(int displayAddress, ushort word)
        {
            _displayData[displayAddress] = (byte)((word & 0xff00) >> 8);
            _displayData[displayAddress + 1] = (byte)(word & 0xff);
        }

        public void DrawByte(int displayAddress, byte b)
        {
            _displayData[displayAddress] = b;
        }

        public void SaveScreenshot(string path)
        {
            EncoderParameters p = new EncoderParameters(1);
            p.Param[0] = new EncoderParameter(System.Drawing.Imaging.Encoder.Quality, 100L);
            _buffer.Save(path, GetEncoderForFormat(ImageFormat.Jpeg), p);
        }

        public void Shutdown()
        {
            // Check if inited here; prevents a small annoyance if program exited before display
            // inited (FIXME: sometimes still whines about a cross-thread call...)
            if (_display != null)
            {
                _display.Close();
            }
        }

        public int MouseX
        {
            get { return _mouseX; }
        }

        public int MouseY
        {
            get { return _mouseY; }
        }

        public int MouseButton
        {
            get { return _mouseButton; }
        }

        public bool MouseOffTablet
        {
            get { return _mouseOffTablet; }
        }

        private void Initialize()
        {
            // Initialize our event, used to wait for initialization as well as screen refresh.
            _initDoneEvent = new System.Threading.AutoResetEvent(false);

            // Create byte array for display data
            _displayData = new byte[DISPLAY_BUFFER_SIZE];

            // Set up .NET/Mono host keyboard -> PERQ mapping
            _keymap = new KeyboardMap();

            _clickFlag = false;
            _mouseButton = 0x0;
        }

        private void StartDisplayThread()
        {
            _displayThread = new System.Threading.Thread(new System.Threading.ThreadStart(DisplayThread));
            _displayThread.Start();
        }

        private void DisplayThread()
        {
            _display = new Form();
            _display.CreateControl();
            _display.BackColor = Color.Black;
            _display.Text = "PERQ";
            _display.ControlBox = false;
            _display.ClientSize = new Size(VideoController.PERQ_DISPLAYWIDTH, VideoController.PERQ_DISPLAYHEIGHT);
            _display.SizeGripStyle = SizeGripStyle.Hide;
            _display.WindowState = FormWindowState.Normal;
            _display.KeyPreview = true;
            _display.KeyUp += new KeyEventHandler(OnKeyUp);
            _display.KeyDown += new KeyEventHandler(OnKeyDown);
            _display.MouseWheel += new MouseEventHandler(OnMouseWheel);

            _buffer = new Bitmap(VideoController.PERQ_DISPLAYWIDTH,
                                 VideoController.PERQ_DISPLAYHEIGHT,
                                 PixelFormat.Format1bppIndexed);

            _dispBox = new PictureBox();
            _dispBox.Image = _buffer;
            _dispBox.Size = new Size(VideoController.PERQ_DISPLAYWIDTH, VideoController.PERQ_DISPLAYHEIGHT);
            _dispBox.Cursor = Cursors.Cross;
            _dispBox.Paint += new PaintEventHandler(OnPaint);
            _dispBox.MouseDown += new MouseEventHandler(OnMouseDown);
            _dispBox.MouseUp += new MouseEventHandler(OnMouseUp);
            _dispBox.MouseMove += new MouseEventHandler(OnMouseMove);

            _display.Controls.Add(_dispBox);

            _displayRect = new Rectangle(0, 0, VideoController.PERQ_DISPLAYWIDTH, VideoController.PERQ_DISPLAYHEIGHT);

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.EmuState, "Display thread started, display initialized.");
#endif

            _display.Shown += new EventHandler(OnDisplayShown);

            // And show the window on this thread.
            _display.ShowDialog();
        }

        void OnDisplayShown(object sender, EventArgs e)
        {
            // Signal that we're done initializing the dialog.
            _initDoneEvent.Set();
        }

        void OnMouseWheel(object sender, MouseEventArgs e)
        {
            _clickFlag = e.Delta > 0;

            if (_clickFlag)
            {
                _dispBox.Top = _display.ClientRectangle.Height - VideoController.PERQ_DISPLAYHEIGHT;
            }
            else
            {
                _dispBox.Top = 0;
            }
        }

        void OnMouseMove(object sender, MouseEventArgs e)
        {
            _mouseX = e.X;
            _mouseY = e.Y;
        }

        /// <summary>
        /// Map the host mouse buttons to the Kriz tablet (passed straight through).
        /// The GPIB BitPad does its own mapping, since the four-button puck has a
        /// slightly strange layout.  Here's the chart:
        ///
        ///     host        Kriz        BitPad
        /// 0x8 XButton1    n/a         0x4 blue    or: alt+right
        /// 0x4 Right       0x4 right   0x8 green
        /// 0x2 Middle      0x2 middle  0x1 yellow  or: alt+left
        /// 0x1 Left        0x1 left    0x2 white
        /// 
        /// If we emulated the 1-button stylus or the 16-button mega puck we'd have
        /// to monkey with the mappings, but this is complicated enough...
        /// </summary>
        void OnMouseDown(object sender, MouseEventArgs e)
        {
            switch (e.Button)
            {
                case MouseButtons.XButton1:
                    _mouseButton = 0x8;
                    break;

                case MouseButtons.Right:
                    _mouseButton = _mouseAltButton ? 0x8 : 0x4;
                    break;

                case MouseButtons.Middle:
                    _mouseButton = 0x2;
                    break;

                case MouseButtons.Left:
                    _mouseButton = _mouseAltButton ? 0x2 : 0x1;
                    break;
            }
        }

        void OnMouseUp(object sender, MouseEventArgs e)
        {
            _mouseButton = 0x0;
        }

        /// <summary>
        /// Handles keyboard input from the Host side, handling "special" keys locally.
        /// Key translation is then done, and applicable results are queued on the Z80
        /// keyboard input buffer.
        /// </summary>
        void OnKeyDown(object sender, KeyEventArgs e)
        {
            byte perqCode = 0;

            //
            // Handle any keys that may affect the Form itself, and are not passed
            // to the PERQ.
            //
            switch (e.KeyCode)
            {
                // Allow Home/PageUp and End/PageDown keys to scroll the display.
                // Useful on laptop touchpads which don't simulate (or mice that
                // don't have) scroll wheels.
                case Keys.Home:
                case Keys.PageUp:
                    _dispBox.Top = 0;
                    e.Handled = true;
                    break;

                case Keys.End:
                case Keys.PageDown:
                    _dispBox.Top = _display.ClientRectangle.Height - VideoController.PERQ_DISPLAYHEIGHT;
                    e.Handled = true;
                    break;

                // Catch the PrintScreen button and initiate a snapshot of the PERQ screen.
                // Would be cool if this could pause the emulator, snap the window, then offer
                // a standard save dialog to store the file...
                case Keys.PrintScreen:
                    Console.WriteLine("PrintScreen called - not yet implemented");
                    break;

                // Toggle the "lock" keys... this needs work.
                case Keys.CapsLock:
                case Keys.NumLock:
                case Keys.Scroll:
                    _keymap.setLockKeyState(e.KeyCode);
                    e.Handled = true;
                    break;

                // Quirks: On Windows, the Control, Shift and Alt keys repeat when held down even
                // briefly.  The PERQ never needs to receive a plain modifier key event like that;
                // it's just a lot of noise, so skip the mapping step and quietly handle them here
                // (though they are still checked below for mouse options).
                case Keys.ShiftKey:
                case Keys.ControlKey:
                    #region GoryDetails
                    //
                    // ** Ugly Hack Alert **
                    // Oh, but it gets better: on the very broken Mac WinForms port, control
                    // characters in KeyDown events don't have the KeyCode of the key pressed -
                    // they're mapped for some insanely stupid reason to 1..26.  (From the Mono
                    // source this appears to be deliberate.  Seriously.)
                    //
                    // That means pressing the Shift or Control keys *by themselves* would get
                    // passed to the PERQ as valid control characters [bonks head on desk].  It
                    // _appears_ that the way to distinguish between a Ctrl-<char> sequence and
                    // a plain ControlKey (or ShiftKey) event is that the KeyValue does not equal
                    // the KeyCode (enum) value.  getKeyMapping() attempts to work that mess out;
                    // here we just strip out the unadorned events.
                    //
                    // Nope, that was hopeless.  Rewrote the Mac Mono keyboard input routine.
                    // Will include the patch with the distribution for any Mac users out there...
                    //
                    #endregion
                    if (e.KeyValue == (int)e.KeyCode)
                    {
                        e.Handled = true;   // Extraneous Shift- or Control-key event - ignore
                    }
                    else
                    {
                        e.Handled = false;  // Mac hack - this is probably a real Ctrl- or Ctrl-Shift- char.
                    }
                    break;

                case Keys.Alt:
                case Keys.Menu:
                    // Since the PERQ doesn't _have_ an "Alt" key, just ignore these entirely?
                    e.Handled = true;
                    break;

                case Keys.Pause:            // Windows keyboards
                case Keys.F8:               // Create a Mac equivalent...
                    // Provide a key to jump into the debugger when focus is on the PERQ,
                    // rather than select the console and hit ^C.
                    e.Handled = true;
                    _system.Break();
                    break;

                default:
                    e.Handled = false;
                    break;
            }

            // If the key wasn't handled above, let's see if we can get the ASCII equivalent.
            if (!e.Handled)
            {
                perqCode = _keymap.getKeyMapping(e);
                if (perqCode != 0)
                {
                    _system.IOB.Z80System.Keyboard.QueueInput(perqCode);   // Ship it!
                    e.Handled = true;
                }
            }

            if (e.Handled)
            {
                // Prevent this from being handled by OnKeyPress.
                e.SuppressKeyPress = true;
            }

            //
            // The following allow special modifiers that make Kriz tablet / BitPadOne tablet
            // manipulation using a standard PC mouse easier:
            //  - If Alt is held down, the Kriz tablet is put into "puck off tablet" mode
            //    which allows relative mode to work better (though it's still pretty clumsy)
            //  - If Ctrl is held down, The Left mouse button simulates Kriz/GPIB middle button
            //    and the Right mouse button simulates GPIB button 4 (blue; n/a on Kriz)
            //
            if (e.Alt)
            {
                _mouseOffTablet = true;
            }

            if (e.Control)
            {
                _mouseAltButton = true;
            }
        }

        /// <summary>
        /// Only used to handle the mouse button hacks.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        void OnKeyUp(object sender, KeyEventArgs e)
        {
            // Reset mouse tweaks if modifier keys are released
            if (!e.Alt)
            {
                _mouseOffTablet = false;
            }

            if (!e.Control)
            {
                _mouseAltButton = false;
            }
        }

        private void OnPaint(object sender, PaintEventArgs e)
        {
            BitmapData data = _buffer.LockBits(_displayRect, ImageLockMode.WriteOnly, PixelFormat.Format1bppIndexed);

            IntPtr ptr = data.Scan0;
            System.Runtime.InteropServices.Marshal.Copy(_displayData, 0, ptr, _displayData.Length);

            _buffer.UnlockBits(data);
        }

        private ImageCodecInfo GetEncoderForFormat(ImageFormat format)
        {
            ImageCodecInfo[] codecs = ImageCodecInfo.GetImageDecoders();

            foreach (ImageCodecInfo codec in codecs)
            {
                if (codec.FormatID == format.Guid)
                {
                    return codec;
                }
            }
            return null;
        }


        private int DISPLAY_BUFFER_SIZE = (VideoController.PERQ_DISPLAYHEIGHT *
                                           VideoController.PERQ_DISPLAYWIDTH_IN_BYTES);

        // Bitmap data (from the PERQ's memory buffer)
        private byte[] _displayData;

        // Display
        private Form _display;
        private Bitmap _buffer;
        private PictureBox _dispBox;
        private Rectangle _displayRect;
        private System.Threading.Thread _displayThread;
        private System.Threading.AutoResetEvent _initDoneEvent;

        // Mouse
        private int _mouseX;
        private int _mouseY;
        private int _mouseButton;
        private bool _clickFlag;

        // Mouse tweaks
        private bool _mouseOffTablet;
        private bool _mouseAltButton;

        // Keyboard map (.NET/Mono -> PERQ)
        private KeyboardMap _keymap;

        private PERQSystem _system;
    }
}

