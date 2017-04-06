// win32interop.cs - Copyright 2006-2013 Josh Dersch (derschjo@gmail.com)
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
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace PERQemu.PlatformSpecific
{
    public class Win32KeyboardInterop : IKeyboardInterop
    {
        public byte GetAsciiCodeForKey(Keys key, bool shifted)
        {
            byte value = 0;

            byte[] lpKeyState = new byte[256];
            if (GetKeyboardState(lpKeyState) != 0)
            {
                //
                // We do not want to get the translated code for Ctrl keys, so we
                // unset them in the returned key state
                //
                lpKeyState[(uint)Keys.LControlKey] = 0;
                lpKeyState[(uint)Keys.RControlKey] = 0;
                //lpKeyState[(uint)Keys.Control] = 0;
                lpKeyState[(uint)Keys.ControlKey] = 0;
                byte[] lpChar = new byte[2];
                if (ToAscii((uint)key, 0, lpKeyState, lpChar, 0) == 1)
                {
                    value = lpChar[0];
                }
            }
            
            return value;
        }        

        /// <summary>
        /// ToAscii is the Win32 API used to map a given virtual key + modifiers to an ASCII
        /// code.
        /// </summary>
        /// <param name="uVirtKey"></param>
        /// <param name="uScanCode"></param>
        /// <param name="lpKeyState"></param>
        /// <param name="lpChar"></param>
        /// <param name="uFlags"></param>
        /// <returns></returns>
        [DllImport("user32.dll")]
        static extern int ToAscii(uint uVirtKey, uint uScanCode, byte[] lpKeyState,
           [Out] byte[] lpChar, uint uFlags);

        [DllImportAttribute("User32.dll")]
        public static extern int GetKeyboardState(byte[] pbKeyState);
    }
}
