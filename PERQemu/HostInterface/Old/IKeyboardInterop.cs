// ikeyboardinterop.cs - Copyright 2006-2013 Josh Dersch (derschjo@gmail.com)
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
using System.Windows.Forms;

namespace PERQemu.PlatformSpecific
{
    public interface IKeyboardInterop
    {
        /// <summary>
        /// Maps a given keyboard key + shift modifier to an ASCII value.  This is used to translate
        /// host system keystrokes to ASCII values in order to simulate PERQ keyboard input.
        /// 
        /// The implementation is system-specific (there is no standard .NET API to do this).
        /// On Windows we need to P/Invoke to a user32.dll API, on Mono... I'm not sure yet.
        /// 
        /// Note this only needs to support the "shift" modifier, since the translation for
        /// Ctrl modifiers does not depend on the keyboard layout (for the PERQ it's always done
        /// by setting bit 7 on the shifted or unshifted ASCII value).
        /// 
        /// We expose this as an interface so that the P/Invoke methods can be selected at runtime
        /// on a per-platform basis (so we can still have one binary for both Windows and Mono).
        /// </summary>
        /// <param name="key"></param>
        /// <param name="modifiers"></param>
        /// <returns></returns>
        byte GetAsciiCodeForKey(Keys key, bool shifted);
    }
}
