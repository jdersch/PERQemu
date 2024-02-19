//
// IStorageController.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
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

using PERQmedia;

namespace PERQemu.IO
{
    public interface IStorageController
    {
        /// <summary>
        /// Perform a "hard" Reset this instance.
        /// </summary>
        void Reset();

        /// <summary>
        /// Attach a drive to the controller.
        /// </summary>
        void AttachDrive(uint unit, StorageDevice dev);

        /// <summary>
        /// Reads controller status as an integer.
        /// </summary>
        int ReadStatus();

        /// <summary>
        /// Loads a register.
        /// </summary>
        void LoadRegister(byte address, int value);

        /// <summary>
        /// Initiate a seek operation.
        /// </summary>
        /// <remarks>
        /// Useful for the Z80-controlled IOB/CIO seek hardware; not sure yet
        /// how the EIO manages that, but assume it's built-in to the 2910-
        /// based disk state machine firmware?
        /// </remarks>
        void DoSingleSeek();
    }
}
