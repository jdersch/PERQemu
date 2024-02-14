//
// CanonFormatter.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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

using System;
using System.IO;

using PERQemu.UI.Output;

namespace PERQemu.IO
{
    /// <summary>
    /// Output file formatters for Canon laser printer output.  These directly
    /// transform a PERQ bitmap in the CanonPrinter's page buffer to a minimal
    /// monochrome image file.
    /// </summary>
    public partial class CanonPrinter
    {
        /// <summary>
        /// Save the page as a PNG image.  One per output file, default compression.
        /// </summary>
        bool SaveAsPNG(string filename)
        {
            // Some informative strings
            string[] keys = { "Title", "Creation Time", "Software" };
            string[] values = { $"PERQ Canon {Model} printer output",
                                DateTime.Now.ToString("yyyy-MM-dd'T'HH:mm:ss.ffK"),
                                PERQemu.Version };

            // Pass 'em to the formatter
            var png = new PNGFormatter(keys, values);

            // Build the output path
            var fullpath = Paths.BuildOutputPath(filename);

            // Update the page for the current paper size and invert
            var page = new Page(_resolution, _pageArea);
            page.CopyBits(_pageBuffer, _maxArea, _printableArea);

            try
            {
                using (var fs = new FileStream(fullpath, FileMode.Create, FileAccess.Write))
                {
                    png.Save(page, fs);
                }

                // Made it!
                return true;
            }
            catch (Exception e)
            {
                Log.Write(Category.Formatter, "Failed to save output: {0}", e.Message);
                return false;
            }
        }

        /// <summary>
        /// Save the page list as an uncompressed, 1bpp bi-level TIFF.
        /// </summary>
        bool SaveAsTIFF(string filename)
        {
            // Set our title and create the formatter
            var tiff = new TIFFFormatter($"PERQ Canon {Model} printer output");

            // Build the file path and let 'er rip
            var fullpath = Paths.BuildOutputPath(filename);

            try
            {
                using (var fs = new FileStream(fullpath, FileMode.Create, FileAccess.Write))
                {
                    tiff.Save(_pageList, fs);
                    fs.Close();
                    return true;
                }
            }
            catch (Exception e)
            {
                Log.Write(Category.Formatter, "Failed to save output: {0}", e.Message);
                return false;
            }
        }

        /// <summary>
        /// Save the page buffer as raw pixel data, full resolution.  Limited 
        /// utility as a debugging aid; can view results online with a tool like
        /// https://rawpixels.net or other utilities.  May be removed.
        /// </summary>
        bool SaveAsRaw(string filename)
        {
            var fullpath = Paths.BuildOutputPath(filename);

            using (var fs = new FileStream(fullpath, FileMode.Create, FileAccess.Write))
            {
                fs.Write(_pageBuffer, 0, _pageBuffer.Length);
                fs.Close();
            }
            return true;
        }
    }
}
