//
// Page.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.UI.Output
{
    /// <summary>
    /// An overachieving rectangle.  Because I'm lazy, and don't really want to
    /// include SDL2 or System.Drawing here.  So sue me.
    /// </summary>
    public struct Region
    {
        public Region(uint x, uint y, uint w, uint h)
        {
            X = x;
            Y = y;
            W = w;
            H = h;
        }

        public override string ToString()
        {
            return $"[Region: X,Y={X},{Y} W,H={W},{H} Scan={ScanWidth}, Bytes={Bytes}]";
        }

        public uint X;
        public uint Y;
        public uint W;
        public uint H;

        public uint ScanWidth => (W + 7) / 8;
        public uint Bytes => ScanWidth * H;
    }

    /// <summary>
    /// One printed page in bitmap form.  Used to capture screenshots or simulated
    /// printer output that can be saved as single images (PNG or TIFF) or multi-
    /// image files (TIFF).
    /// </summary>
    /// <remarks>
    /// Bitmaps are assumed 1bpp, 8 packed pixels per byte.  If you want a screen
    /// capture with window borders in color, just use the system/host utility!
    /// 
    /// For now, assume square pixels.  No reason I couldn't extend this to allow
    /// different horizontal and vertical dot pitch for something like a dot matrix
    /// printer, if necessary.
    /// </remarks>
    public class Page
    {
        public Page(uint dpi, Region geom, ushort pagenum = 1)
        {
            _resolution = dpi;
            _dstRect = geom;
            PageNumber = pagenum;

            Log.Debug(Category.Formatter, "New Page: {0}", _dstRect.ToString());

            // Compute the scan width and create the empty bitmap
            _bitmap = new byte[_dstRect.Bytes];
        }

        public ushort PageNumber;

        public uint Resolution => _resolution;
        public byte BitDepth => 1;
        public uint BitWidth => _dstRect.W;
        public uint BitHeight => _dstRect.H;
        public uint ScanWidth => _dstRect.ScanWidth;
        public byte[] Buffer => _bitmap;

        /// <summary>
        /// Copy the PERQ bits for output, inverting and clipping if necessary.
        /// </summary>
        /// <remarks>
        /// The Canon driver renders each page into a large, fixed size byte array
        /// since it doesn't know in advance (or even from one page to the next)
        /// what the PERQ is going to send.  Here we left/top justify the active
        /// region / printable area to eliminate the large surrounding border (or
        /// crop it to fit the selected paper size).  This is expensive and silly
        /// since it involves a bytewise copy in managed code, but it ain't like
        /// printing in the real world isn't mechanical and slow... :-)
        /// </remarks>
        public void CopyBits(byte[] source, Region srcRect, Region clipRect)
        {
            // Debug
            if ((clipRect.Bytes) > (srcRect.Bytes))
            {
                Log.Error(Category.Formatter, "Clipped area bigger than source!");
                return;
            }

            if ((clipRect.ScanWidth > _dstRect.ScanWidth) || (clipRect.W > _dstRect.W) || (clipRect.H > _dstRect.H))
            {
                Log.Error(Category.Formatter, "Bad clipping dimensions: {0}", clipRect);
                return;
            }

            if (_dstRect.Bytes != _bitmap.Length)
            {
                Log.Warn(Category.Formatter, "Byte count mismatch (Region={0}, Buffer={1})", _dstRect.Bytes, _bitmap.Length);
            }

            // Canon computes the X,Y for the page area (_dstRect) when the paper
            // size and printable area are computed.  Convert to a byte offset
            var offset = _dstRect.X / 8;

            // Starting source address
            var srcAddr = clipRect.Y * srcRect.ScanWidth + offset;
            var dstAddr = _dstRect.Y * _dstRect.ScanWidth + offset;

            Log.Debug(Category.Formatter, "CopyBits:  X Offset={0}, srcAddr={1}, dstAddr={2}", offset, srcAddr, dstAddr);
            Log.Debug(Category.Formatter, "Src Rect:  {0}", srcRect.ToString());
            Log.Debug(Category.Formatter, "ClipRect:  {0}", clipRect.ToString());

            // S l o w... copy the rectangle and invert the bytes, since PERQ
            // memory images are the opposite of what PNG/TIFF expect. :-|

            // Init the background; we can't be sure how much the PERQ actually
            // filled in, so zap it all to white.  Wither memset, C#?
            for (var i = 0; i < _bitmap.Length; i++)
                _bitmap[i] = 0xff;

            for (var y = 0; y < clipRect.H; y++)
            {
                for (var x = 0; x < clipRect.ScanWidth; x++)
                {
                    _bitmap[dstAddr + x] = (byte)~source[srcAddr + x];
                }

                dstAddr += _dstRect.ScanWidth;
                srcAddr += srcRect.ScanWidth;
            }
        }

        /// <summary>
        /// Copy the bitmap directly, with no clipping.
        /// </summary>
        public void CopyBits(byte[] source)
        {
            source.CopyTo(_bitmap, 0);
        }

        uint _resolution;
        Region _dstRect;
        byte[] _bitmap;
    }
}

/*
    Notes:
    
    For Canon, _maxArea is the full-size buffer (9" x 11", or x 14" if/when Legal
    size is supported); Page is created with the actual paper size depending on
    which cassette is loaded (so, max 8.5" x 14") while the clipping rect is the
    printable area divined from any number of magical/BS sources.  So:

    Clipping with CopyBits (example, using US Letter):
        
            SrcRect             Page                Printable
        W   9"  (2704 / 338)    8.5" (2550 / 319)   8.19" (2457 / 308)
        H   12" (3600)          11" (3300)          10.86" (3258)

    X is (2550 - 2457) / 2 = floor(46.5 / 8)  ->  floor((319 - 308) / 2) = 5

    So far other paper types haven't been tested, since there's no PERQ support
    for changing it.  I should see if A4 pages look okay, since non-US users may
    prefer that.

    Saving to the actual paper dimensions is great for presenting the full size
    page on a display, but subsequently printing that means clipping or scaling
    by around 96% to fit a typical modern printer.  HMM.  Make that an option?
    "settings canon clip (to printable area) true?"

    Screenshots are just direct saves of the display dimensions - no paper size
    or printable area concerns.

 */