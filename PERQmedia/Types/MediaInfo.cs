//
//  MediaInfo.cs
//
//  Author:  S. Boondoggle <skeezicsb@gmail.com>
//
//  Copyright (c) 2022, Boondoggle Heavy Industries, Ltd.
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

using System;
using System.IO;
using System.Text;
using System.Drawing;
using System.Drawing.Imaging;

namespace PERQmedia
{
    /// <summary>
    /// Formatter codes set when an older format file is loaded, in case the
    /// user wants to write the image back out without conversion.
    /// </summary>
    public enum Formatters
    {
        Unknown = 0,
        RawFormat = 1,
        PHDFormat = 2,
        IMDFormat = 3,
        TAPFormat = 4,
        PRQFormat = 42
    }

    /// <summary>
    /// Filesystem hint.  These are purely informational, and can be set by
    /// PERQdisk, PERQFloppy or Stut to help organize a PERQ media library.
    /// </summary>
    public enum FilesystemHint : byte
    {
        Unknown = 0,
        POS = 1,        // Vanilla POS
        POSG85 = 3,     // Experimental POS 32-bit? (rare, undocumented)
        Accent = 6,     // POS extended with part-relative addressing?
        PNX = 7,        // Unix V7ish but no magic number...
        PNXBoot = 8,    // POS boot partition + PNX root hybrid!
        RT11 = 11,      // Floppy floppies
        RT11Ext = 12,   // SuperFloppy (RT-11 with extra sauce)
        Stut = 20,      // POS-ish tape dump format
        Tar = 21        // Primitive tar-like format (Unix compatible?)
    }

    /// <summary>
    /// Returns extra goodies from the media file, if present.  These aren't
    /// strictly required for the emulator to use the data for the disk or
    /// tape image, but aid the user in organizing their media files.  The
    /// emulator's GUI might offer the label/image data to help identify the
    /// image to load, for instance.
    /// </summary>
    public class MediaInfo
    {
        public MediaInfo()
        {
            Version = 0;
            Format = Formatters.Unknown;
            FSType = FilesystemHint.Unknown;
#if DEBUG
            ArchiveDate = DateTime.Today;   // don't change every damn run when testing
#else
            ArchiveDate = DateTime.UtcNow;
#endif
            ArchivedBy = Environment.UserName;
            _textLabel = null;
            _imageLabel = null;
        }

        public MediaInfo(byte vers, Formatters form, FilesystemHint hint,
                         string user, DateTime date, byte[] text, byte[] image)
        {
            Version = vers;
            Format = form;
            FSType = hint;
            ArchivedBy = user;
            ArchiveDate = date;
            _textLabel = text;
            _imageLabel = image;
        }

        public byte Version;
        public Formatters Format;
        public FilesystemHint FSType;
        public DateTime ArchiveDate;
        public string ArchivedBy;

        public byte[] TextLabel
        {
            get { return _textLabel; }
            set { _textLabel = value; }
        }

        public byte[] ImageLabel
        {
            get { return _imageLabel; }
            set { _imageLabel = value; }
        }

        /// <summary>
        /// Encode the Text Label from a string as an array of bytes using
        /// the default UTF8 encoding.
        /// </summary>
        public void EncodeTextLabel(string label)
        {
            if (!string.IsNullOrEmpty(label))
            {
                _textLabel = Encoding.UTF8.GetBytes(label);
            }
            else
            {
                _textLabel = null;
            }
        }

        /// <summary>
        ///  Get the UTF8-encoded Text Label back as a string.
        /// </summary>
        public string DecodeTextLabel()
        {
            if (_textLabel?.Length > 0)
            {
                return Encoding.UTF8.GetString(_textLabel);
            }

            return string.Empty;
        }

        /// <summary>
        /// Reconstitutes the ImageLabel from a byte array.
        /// </summary>
        /// <remarks>
        /// How do it know?  I guess we'll suck it and see!
        /// </remarks>
        public Image DecodeImageLabel()
        {
            if (_imageLabel?.Length > 0)
            {
                using (MemoryStream ms = new MemoryStream(_imageLabel))
                {
                    return Image.FromStream(ms);
                }
            }

            return null;
        }

        /// <summary>
        /// Convert and store an ImageLabel in PNG format (default).
        /// </summary>
        public void EncodeImageLabel(Image img)
        {
            EncodeImageLabel(img, ImageFormat.Png);
        }

        /// <summary>
        /// Sets the ImageLabel based on a file extension.
        /// </summary>
        public void SetImageLabel(Image img, string extension)
        {
            switch (extension)
            {
                case ".jpeg":
                case ".jpg":
                    EncodeImageLabel(img, ImageFormat.Jpeg);
                    break;
                case ".png":
                    EncodeImageLabel(img, ImageFormat.Png);
                    break;
                case ".tiff":
                    EncodeImageLabel(img, ImageFormat.Tiff);
                    break;
                case ".gif":
                    EncodeImageLabel(img, ImageFormat.Gif);
                    break;
            }
        }

        /// <summary>
        /// Sets the ImageLabel with a specified format.
        /// </summary>
        public void EncodeImageLabel(Image img, ImageFormat fmt)
        {
            _imageLabel = ConvertImageToByteArray(img, fmt);
        }

        /// <summary>
        /// Converts the image to byte array.
        /// </summary>
        private byte[] ConvertImageToByteArray(Image image, ImageFormat fmt)
        {
            using (var memoryStream = new MemoryStream())
            {
                image.Save(memoryStream, fmt);
                return memoryStream.ToArray();
            }
        }

        // Storage for the "raw" label data
        private byte[] _textLabel;
        private byte[] _imageLabel;
    }
}
/*

using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;

public sealed class SignatureDetector
{
   public static readonly SignatureDetector Png =
	   new SignatureDetector(0x89, 0x50, 0x4e, 0x47);

   public static readonly SignatureDetector Bmp =
	   new SignatureDetector(0x42, 0x4d);

   public static readonly SignatureDetector Gif =
	   new SignatureDetector(0x47, 0x49, 0x46);

   public static readonly SignatureDetector Jpeg =
	   new SignatureDetector(0xff, 0xd8);

   public static readonly IEnumerable<SignatureDetector> Images =
	   new ReadOnlyCollection<SignatureDetector>(new[] { Png, Bmp, Gif, Jpeg });

   private readonly byte[] bytes;

   public SignatureDetector(params byte[] bytes)
   {
	   if (bytes == null)
	   {
		   throw new ArgumentNullException("bytes");
	   }
	   this.bytes = (byte[])bytes.Clone();
   }

   public bool Matches(byte[] data)
   {
	   if (data == null)
	   {
		   throw new ArgumentNullException("data");
	   }
	   if (data.Length < bytes.Length)
	   {
		   return false;
	   }
	   for (int i = 0; i < bytes.Length; i++)
	   {
		   if (data[i] != bytes[i])
		   {
			   return false;
		   }
	   }
	   return true;
   }

   // Convenience method
   public static bool IsImage(byte[] data)
   {
	   return Images.Any(detector => detector.Matches(data));      
}
 */