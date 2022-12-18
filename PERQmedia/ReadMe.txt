
The PERQmedia Library
=====================

Overview
--------

PERQmedia defines a common storage format for all virtual PERQ media.  It
provides one StorageDevice class that presents a simple block-oriented API
for clients to read and write data to an in-memory copy of a disk, floppy or
tape image.  Methods are provided to load and save images in a number of
on-disk formats.

These clients can/will use the PERQmedia library and file format:

    PERQemu     - the PERQ emulator
    PERQdisk    - a program to browse and retrieve data from POS filesystems
                  or read and write RT-11 format floppies
    Stut        - a simple file extractor for PERQ Stut tape images


PERQemu v0.4.6 and beyond incorporate the PERQmedia library for all I/O to the
supported floppy and hard disk formats.  PERQemu v0.4.8 adds support for the
streamer tape and TAP format.

A beta release of PERQdisk (v0.9.0) was released which combines the POS and
RT-11 (floppy) functionality into one program!

With the addition of streaming tape support to PERQemu, an updated version of
the standalone Stut utility (named for its counterpart in POS and Accent) will
be updated to use the PERQmedia library, giving access to tape images saved in
both TAP and PRQM formats!  [Release date TBD]


File Format
-----------

The new PERQmedia native file format is essentially a thin wrapper around the
StorageDevice class.  All PERQ disk devices use the basic Cylinder/Head/Sector
physical addressing scheme; StorageDevice provides the same mechanism, but
leaves the specifics of device status and control functions to clients.

All of the PERQ storage formats previously used by PERQemu have enough 
distinguishing characteristics that things like filename extension don't have
to be relied upon.  A series of media formatters are defined that translate
to and from the new common format.  If the filename has a recognized extension,
the Formatter associated with that file type will be tried first, otherwise the
Loader will try each Formatter in turn to try to properly identify the media:

    PRQFormatter - ".prqm"
        The new full-featured PERQmedia format, appropriate for all PERQemu
        drive classes.  The file format is detailed in PRQM_Format.txt.

    PHDFormatter - ".phd"
        The original PERQemu Hard Disk format, produced by the "dumpdisk"
        program.  Only used for Shugart hard disk images, though it isn't
        necessarily limited to that geometry.

    IMDFormatter - ".imd"
        Reads and writes floppy images in the popular IMD format, v1.18
        (latest as of Jan-2022).

    RawFormatter - ".raw", ".pfd"
        Reads and writes old PERQemu "raw" floppies, converted from the DMK or
        IMD formats to "raw" images.  The "PFD" format was an aborted attempt
        to sneak an identifiable "cookie" and some hints useful to PERQemu into
        the raw format; it was never fully implemented in the emulator.  Because
        sector 0/0/0 is not used by any known PERQ OS or data format, the PFD
        cookie can be written there without requiring any changes to the loader
        or affecting operation of the emulator in any way.  If PERQmedia loads
        a raw floppy and sees the PFD header it will set the hints it provides;
        if writing a raw floppy the PFD cookie will always be set (for older
        client programs that use it).

    TAPFormatter - ".tap"
        Reads and writes magtape images used by SIMH.  The TAP format is quite
        minimalistic, with no metadata about the contents stored in the file in
        the "standard" format.  However, PERQmedia takes advantage of "extended"
        (class E) markers designated for the purpose of storing metadata to save
        all of the same details, including labels, as the native PRQM format!
        This formatter initially supports importing images with fixed 512-byte
        blocks, which seems to be the only size used by the Archive Sidewinder
        and the PERQ software.
        

The precise layout of a PERQmedia file is described in PRQM_Format.txt.


StorageDevice Class
-------------------

A basic StorageDevice provides only a "physical" view of the device's data
blocks.  It does not impose any other structure or interpretation, though it
does provide storage for an optional "header" field separate from the typical
data block.  This allows the unique PERQ hard disk "logical header" data to be
defined (and, possibly, physical tape block headers in the future).  Typical
client usage is to subclass StorageDevice to provide struture to the data:

    PERQdisk provides a POS filesystem view through a LogicalDisk class.
    This provides the basis for all of the peculiar address translations
    used by POS (and Accent) to view the raw device as Volumes, Partitions,
    and Directories.
    
    PERQfloppy (rolled into PERQdisk) allows reading and writing of RT-11
    format floppies.  It maps the RT-11 directory structure to floppy
    sectors while PERQmedia handles translation from PRQM, IMD and PFD/raw
    formats.

    PERQemu provides a hardware-level emulation of actual disk, floppy and
    tape drives, interacting at the register/microcode level through device
    controllers.  PERQmedia pulls all of the grungy details of loading and
    saving media images out of the emulator.

And if you order in the next 30 minutes we'll double your order for free!!


Basic Usage
-----------

StorageDevice may be instantiated directly and used with no frills or fuss:

    var dev = new StorageDevice("filename");
    dev.Load();

    // do stuff - dev.Read() and dev.Write()
   
    dev.Save();

This provides the basic access to the data contained in an image, allowing a
client to read and write blocks directly.  The structures that contain the
file's metadata, the device's geometry and performance specs, and the sector
data are organized with typical client usage in mind.  [As the API is still in
active development, details of the properties and methods available will be
included when finalized.]

The StorageDevice remembers the filename assigned to it and which formatter is
used to load it.  By default, the same filename and format will be used to save
the data.  If changing formats, PERQmedia always tries to preserve as much of
the metadata is possible given the target format.


Loading Images
--------------

    dev.CanLoad()
    dev.CanLoad(filename)
        Reads 'filename' to determine if it contains a valid image (of any
        recognized format), and fills in the dev header fields if true.

    dev.Load()
    dev.LoadFrom(filename)
        Sets the dev.Filename field to 'filename', then Load()s the file.


Currently the manipulation of file and path names is entirely up to the client.
Other than a basic Exists() check, the library will not change file extensions
or validate file/path names in any way.  [Subject to change/enhancement.]


Saving Images
-------------

    dev.Save()
        Writes the data to the same file, in the same format, that it was
        loaded from.

    dev.SaveAs(filename)
        Writes the data to 'filename', using the formatter used to initially
        load it.  If the formatter isn't set, PRQM format is used by default.

    dev.SaveWithFormat(format)
        Save to the original filename in the specified format, if applicable.

    dev.SaveAsWithFormat(filename, format)
        Save with the name and format specified.


Interoperability
----------------

All of the formatters can read and write their own images.
All of the formats can be translated into PERQmedia format.
Floppy formats are generally interchangeable with each other.

                       Destination Format

            |  PRQ  |  PHD  |  IMD  |  Raw  |  TAP  |
        ---------------------------------------------
        PRQ |  r/w  |  r/w  |  r/w  |  r/w  |  r/w  |
        ---------------------------------------------
        PHD |   w   |  r/w  |   -   |   -   |   -   |
Source  ---------------------------------------------
Format  IMD |   w   |   -   |  r/w  |   w   |   -   |
        ---------------------------------------------
        Raw |   w   |   -   |   w   |  r/w  |   -   |
        ---------------------------------------------
        TAP |   w   |   -   |   -   |   -   |  r/w  |
        ---------------------------------------------


Things You'll Never Need To Care About, Probably
------------------------------------------------

Nobody in the world besides me will ever know or care about this weird little
implementation detail about the IMDFormatter, but here it is for posterity:

    There's no need to record "deleted" sector marks in the emulator.  If
    we read a .IMD that (accurately) scanned real media with deleted data
    marks, it's sufficient to translate those as "bad" blocks (like the PHD
    format's "bad" flag).  A FloppyDisk subclass of StorageDevice can choose
    to present those to the floppy controller as errored or deleted sectors
    however it sees fit.  There is scant mention of the "SK" bit in the source
    (POS and Accent), though Z80 command codes are defined (POS IO_Private
    Z_Commands enumeration: Z_ReadDeletedData, Z_WrtDeletedData) which leaves
    open the possibility that user-written software could issue those commands.
    (The v87.z80 code does not explicitly issue them to the controller chip,
    however.)   

    In short, on read any sector (compressed or not) with Deleted or Error
    type codes is marked as a "bad" sector; on write to an IMD image, we
    _only_ record those as Error sectors.  PERQemu _may_ opt to report bad
    sectors to the FloppyController as address errors so that the PERQ software
    will see an "address mark not found" error.  But this is only ever likely
    to be applicable if you want to faithfully reproduce a floppy with defects
    on it to write out to a real 8" diskette and stick it in a real PERQ, and
    that is a little hardcore (even for me).

--
Last update: skeezics   Sat Dec 17 23:22:34 PST 2022


 

