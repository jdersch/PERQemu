PERQemu Source - Readme

v1.8 - 12/18/2022 - skeezics
v1.7 - 9/19/2022 - skeezics
v1.6 - 11/29/2021 - skeezics
V1.5 - 6/20/2018 - skeezics
V1.4 - 5/15/2018 - skeezics
V1.3 - 3/11/2018 - skeezics
V1.2 - 3/18/2017 - skeezics
V1.1 - 9/21/2014 - skeezics
V1.0 - 6/01/2013 - jdersch


1.0  Introduction (Author)
==========================

Well, I finally released the source to this thing after years of procrastination.
The purpose of this document is to give a basic overview to what's what and
what's yet to be.

Please go easy on this code; I started working on this in 2006 and this project
slowly evolved from a simple experiment ("can I emulate the PERQ CPU?") to a
monster, so there are things that are suboptimal, ugly, and proabably pretty
boneheaded.  Had I to do it over from scratch I'd do some things very differently.

Regardless, if you have feedback, questions, code changes or if you just want to
yell at someone, please do drop me a line at derschjo@gmail.com.  (I care!)

Thanks for taking a look.  I hope this will prove useful for the remaining PERQ
fanatics out there.

-- Josh


1.1  Introduction (This Branch)
-------------------------------

This "experiments" branch is developing rapidly and undergoing fairly major
restructuring.  I aim to make it possible to merge back into Josh's master
after it's stable enough to release, and after he's had time to go through the
code and clean up all of my quirks, shortcuts, errors and other questionable
coding choices. ;-)

In the meantime, this is more Mac-focused, since that's the primary development
platform I have available.  For now it runs on MacOS X versions as old as 10.11
and 10.13; it hasn't been tested on newer versions of macOS or non-Intel Macs.
PERQemu is now tested almost exclusively in 64-bit mode.  It's unlikely to run
on 32-bit Mono due to OS limitations.

Testing on Windows and Linux (in a VM) is now being integrated, but due to the
sprawling, interactive nature of the emulator there are no automated tests so
I mostly use the same QA approach that 3RCC used, for better or worse: if it
runs the POS "burn in" code (i.e. the SIGGRAPH demos) it's good to go!


1.2  Version History
--------------------

The next release will incorporate major changes and expanded functionality.
It is currently in development on the "experiments" branch as PERQemu 0.4.6
but should warrant a bump to 0.5 given the scope of the changes.

The experimental branch is being bumped to v0.4.8 (streamer added).
PERQemu 0.4.6 was a pre-release snapshot to preview v0.5.0 changes.
PERQemu 0.4.5beta was an experimental/interim release for VCF PNW.
The sixth snapshot was the first Github-tagged release, PERQemu 0.4.4.
The fifth update fixed a single comma, corresponding to PERQemu 0.4.3.
The fourth update was the first on GitHub, corresponding to PERQemu 0.4.2.12.
The third "source release" corresponded to PERQemu 0.4.2.
The second corresponded to PERQemu version 0.4.0.
The first corresponded to PERQemu version 0.3.x.


2.0  New Code Organization
==========================

[This section describes a pretty radical refactoring in the skeezicsb/PERQemu
fork, under the "experiments" branch.  At some point all of these changes
will be rolled into "master" and then Josh can decide if some or all of it
should be pulled into his definitive source tree. -- skeezics 11/11/2021]

Several new top-level folders now provide a bit more structure to the growing
project.  Many of the ideas here are shamelessly cribbed from Josh's other
excellent emulation projects, Contralto and Darkstar.

    Configurator
        A fairly self-contained set of classes that allow for interactive
        configuration of all makes and models in the PERQ line.  It was
        written to hook into a WinForms-based GUI and the interactive CLI.
        As currently integrated into PERQemu, only the CLI version is used
        since there is no 64-bit/Cocoa-based port of WinForms and it's no
        longer tenable to support 32-bit Mono.  "MAUI" seems promising but
        would require entirely new hardware and OS upgrades to even test it.)

    Controller
        The new ExecutionController manages the setup, running, and teardown
        of the virtual machines described by the Configurator.  This class
        was written to hook into a graphical front end (again, WinForms
        based, so not included in PERQemu at this time) as well as provide
        the command-line interface.  The virtual PERQ is encapsulated in a
        PERQSystem object which is instantiated by the Controller.  This
        is the intermediary that fires off background threads for the CPU
        and Z80.

    Debugger
        This directory [in transition] will contain all of the debugging
        features that will be compiled out of the release version.  Logging,
        debugging commands, single stepping, dumping internal state, and
        even some of Josh's super cool Z80 source-level debugging stuff will
        all live here.  When building from source, the Debug configuration
        will include everything; the Release configuration will strip most
        of it out in order to run as fast as possible.  Enabling the DEBUG
        symbol and turning on tracing incurs a massive performance overhead
        but allows really valuable insight into debugging actual PERQ code,
        as well as PERQemu itself.

    Emulator
        All of the code that makes up the PERQ emulation engine lives here.
        When a new PERQSystem is summoned into being, it builds up the system
        in a manner that mirrors the hardware: the Configurator describes the
        "chassis" (model) to which CPU, IO, MEM and Option boards are added
        and selected peripherals attached.  Boot ROM and Z80 firmware images
        are loaded based on the Configuration, and configured disk images are
        loaded.  PERQSystem provides the attachment point for the Debuggers,
        which allows the user to pause, single step or reset the virtual
        machine through the CLI and (eventually) the GUI.  When "powered off"
        the emulator allows the user to save modified disk images before the
        PERQsystem is deallocated.  At this point a new configuration or
        different media may be loaded and the virtual machine reinstantiated.

    UI
        The reorganized user interface is broken down into three folders (of
        which only two are currently integrated into the scheme):

        Forms
            A FrontPanel anchors the graphical interface.  Menus are provided
            for all basic operations, while a graphical toolbar offers quick
            access to the Configurator, a Settings panel, the Debuggers (CPU
            and Z80) and extras like the DDS, power and reset switches, easy
            one-click screenshots, etc.  It's all very snazzy, even with the
            fairly limited functionality of the old crappy Mono Carbon-based
            Winforms/libgdi+ implementation.  Alas.  "MAUI" may someday be a
            possible cross-platform solution, but don't hold your breath.

        SDL
            The Display is a self-contained, minimal window that uses SDL-CS
            (a C# wrapper around SDL2) to pump the PERQ's video bits to your
            screen.  All of the SDL calls and the SDL message pump must run on
            the main application thread.  The display object is managed by the
            PERQSystem.

        CLI
            This is where the new "Debugger" lives.  To make it more general
            purpose it is now called "Command___" instead of "Debugger___";
            CommandAttribute replaces the old DebugFunction to identify which
            methods to hook into the CLI using the same basic approach as
            before, building up a command tree using reflection at startup.
            Due to the dynamic nature of the emulation system, though, most
            of the methods that were once directly invoked on the CPU or other
            emulator objects are now moved into a separate set of classes
            (DebugCommands, ExecCommands, ConfigCommands, etc).  A significant
            change is that the CLI runs on the main application thread even as
            the emulator is running on the CPU/Z80 threads; calls to read the
            console are multiplexed with the SDL event loop so that commands
            may be entered while the PERQ is running!


2.1  The CPU
------------

Under the Emulator/CPU directory you'll find the implementation of the main
PERQ microengine.  If you are not familiar with it I seriously suggest reading
through the following documents on Bitsavers:

    http://bitsavers.trailing-edge.com/pdf/perq/PERQ_CPU_Tech_Ref.pdf
    http://bitsavers.trailing-edge.com/pdf/perq/pos_G5/PERQ_uProgRefMan_Mar84.pdf

    [Note: Bitsavers links may change; Appendix B in UserGuide.pdf will
    provide a more comprehensive bibliography and links to a "permanent"
    repository of documentation.]

The former describes the hardware in intimate detail (and was written by a
supergenius PERQ fanatic madman), and the latter describes the microcode in
detail.  In a nutshell, the PERQ CPU is a microcoded (48-bit microcode word)
machine with 4- or 16K of writeable control store, a 20-bit ALU, 256 general
purpose 20-bit registers, a 16-bit IO bus and just a ton of other neat features.

In the 24-bit variant of the 16K CPU, all of the 20-bit datapath elements are
extended by four bits, allowing access to a much larger memory address range.
Note that the SIXTEEN_K conditional is removed, and CPU type may be configured
by the user at runtime.

CPUBoard.cs is the container for a specific CPU, a Scheduler, and the wrapper
around the Thread that runs the CPU.

CPU.cs contains the nexus of the implementation in the CPU class.  It directly
implements the decoding and execution logic, and provides debugger hooks for
pulling out status information.  To make it a little more "wieldy" it has been
refactored to break out many of the subordinate tasks:

    - The "ALU" class implements the CPU's 20- or 24-bit ALU.
    - The "CallStack" class implements the PERQ's hardware callstack (12 bit
      on 4K machines, 14 bit on 16K).  This is now internal to the Sequencer
      class (see below).
    - The "ControlStore" class manages the writable control store and boot ROM.
    - The "ExpressionStack" class implements the 16-level, 20- or 24-bit push-
      down stack and TOS register.
    - The "ExtendedRegister" class implements the odd two-part register types
      used in several places in the processor, with separate or combined 
      access to the upper and lower parts of the register.
    - The "Instruction" class caches instruction decoding to make things faster.
    - The "InterruptEncoder" class handles priority encoding of 8 hardware
      interrupt sources, and methods to raise or clear them.
    - The "RasterOp" class emulates the hardware datapath for doing fast block
      memory moves used in graphics operations.
    - The "RegisterFile" class provides the XY register file of 256 general
      purpose registers, either 20- or 24-bits wide.
    - The "Sequencer" class implements the Am2910 microsequencer and the PC, S
      and Victim registers.  These are all 12 bits wide in the 4K CPU, extended
      by the "2 bit kluge" to 14 bits in the 16K CPUs.
    - The "Shifter" class implements the PERQ's Shift, Rotate and bitfield
      operations.

CPU.Execute() is the main entrypoint of interest; everything branches off
from here.


2.2  Memory
-----------

The PERQ has a word-addressed memory architecture.  While it is organized around
64-bit "quad word" access, the smallest unit transferred is one 16-bit word at a
time.  All original PERQ models were limited to 1 megaword (2MB) of memory by
their 20-bit physical address limit.  The rare 24-bit PERQ was extended to allow
16 megawords (32MB), but historically only a 2 megaword (4MB) board was produced
due to technology limitations at the time (i.e., 1Mbit DRAMs didn't exist yet).
Documentation hints at the existence of an 8MB PERQ (though no OS software
currently available makes use of that much RAM).

PERQemu can support dynamic memory configuration up to the full utilization of
the 24-bit address space, but has only been fully tested with the 20-bit CPU.

The memory/video board is implemented by the code in Emulator/Memory:

    - The "Memory" class (MemoryBoard.cs) implements the PERQ's memory store
      and memory state machine.
    - The "MemoryController" class provides separate memory input and output
      queues to support the overlapped Fetch/Store required by RasterOp.
    - "VideoController" lives here now, as it's integral to the memory board;
      this is described in the Display section below.

Note that currently PERQemu does not implement the PERQ's DMA hardware; this is
used for high-volume data transfers by the hard disk controller, Ethernet or
I/O Option devices.  The only practical implication is that the emulated PERQ
may run a little faster than the real hardware, as essentially the CPU never
has to surrender memory cycles for DMA transfers. (This may be addressed in a
future release, possibly as an optional RateLimit setting.)


2.3  I/O
--------

All I/O related code lives under the Emulator/IO directory.  The IOBus class
acts as the hub (or "bus," if you will) and essentially delegates I/O reads and
writes from the CPU to the device they're intended for.  I/O devices that
connect to this bus implement the IIODevice interface.

To support dynamic configuration, the abstract IOBoard.cs class provides the
plumbing that hooks into PERQSystem.  Specific implementations are embodied in
IOB.cs, CIO.cs, and EIO.cs; these include the model-specific controllers and a
container for the Z80 subsystem.

Similarly, OptionBoard.cs is a base class for (future) implementations of the
various I/O option boards; only a bare-bones "OIO" board is currently provided.

The Z80 subsystem controls most of the I/O devices on the system; this code
lives under Emulator/IO/Z80.  Z80System implements emulation of the real Z80
system, currently implemented using the Z80dotNet package.  It runs the actual
PERQ Z80 ROM code, including device handling, it's own local DMA, and the
PERQ<->Z80 message protocol.

To get better acquainted with the hardware/software here, I recommend looking
at the following:

    - The Z80 ROM assembly source -- see Docs/v87.z80
    - The PERQ<->Z80 communication protocol -- see Docs/perqz80.doc (plaintext,
      not a Word DOC file)

All devices hung off of the Z80 subsystem implement the IZ80Device interface.


2.3.1  The Z80 Subsystem
------------------------

[Differences between the PERQ-1 "IOB" and the PERQ-2 "EIO" boards are fairly
significant; additional refactoring will likely be required in the next release
to properly support the PERQ-2.  For now, the Z80 code is in one directory.]

All of the Z80 subsystem resides in the Emulator/IO/Z80 directory.  There is a
lot to unpack here!

The Z80dotNet CPU is instantiated by the Z80System object; it requires two
small objects that provide I/O bus mapping (Z80IOBus.cs) and a memory map
(Z80MemoryBus.cs).

Numerous interfaces are defined (ISIODevice.cs, IZ80Device.cs, ICTCDevice.cs
and IDMADevice.cs) to glue parts of the Z80 I/O system together.  These are
pretty straightforward.

There are a bunch of Z80 devices:

    - The "DMARouter" class enumerates the DMA-capable devices that may be
      selected through "IOREG3".
    - In the PERQ-1, the "HardDiskSeekControl" class provides glue logic for
      the seek pulse generator used to move Shugart hard disk heads; this
      complicated dance uses the Z80's CTC and custom hardware to issue pulses
      to the (emulated) hard disk.
    - The "IOReg3" device implements an addressible latch that sits on the
      Z80's I/O bus for managing DMA channel selection and several device
      interrupt enable flags.
    - The "Keyboard" class provides an interface to the 8-bit parallel latch
      for incoming PERQ-1 keyboard characters.
    - The "NECuPD765A" class is a register-level emulation of the eponymous
      floppy disk controller chip.  It is a DMA-capable Z80 device that can
      support up to 4 floppy drives; the PERQ only has the physical wiring to
      accommodate two drives (but only software support for a single drive).
    - The PERQ<->Z80 FIFOs are a hardware channel used to send messages
      between the main CPU and the Z80.  In the PERQ-1, these are simply
      latches; in the PERQ-2 these are actual FIFOs.  Currently two classes
      (PERQtoZ80Fifo.cs and Z80toPERQFifo.cs) implement the unidirectional
      channels and their control registers.  Here thar be dragons.
    - The "TMS9914A" class emulates the TI GPIB controller chip and its
      interface to the Z80 I/O bus.  See below for more.
    - The "Z80CTC" class is an emulation of the Zilog CTC chip used for timing
      and baud rate generation on the PERQ-1 IOB.
    - The "Z80DMA" class emulates the Zilog DMA controller, a single channel
      device that provides byte-wide DMA transfers between several devices and
      the Z80's local RAM.
    - The "Z80SIO" class emulates the Zilog SIO/2 chip used for RS-232,
      "speech", and the Kriz tablet.  The PERQ-1 has one SIO chip; the PERQ-2
      EIO will have two.  The "Z80SIOChannel" class does the heavy lifting.
    

2.3.2  Serial Devices
---------------------

Several serial devices are used in the PERQ.  The PERQemu implementation groups
these into the Emulator/IO/SerialDevices folder.

    - The base "SerialDevice" class provides the base for RS-232 ports:
      "RealPort" uses the System.IO.Ports.SerialPort class to access a host
      serial device (including USB-to-serial adapters on modern PCs that lack
      actual, physical COM ports), while "NullPort" provides a data sink when
      the user hasn't configured one.
    - The POS "RSX:" pseudo device enables text file transfers to and from the
      host.  It's implemented as the "RSXFilePort" class.
    - The "KrizTablet" class emulates the Three Rivers custom "Kriz tablets",
      which use the SIO chip to transmit mouse coordinates.
    - "SerialKeyboard" will contain the driver for the PERQ-2's "VT100-style"
      keyboard, attached to the EIO board.  [Not yet implemented]
    - The "Speech" class will emulate the PERQ's CVSD chip to provide "telephone
      quality" (8Khz, mono) audio output.  This class will provide the glue to
      stream data from the SIO to the SDL audio routines.  [Not yet implemented]

            
2.3.3  GPIB
-----------

All PERQs have a GPIB controller and can work with a variety of GPIB devices.
The emulator currently only supports the Summagraphics BitPadOne tablet, though
code exists to drive a number of now fairly obscure printers and possibly other
GPIB tape drives and other peripherals.  As of v0.5.0, the code is sufficiently
robust to interface with the BitPad, but has not been exhaustively tested and
likely needs additional work.  The code is in the Emulator/IO/GPIB directory:

    - The "GPIBBus" class provides the actual bus abstraction, allowing the
      controller (TMS9914A, in the Z80 folder) to set talker and listener
      addresses and exchange data with them.  All peripherals that attach to
      the bus implement the IGPIBDevice interface.
    - The "BitPadOne" class emulates the classic Summagraphics tablet used
      (primarily) with the PERQ-1.
    - Additional peripherals may be added in a future release.


2.3.4  Storage Devices
----------------------

In the refactoring for v0.5.0, the storage architecture was heavily revamped.
All of the reading and writing of media files on the host is now managed by the
PERQmedia library, which liberates the emulator from worrying about the details
of file formats.  The PERQmedia subproject contains some fairly detailed docs
about its design, interface, file format, etc.

All PERQs shipped with an internal hard disk, and most (or nearly all?) PERQs
included the "optional" floppy drive.  The code to implement disk storage is in
Emulator/IO/DiskDevices:

    - The "FloppyDisk" class is a wrapper around the PERQmedia StorageDevice
      class.  It works with the Z80 floppy disk controller (FDC) to emulate
      the Shugart SA851 8" floppy drive.
    - The "HardDisk" class is a "generic" hard disk.  It wraps the PERQmedia
      StorageDevice class to provide the mechanical operations such as seeks,
      sector operations, an index pulse, and so on.
    - The "ShugartDiskController" class implements the Shugart SA4000 interface
      (the "Disk14Inch" class of drives) common to all PERQ-1 configurations.
      It works with the Z80's HardDiskSeek device to manage stepping the heads.
    - The "MicropolisDiskController" class is in development.  This will
      support the Micropolis 1200-series 8" hard disks that were originally
      introduced with the PERQ-2 and PERQ-2/T1.  This will allow configuration
      of drives in the "Disk8Inch" storage class.
    - The "MFMDiskController" and "SMDController" classes will someday provide
      support for Disk5Inch and DiskSMD devices (on PERQ-2 models).

Several tape drives will also be supported, but PERQemu does not yet emulate
the controllers for these.  Tape media will be accessed through the PERQmedia
library just like disk devices.  The first of these is now in development, with
the code in Emulator/IO/TapeDevices:

    - QICTapeController is the thin PERQ interface to the QIC-02 "bus" that
      attaches one streaming tape drive.  It can be configured as the Tape
      option when the OIO board is selected.
    - Sidewinder.cs contains all of the drive's mechanical and controller
      logic, emulating the Archive Sidewinder 3020I drive.  A ton of extra
      information is included in Docs/Streamer.txt.

All disk (and tape) controllers implement the IStorageController interface as
defined in Emulator/IO/IStorageController.cs.  This provides a common point of
attachment as some IOBoards can accept different controller types.

All modifications to disk contents are in-memory only (they are not directly
backed by files on the disk).  If changes to the disk are to be saved, they
must be flushed manually.


2.3.5  PROMs
------------

The PERQ hardware made fairly extensive use of ROM, PROM and other PLDs (early
1980s PALs and GALs).  Some of the code executed by the PERQ microengine is
based on the actual PERQ binaries, while others are simulations.  The various
files are loaded from the PROM directory at startup:

    - CPU:  The original PERQ-1/1A "boot.rom" contains the microcode to boot
      the 4K or 16K CPUs in a PERQ-1 with an IOB and a Shugart hard disk.
      When a CIO board is installed (IOB with "new Z80" firmware) the file
      "cioboot.rom" is loaded instead.  Additional boot ROM images will be
      included when the PERQ-2/EIO support is completed.

      NOTE: these are the actual ROM dumps, but with their address lines
      "unscrambled".  See PROM/Unscrambler.cs if you're not squeamish.

    - RasterOp:  Two text files and a small Perl script are included to
      build "rsc03emu.rom" and "rds00emu.rom".

    - Memory:  The "bkm16emu.rom" image is used by the Memory State Machine.
      The source and a small Perl hack to build it are included here.

    - Z80:  oioz80.bin and oioz80.lst are new files used by the "real" Z80.
      The .bin file is loaded into the Z80's memory map, while the .lst file
      is a source code listing enhanced to allow the Z80 debugger to map ROM
      addresses to the actual source.  Slick!  The CIO versions are included
      now as well.


2.3.6  Display and UI
---------------------

The VideoController class (now in Emulator/Memory/VideoController.cs) implements
the hardware that controls the display and is responsible for dealing with video
timing, cursor positioning, and rendering scanlines of video from main memory
to the host display's framebuffer.

The framebuffer is supplied by the Display class (UI/SDL/Display.cs).  This uses
the SDL2 library to create a window and render the bitmap data streamed to it by
the VideoController.  It also provides an FPS counter and will (eventually) also
provide for capturing screenshots.

Keyboard and mouse input from the host are now handled by the "InputDevices"
class (UI/SDL/InputDevices.cs).  Input events are filtered and funneled to the
appropriate emulated I/O device.

Mappings from SDL2 to either the PERQ-1 or PERQ-2 keyboard codes are provided
by the KeyboardMap class, in UI/SDL/KeyboardMap.cs.  Testing for the PERQ-2's
"VT100-style" keyboard is incomplete, pending the development of the EIO and
Z80 enhancements for the PERQ-2 line.  There are several fixed mappings for
some special PERQ-specific keys that must be made configurable somehow, or a
graphical keyboard provided so that a variety of host keyboards can be better
accommodated.  See the UserGuide.pdf for more info about this.

Caps-lock, num-lock and scroll-lock tracking hasn't been tested under SDL2;
the use of the Alt key (Option on Mac) to simulate the "mouse off tablet"
condition (for relative-mode mouse tracking) has been restored but has not
been exhaustively tested.


2.3.7  Scheduler
----------------

The Scheduler class is used to fire events within the "virtual time" domain of
the emulation environment.  The PERQ and Z80 each use their own Schedulers,
one running on each thread and at the respective CPU frequencies (so a PERQ
scheduling "tick" is 170ns, while the Z80's is dependent on the I/O board type
loaded).  The Schedulers are used to maintain the proper execution ratio
between the main CPU and the Z80.

The SystemTimer provides a "heartbeat" used to regulate the execution speed of
the emulated PERQ in the real time domain.  It uses the HighResolutionTimer to
try to limit the emulation to exactly 60fps (170ns CPU cycle) on fast hardware
when the "CPUSpeed" RateLimit option setting is enabled.


2.4  The Debugger / Console interface
-------------------------------------

[All of the Debugger/CLI stuff has been split up; the complete command-line
interface now lives in UI/CLI and provides the "Command" attribute that lets
all of the various subsystems hook into the command tree.  All of the support
for debugging will remain here, such as the disassembler, Qcodes, Z80 source
debugger, etc.  Big rewrite required here.]


All Debugger code lives under the top-level Debugger folder.  The DebugAttribute
class defines attributes for functions and properties that can be applied to
emulator code; this allows the main Debugger to use reflection to find methods
it can call and properties it can query or modify.  In theory this makes it
possible for the code that examines/modifies/frobs emulated objects to live
alongside the actual implementation of the emulated objects without too much
work (just set an attribute and the debugger will figure out the rest.)

The debugger builds a tree of possible commands from this reflection.  These
are exposed to the user through the debugger prompt, which provides automatic
command completion.

I had grand plans for this but I never quite finished them.  It should be
fairly easy to extend at this point, but there are some rough edges.


2.5  The Configurator
---------------------

A great (as in "large," not as a reflection of _quality_) wad of code in the
Configurator directory provides the interface to a Configuration object, which
stores all of the metadata required to build a virtual PERQ.  Configuration.cs
contains the class which holds all of the data, with support from ConfigTypes.cs
for enumerating various options.  Configurator contains the rules for modifying
and checking that a Configuration is valid; it has no direct UI and instead
hooks to the GUI (removed) and CLI (ConfigCommands.cs).  Future work to replace
WinForms with a cross-platform SDL2- or MAUI-based GUI will allow more natural,
direct graphical manipulation of Configurations.

StorageCommands.cs provides supplemental CLI commands for managing the creation
and assignment of hard disk and floppy media.

User preferences are configured by the Settings class, also kept here with its
CLI interface (SettingsCommands.cs).  This is another obvious candidate for a
GUI implementation.  Settings aren't yet fully fleshed out, nor are they well
documented in the UserGuide (yet).


2.6  Execution Controller
-------------------------

The Controller directory accumulates a number of utility classes alongside the
ExecutionController class.  Like the Configurator, this is the UI-neutral class
used to manage the creation and startup of the virtual PERQ, run commands (like
pausing, single stepping, etc.) and shutdown of the machine.  It was designed
to be wired to a GUI (old WinForms code removed) and the CLI (ExecCommands.cs).
The ExecutionController takes a Configuration and uses it to instantiate a
PERQSystem object, which is the nexus for the entire virtual machine.  While
the "power on" command seems extraneous in the CLI, the GUI provided an actual
power button modeled on the PERQ-2/Tx front panel... 

Commands typed at the CLI or controls clicked in the GUI trigger changes in the
virtual machine's run state; the Controller provides the state machine and
"plumbing" required to manage these transitions.  At "power off" the entire
PERQSystem is torn down and disposed so that the user may change or restart
another configured PERQ.

Conversion, Events and Paths provide some useful utility routines.  The logging
class in Log.cs is a homegrown logging system that provides highly configurable
output options for debugging and tracing to the console and/or files.

Finally, a HighResolutionTimer class provides a platform-independent way to do
very high resolution timing, although it has been rolled up into the SDL/CLI
event loop and runs on the main application thread, rather than running on a
dedicated background thread; accuracy suffers but it simplifies firing timer
callbacks without cross-thread invokes and only the SystemTimer client really
needs the high resolution (when RateLimit options are engaged).  Sigh.


3.  PERQmedia
=============

The PERQmedia subproject is included in the PERQemu Visual Studio solution and
encapsulates all of the code relating to loading and saving PERQ disk images.
There's a good amount of documentation there.  It also refers to a separate
GitHub repository prepared years ago where some additional OS images are stored;
the idea was/is to have a separate collection of vetted images that are tested
and ready to use with PERQemu.  Currently this includes PNX 1.3 and POS F.15.

The repository may be renamed to make it less confusing... or the "PERQmedia"
subproject will be somehow made more standalone (Nuget package?) and moved out
of the PERQemu project so that it can be better integrated with the other tools
(like PERQdisk, and soon Stut).  The goal is to expand the archive with more of
the _hundreds_ of floppy, disk and tape images that have been recovered and
archived so far.  In addition, permanent links to copies of documentation culled
from Bitsavers and other local sources may be added here, as Bitsavers doesn't
provide permalinks and may be reorganized from time to time.


4.  Miscellany
==============

A lot of interesting documentation can be found under Docs.  Microcode and
Pascal sources extracted from the disks on Bitsavers, a few disassemblies I
started, and a few random odds and ends.  These can be very useful for reverse-
engineering the behavior of hardware.

Work to reorganize and expand/refine the documentation has begun; additional
web resources are being developed.  See the UserGuide.pdf for a list of links.
(Currently the UserGuide is edited in Google Docs and the exported PDF copied
into the PERQemu tree; this isn't ideal, as source ought to be included...)

ChangeLog.txt is a loose / verbose record of development effort over the course
of this "experiments" branch.

With the shift to GitHub and more active collaboration happening, this file is
slowly being broken up into more manageable pieces and reorganized.  But as
PERQemu incorporates more features, additional supplemental bits are being
added as well -- archived media, more scanned documentation, even marketing
materials and photos and more will be gathered into a definitive collection of
PERQ info and lore.  More to come!
