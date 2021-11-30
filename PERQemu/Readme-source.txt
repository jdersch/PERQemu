PERQemu Source - Readme

v1.6 - 11/29/2021 - skeezics
V1.5 - 6/20/2018 - skeezics
V1.4 - 5/15/2018 - skeezics
V1.3 - 3/11/2018 - skeezics
V1.2 - 3/18/2017 - skeezics
V1.1 - 9/21/2014 - skeezics
V1.0 - 6/01/2013 - jdersch

        **
        ** THIS FILE NEEDS TO BE REORGANIZED AND REWRITTEN
        ** 
        ** It's out of date and so much has changed that it
        ** really needs to be broken up into a smaller Readme
        ** and the bulk of the internal documentation moved
        ** into Docs.
        **

1.0 Introduction (Author)
=========================

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


1.1 Introduction (This Branch)
------------------------------

This "experiments" branch is developing rapidly and undergoing fairly major
restructuring.  I aim to make it possible to merge back into Josh's master
after it's stable enough to release, and after he's had time to go through the
code and clean up all of my quirks, shortcuts, errors and other questionable
coding choices. ;-)  In the meantime, this is more Mac-focused, since that's
the development platform I have available.  As soon as is practicable I will
confirm that the changes made here are compatible with Windows and Linux too.

This version uses the new Z80 and SDL2 code, but can still run on 32-bit Mono.
However, the WinForms-based GUI code is not being included, since there's no
way forward to 64 bits (despite hoping FOR THE LAST THREE YEARS) that someone
would finally integrate the WinForms Cocoa backend.  Without that there are NO
other reasonable cross-platform solutions for C#/Mono, so this branch is CLI
only.  The in-development 32-bit GUI version that runs on MacOS 10.11-10.13
won't be released.

There's a ton of reorganization and refactoring to do, so all of this is in a
state of flux.  Check back often for updates.

I too would be thrilled for any feedback you care to send my way:
skeezicsb@gmail.com.

-- Chris


1.2 Version History
-------------------

The next release will incorporate major changes and expanded functionality.
It is currently in development on the "experiments" branch as PERQemu 0.4.6
but should warrant a bump to 0.5 given the scope of the changes.

PERQemu 0.4.5beta was an experimental/interim release for VCF PNW.
The sixth snapshot was the first Github-tagged release, PERQemu 0.4.4.
The fifth update fixed a single comma, corresponding to PERQemu 0.4.3.
The fourth update was the first on GitHub, corresponding to PERQemu 0.4.2.12.
The third "source release" corresponded to PERQemu 0.4.2.
The second corresponded to PERQemu version 0.4.0.
The first corresponded to PERQemu version 0.3.x.


2.0 What's Implemented (and Where)
----------------------------------

The following hardware has been implemented in the emulator to an extent that
it is reasonably functional and stable (though there may still be issues):

- PERQ 4K CPU - Complete.  Everything seems to work, all diagnostics pass.

- PERQ 16K CPU - Complete.  Mulstep/Divstep is complete, but testing has
  not been what you'd call "exhaustive."  Single-precision multiplication and
  division works, though there is no code (in POS D or F, anyway) using the
  Mul/Div unit.  It is unknown at this point if early versions of Accent or PNX
  make use of MulDiv, although later versions do!

- PERQ 24-bit CPU - Complete?  Changes to support the extended 24-bit 16K
  CPU are in place, but no testing can be done until PERQ-2/Z80/EIO changes
  are complete, along with 24-bit ROM images and a configuration option to let
  the user choose which machine to configure.

- Memory - Complete.  The size of main memory is now configurable at runtime
  and is limited only by the type of PERQ you configure: up to 2MB (1 megaword)
  using the 20-bit processor, or up to a theoretical maximum of 32MB (16MW) if
  the 24-bit processor is selected.

    Note: None of the operating systems that ran on the PERQ-1 supported
    more than 1MW of memory.  Until PERQ-2 support is complete, nothing
    beyond 1MW has been tested.

- RasterOp - Complete.  This version is "cycle accurate", or fakes it well
  enough to run multiple operating systems and different versions of the
  RasterOp microcode.  Could be a little faster, though.
  
    Note: There is only one subtle glitch which may be due to buggy microcode
    (only manifests in POS F.15, during one demo) but it's otherwise 100%.

- Hard and Floppy disk - 95% complete.  Enough is implemented to allow bringing
  up an "empty" Shugart hard disk with a new POS install from floppy images.

    These are currently implemented by code under Emulator/IO/HardDisk and
    Emulator/IO/Z80/IOB/FloppyController for the control logic, and
    Emulator/PhysicalDisk for the actual media emulation.  [There is much
    work to be done here.]

- Display - Complete.  The emulator trusts the microcode to set the correct
  video register values to account for horizontal and vertical refresh timing,
  and it appears to be accurate for POS, Accent and PNX.  The portrait display
  (768 x 1024) is complete, and support for the landscape display (1280 x 1024)
  has been added [but not yet tested].  This is configurable at runtime.

  Code that drives the graphics display is in two parts:

    Emulation of the PERQ's memory-resident frame buffer (control registers,
    video timing, cursor control, etc.) is in Emulator/Memory/VideoController.

    The platform-independent display driver, now using SDL2 for rendering,
    is in UI/SDL/Display.

- Z80 Subsystem - In progress!  Josh incorporated a "real" Z80 emulation to
  replace the simulation of the "old Z80" protocol as implemented on the
  original PERQ-1 IOB.  The new Z80 actually executes the PERQ's firmware
  directly, so all of the peripherals and controllers that are managed by the
  Z80 are being rewritten to mimic the real hardware at the register level.
  The huge benefit of this work is that the PERQ-2 "EIO" board can now be
  emulated, allowing PERQemu to run a ton more software and all the newer
  versions of POS, Accent and PNX that used the "new Z80" protocols.
  
    This is implemented by a huge wad of code under Emulator/IO/Z80 and is
    in a state of rapid development.  At the moment, the original v8.7 code
    and the PERQ-1 IOB configuration is supported, but all of this is being
    refactored to support the CIO, EIO and NIO boards.  [11-29-21]

  This will be updated as the new IO and Z80 code is more stable.


2.1 What's In Progress
----------------------

PERQ-2 features to be completed for the next major release:

- Fleshing out the new Z80 system and expanding/testing additional IO board
  types (and locating ROM images).  ALL of the device controllers are being
  rewritten to interact at the register level to actual Z80 I/O instructions.

- Additional hard disk types.  New controllers are required for the Micropolis
  8" and ST-506/MFM 5.25" hard drive interfaces.  Some fairly in-depth changes
  to the user interface to allow selection of different disk types and the
  assignment of media files for multiple-drive systems.

- Landscape display.  [Added; not yet tested]

- PERQ-2 "VT100-style" keyboard map.  [Added; not yet tested]


2.2 What's Not Implemented (Yet)
--------------------------------

- Ethernet.  This is completely unimplemented.  Would be an interesting
  challenge, though POS doesn't make much use of it aside from basic file
  transfer stuff (FTP) or network printing (CPrint).  Accent does, however...
  including a basic TCP/IPv4 stack!

  [Update: Accent S4 will boot, but hangs while attempting to start up the
  network servers; in the d6.phd disk image they are disabled as a workaround
  to get past this.  The much better solution will be to adopt the Ethernet
  emulation code developed for ContrAlto into PERQemu!]

- PERQLink.  I started working on this but didn't spend much time on it.  Would
  be cute to have.  [And very, very useful to do microcode debugging with
  ODTPRQ or PDM... extending the "virtual PERQlink" to use TCP/IP would allow
  two instances of PERQemu to talk to each other; hacking up an Arduino or Pi
  to provide a translation from _real_ PERQ hardware to a PERQemu instance
  would be pretty phenomenal.  Kickstarter, anyone?]

- Sound.  A stub is implemented, but not hooked up to any sort of host output
  device.  The PERQ never made much use of the "speech" output besides simple
  beeps, although there are some amazing demo programs that sing, talk, and
  play Beethoven on-the-fly.  The PERQ basically just streams bits through the
  Z80 SIO in 32-byte chunks through a Motorola MC3417 CVSD chip, which has lots
  of other features that they never had the time or inclination to exploit.
  For emulation it'd be terribly cool to at least get sound output working.

- Printing.  Emulating the Canon CX would be pretty neat (and not altogether
  difficult.)  There is some support in POS for various RS232 and GPIB printers,
  although nobody probably has a working Okidata Microline 184 from 1986
  anymore.  Well, besides me, anyway.


2.3 Operating System Support
----------------------------

As of v0.4.2, the following PERQ operating systems are known to boot:

 - POS versions D.6, F.0 and F.1 (official 3RCC releases);
 - POS version F.15 (released by Boondoggle Heavy Industries, Ltd);
 - MPOS version E.29 (unreleased by 3RCC);
 - Accent S4 (an early version from CMU, unreleased);
 - PNX 1.0 (first public release by ICL).

NOTE: PNX drops into its microcode debugger (i.e., crashes) after booting if
2MB of memory is configured; it runs fine with 1MB.  POS and Accent have no
trouble with a full megaword of memory.

PERQ FLEX is untested, though hopefully media recovery efforts are ongoing or
may soon succeed.  This may provide the first 8" Micropolis hard disk image to
test once the PERQ-2 emulation features are complete.

Accent S4 now tracks the mouse, though it takes a little getting used to since
it runs in relative mode.  To simulate mouse "swipes" you have to use the Alt
key (Command key on Mac) to tell PERQemu the mouse is "off tablet", reposition,
then release the key to start tracking again.  It's a little clumsy at first.


If anyone has any other software that ran on the PERQ-1 and does not run
successfully under PERQemu, send us a copy and we'll find out why!


3.0 The Basic Idea
==================

[This section should probably be split off into a separate guide?  I've been
going bananas with the refactoring and there's so much new stuff in here it's
probably useful to move this into /Docs...]

3.1 New Code Organization
-------------------------

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
        longer tenable to support 32-bit mono.  If/when time permits, I'd
        like to try at least offering the graphical version on Windows,
        even if the Mac/Linux mono ports remain text based.

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
        When a new PERQSystem is summoned into being, it builds the system
        up in a manner that mirrors the actual hardware: a CPU board is
        created based on the user's configuration; the selected memory
        configuration is allocated and the display (portrait or landscape)
        is attached; the appropriate IO board is set up (which in turn loads
        a Z80 subsystem), and finally any optional IO devices are attached
        through an IO Option board (if configured).  As all configuration is
        now dynamic, the appropriate boot ROM and Z80 firmware images are
        loaded, and any configured disk drive images are loaded.  With the
        (planned) GUI, the user may open debugger windows, load and unload
        floppy disks, pause, single step or reset the machine, etc. through
        direct on-screen buttons; for now, all interaction is through the
        CLI.  When the "power off" command is given, the user may save any
        modified disk images before the PERQsystem is deallocated.  At this
        point a new configuration or different media may be loaded and the
        virtual machine reinstantiated.

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
            Winforms/libgdi+ implementation.  Alas.

        SDL
            The Display is a self-contained, minimal window that uses SDL-CS
            (a C# wrapper around SDL2) to pump the PERQ's video bits to your
            screen.  All of the SDL calls and the SDL message pump must run on
            the main application thread.  The display object is managed by the
            PERQSystem.

        CLI
            This is where the "new" Debugger lives.  To make it more "general
            purpose" it is now called "Command___" instead of "Debugger___"
            and the CommandAttribute replaces the old DebugFunction to identify
            which methods to hook into the CLI.  It offers a bunch of nifty new
            features.  Of course, it was written before Josh added some new
            stuff to the original debugger, so there is work to be done to
            incorporate those changes into the new code.  The new CLI uses the
            same basic approach, building up a command tree using reflection at
            startup.  Due to the dynamic nature of the emulation system, though,
            most of the methods that were once directly invoked on the CPU or
            other emulator objects are now moved into a separate set of classes
            (DebugCommands, ExecCommands, ConfigCommands, etc).



3.2 The CPU and Memory
----------------------

Under the Emulator/CPU directory you'll find the implementation of the main
PERQ microengine.  If you are not familiar with it I seriously suggest reading
through the following documents on Bitsavers:

    http://bitsavers.trailing-edge.com/pdf/perq/PERQ_CPU_Tech_Ref.pdf
    http://bitsavers.trailing-edge.com/pdf/perq/pos_G5/PERQ_uProgRefMan_Mar84.pdf

The former describes the hardware in intimate detail (and was written by a
supergenius PERQ fanatic madman), and the latter describes the microcode in
detail.  In a nutshell, the PERQ CPU is a microcoded (48-bit microcode word)
machine with 4- or 16K of writeable control store, a 20-bit ALU, 256 general
purpose 20-bit registers, a 16-bit IO bus and just a ton of other neat features.

In the 24-bit variant of the 16K CPU, all of the 20-bit datapath elements are
extended by four bits, allowing access to a much larger memory address range.
Note that the SIXTEEN_K conditional is removed, and CPU type may be configured
by the user at runtime.

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


All original PERQ models were limited to 2MB (1MW) of memory by their 20-bit
address limit.  The rare 24-bit PERQ was extended to allow 32MB (16MW), but
historically only a 4MB (2MW) board was produced due to technology limitations
at the time (i.e., 1Mbit DRAMs didn't exist yet).  Documentation hints at the
existence of an 8MB PERQ (though no OS software currently available makes use
of that much RAM).  PERQemu will allow dynamic memory configuration up to the
full utilization of the 24-bit address space (?); for now a half megaword (1MB)
is standard.

    - The "Memory" class (Emulator/Memory/MemoryBoard.cs) implements the
      PERQ's memory store and memory state machine.
    - The "MemoryController" class (Emulator/Memory/MemoryController.cs)
      provides separate memory input and output queues to support the
      overlapped Fetch/Store required by RasterOp.
    - "VideoController" lives here now, as it's integral to the memory board.


3.3 I/O
-------

[This is all undergoing a major overhaul as the "real Z80" is introduced.]

All I/O related code lives under the Emulator/IO directory.  The IOBus class
acts as the hub (or "bus," if you will) and essentially delegates I/O reads and
writes from the CPU to the device they're intended for.  I/O devices that
connect to this bus implement the IIODevice interface.

The Z80 subsystem controls most of the I/O devices on the system; this code
lives under Emulator/IO/Z80/IOB.  Z80System implements emulation of the real
Z80 system, running the actual PERQ Z80 ROM code, including device handling,
DMA, and the (fairly well documented) PERQ<->Z80 communication protocol.

The PERQ hardware provides both input and output FIFOs between the PERQ CPU and
the Z80 CPU.  The PERQ sends the Z80 a message, the Z80 processes that message
and returns a response.

To get better acquainted with the hardware/software here, I recommend looking
at the following:

    - The Z80 ROM assembly source -- see Docs/v87.z80
    - The PERQ<->Z80 communication protocol -- see Docs/perqz80.doc (plaintext,
      not a Word DOC file)

All devices hung off of the Z80 subsystem implement the IZ80Device interface.

RS-232 ports are kinda broken right now.  Lots of work to do with integrating
them into the Configurator/Settings and new Z80 stuff.


3.4 PROMs
---------

The PERQ hardware made fairly extensive use of ROM, PROM and other PLDs (early
1980s PALs and GALs).  Some of the code executed by the PERQ microengine is
based on the actual PERQ binaries, while others are simulations.  The various
files are loaded from the Emulator/PROM directory at startup:

    - CPU:  The original PERQ-1/1A "boot.bin" contains the microcode to boot
      the 4K or 16K CPUs in a PERQ-1 with an IOB and a Shugart hard disk.
      Images are still needed for the PERQ-1/1A to boot when a "CIO" is
      installed (IOB with "new Z80" firmware -- and possibly Micropolis 8"
      disk support too!?) and for the EIO board (both 20- and 24-bit variants).

      NOTE: these are not the actual ROM dumps, but are the .bin files output
      from the microassembler.  They seem to be "scrambled" in the same way
      that the actual ROM images are, though?

    - RasterOp:  Two text files and a small Perl script are included to
      build "rsc03emu.rom" and "rds00emu.rom".

    - Memory:  The "bkm16emu.rom" image is used by the Memory State Machine.
      The source and a small Perl hack to build it are included here.

    - Z80:  pz80.bin and pz80.lst are the new files used by the "real" Z80.
      The .bin file is loaded into the Z80's memory map, while the .lst file
      is a source code listing enhanced to allow the Z80 debugger to map ROM
      addresses to the actual source.  Slick!

      HOWEVER, we now have to come up with the v10.017 ("new" Z80) ROMs for
      the CIO/EIO/NIO...


3.5 Storage
-----------

[This too is changing to accommodate PERQ 2 disk types]

The Configurator is designed to allow a complete configuration, including the
media images, to be stored and recalled from files in the Conf/ directory.
This means both pre-configured PERQs (with default disk images provided with
the PERQemu distribution) and user-defined configurations may be quickly loaded
and run without a bunch of extra typing or poking around in the GUI.

To accommodate removable media, such as floppies (and, later tapes or even
removable disk packs) the CLI still provides load and unload commands while
the virtual machine is running.  The number and type of storage devices for
a given configuration is determined by the selected I/O Board and chassis type
(and, in future, by the Option Board).

All media files are loaded into memory, and changes made by the virtual PERQ
must be saved by the user if desired.  New preferences allow control over the
automatic saving of updated images (see "settings autosave" in the CLI).  The
name of the media file is remembered so that any "save" is written back to
the original file; an [equivalent] "save as" is provided to easily make copies
of any floppy, hard disk [or tape] image without disturbing the original.

PERQemu [should/will] treat files set to "read only" by filesystem permissions
as read-only media; this makes sense for floppy and tape images which have the
analogous physical means to make them unwritable.  If possible, the "default"
configuration should always be available and unmodified, while user created
media and config files can be changed all willy nilly...

A proposed new structure for multiple disk support and different controller
types is detailed in Docs/Configuration.txt.

[Original doc:]
The physical media is simulated by the PhyicalDisk implementations under
/PhysicalDisk.  RawFloppyDisk wraps a RAW floppy image.  ShugartDisk wraps an
image of a PERQ 1/1A's Shugart SA4000-series disk (12- or 24MB).

The hardware interface / controller is implemented by the ShugartController
class (under /IO/HardDisk/ShugartController) for the hard disk, and
/IO/Z80/IOB/FloppyController.cs for the floppy drive.

All modifications to disk contents are in-memory only (they are not directly
backed by files on the disk).  If changes to the disk are to be saved, they
must be flushed manually.


3.5.1 Floppy Notes
------------------

The PERQ supports two different and incompatible floppy formats: POS filesystem
floppies (which can be bootable, and mounted/used just like the hard disk) and
RT-11 compatible data floppies.  By default, all floppies are formatted as
double-sided, single-density (DSSD), giving around .5MB of storage capacity.

PNX uses a third floppy format, based on the V7/System III-era Unix filesystem.
These are incompatible with POS, but POS-compatible RT-11 floppies are
potentially usable for file exchange.

The PERQ hardware supports double-density floppies but PERQemu does not -- yet.
You can sometimes read double-density RT-11 floppies, but writing tends to blow
things up (usually hanging, or crashing, the emulator).  [This will be fixed as
part of the ongoing PERQ-2 changes.]


3.6 The Display
---------------

[This is changing to support the PERQ-2/landscape and change to SDL2]

The VideoController class (now in Emulator/Memory/VideoController.cs) implements
the hardware that controls the display and is responsible for dealing with video
timing, cursor positioning, and rendering scanlines of video from main memory
(/Memory/MemoryBoard.cs) to the host display's framebuffer.  The framebuffer is
supplied by the Display class (UI/SDL/Display.cs).

The Display class provides the display for the PERQ's bitmapped display, and it
also provides keyboard and mouse input.  (In a perfect world this code would be
factored out of this class.)  This input is funneled to the appropriate emulated
I/O device.

Mappings from SDL2 to either the PERQ-1 or PERQ-2 keyboard codes are provided
by the KeyboardMap class, in UI/SDL/KeyboardMap.cs.  Testing for the PERQ-2's
"VT100-style" keyboard is incomplete, pending the development of the EIO and
Z80 enhancements for the PERQ-2 line.

Caps-lock, num-lock and scroll-lock tracking hasn't been tested under SDL2;
the use of the Alt key (Option on Mac) to simulate the "mouse off tablet"
condition (for relative-mode mouse tracking) has been restored but has not
been exhaustively tested.


3.7 The Debugger / Console interface
------------------------------------

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

[All of the Debugger/CLI stuff has been split up; the complete command-line
interface now lives in UI/CLI and provides the "Command" attribute that lets
all of the various subsystems hook into the command tree.  All of the support
for debugging will remain here, such as the disassembler, Qcodes, Z80 source
debugger, etc.  Big rewrite required here.]


3.8 Extras
----------

A lot of interesting documentation can be found under Docs.  Microcode and
Pascal sources extracted from the disks on Bitsavers, a few disassemblies I
started, and a few random odds and ends.  These can be very useful for reverse-
engineering the behavior of hardware. [Note: much of this culled from the
source tarball; should curate a nice collection of sources and documentation
as an optional add-in package.]


3.9 Miscellany
--------------

With the shift to GitHub and more active collaboration happening, this file
should be split into a ChangeLog, TODO, and more static README, undoing the
mess I've made of this. [I'm putting this on my to-do list before the next
update, 'cuz this is getting out of hand.  skz]
