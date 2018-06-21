PERQemu Source - Readme

V1.5 - 6/20/2018 - skeezics
V1.4 - 5/15/2018 - skeezics
V1.3 - 3/11/2018 - skeezics
V1.2 - 3/18/2017 - skeezics
V1.1 - 9/21/2014 - skeezics
V1.0 - 6/01/2013 - jdersch


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


1.1 Introduction (Current maintainer)
-------------------------------------

As the original self-described "PERQ Fanatic," I've taken up the mantle of
ongoing development of PERQemu.  Despite Josh's modesty, it's pretty amazing
what he accomplished.  As I go into this with zero .NET or C# experience,
learning as I go is a little daunting.  My aim is not just to add functionality
and fill in missing features, but to do it in a way that isn't terribly
embarassing to either of us!  Now that the project is on Github you can judge
for yourself if we've succeeded.

The focus in my first release was to complete the 16K CPU features and get
Accent to boot.  To that end, I replaced the "fake RasterOp" with a cycle-
accurate "real RasterOp" pipeline that's closer to the hardware implementation.
At the interim 1.1 release, it was working well enough to run POS, Accent and
PNX!  There were some glitches, the code was fairly atrocious, and performance
was far from optimal.

In the 1.2 release, I completed a far more extensive reimplementation of the
RasterOp pipeline and worked to make it smaller, tighter, and faster, while
eliminating the last of the show-stopping bugs (emulation crashes) and visual
glitches (artifacts on screen).  I'm pleased to report that this RasterOp is
99.9% there -- only one remaining puzzle in one particular scenario that should
be fixable with a small ROM update.  At this point the next performance
optimization for RasterOp (and overall emulation speed) will come from tuning
the Memory implementation, although there may be some further tuning to remove
dynamic memory allocations and reduce overhead.  Numerous other little changes
are detailed elsewhere.

The 1.3 update does, in fact, comprise the Memory rewrite.  Drawing inspiration
from the hardware, all of the complex timing is now managed by a state machine
driven by a new ROM.  This has drastically reduced the amount of GC overhead
and given a decent performance boost!  As part of this, the CPU can now be
single-stepped through microinstructions that abort due to memory holds, which
is an aid to debugging (if you like single-stepping through microcode as much
as I do).  And the last obvious RasterOp glitch was, in fact, fixed with a ROM
update!

What I'm calling the 1.4 update was a month-long effort to fix a single comma
which prevented mouse tracking from working in Accent S4.  It's a long story. 

This 1.5 update wraps up a bunch of small changes and fixes so that I can 
finally push "POS F.15" out the door, along with the update disk image and
fixes for Accent S4.  My fork of the project is at skeezicsb/PERQemu, where
I can wreak havoc without worrying about breaking Josh's master copy.  Poke
around in there for the latest mischief. 

I've built this project under both Visual Studio 2010 (WinXP Pro 64-bit) and
MonoDevelop (Mac OS X) from a shared NetApp repository using DOS-style line
endings; there may be a few formatting differences but the project was
compatible with and opened just fine in both environments.  [After an "upgrade"
to Xamarin Studio Community and the shift to Github, updated files sometimes
now generate huge diffs based on whitespace -- tabs to spaces expansion --
which it seems to apply at random.  Sigh.]

I too would be thrilled for any feedback you care to send my way:
skeezicsb@gmail.com.

-- Chris


1.2 Version History
-------------------

This snapshot is the first Github-tagged release, for PERQemu 0.4.4.
The fifth update fixed a single comma, corresponding to PERQemu 0.4.3.
The fourth update was the first on GitHub, corresponding to PERQemu 0.4.2.12.
The third "source release" corresponded to PERQemu 0.4.2.
The second corresponded to PERQemu version 0.4.0.
The first corresponded to PERQemu version 0.3.x.


2.0 What's Implemented (and Where)
----------------------------------

The following hardware has been implemented in the emulator to an extent that
it is reasonably functional and stable (though there may still be issues):

- PERQ 4K CPU - 99.9% complete.  Everything seems to work, all diagnostics pass.

- PERQ 16K CPU - 99.9% complete.  Mulstep/Divstep is complete, but testing has
  not been what you'd call "exhaustive."  Single-precision multiplication and
  division works, though there is no code (in POS D or F, anyway) using the
  Mul/Div unit.  It is unknown at this point if early versions of Accent or PNX
  make use of MulDiv, although later versions do.

    These are both implemented by code under the \CPU directory.  16K CPU
    support is toggled by the "SIXTEEN_K" conditional compilation symbol.


- Memory - 99.9% complete.  The default memory board provides 1MB (.5MW) of RAM.
  You can increase this to 2MB (1MW, the maximum supported) by enabling the
  "TWO_MEG" conditional compilation symbol, which is enabled by default in all
  release builds.  The PERQ could run with as little as .5MB (.25MW) but unless
  you're a real masochist, stick with as much memory as the old beastie can
  support.  [This will eventually be configurable at runtime.]

    This is implemented by code in the \Memory directory.  A new ROM image
    for the memory state machine and a Perl script to build it are included
    in the \PROM directory.


- RasterOp - 99.9% complete.  The newest cycle-accurate emulation, working with
  the new Memory implementation, is enabled by default.  The old "fake" RasterOp
  code is gone.

    RasterOp still lives in the \Memory directory, though it probably ought to
    be in \CPU.  Two new text files and a small Perl script are included in the
    \PROM directory which are used to build a new pair of small ROMs that drive
    the new RasterOp, and contain usage notes.


- Hard and Floppy disk - 95% complete.  Enough is implemented to allow bringing
  up an "empty" Shugart hard disk with a new POS install from floppy images.

    These are implemented by code under \IO\HardDisk and
    \IO\Z80\IOB\FloppyController for the control logic, and \PhysicalDisk
    for the actual media emulation.


- Display - 99% complete.  The emulator trusts the microcode to set the correct
  video register values to account for horizontal and vertical refresh timing,
  and it appears to be accurate for POS, Accent and PNX.  Only the portrait
  display is offered (for now).

    This is implemented by code under \Display.


- Z80 Subsystem - 97% complete.  Currently only a simulation of the real thing;
  as this is completely transparent (the PERQ1 can't upload custom software to
  the Z80 so a simulation is sufficient) a full emulation of the Z80 hardware
  is very low on my list of priorities (but it would be nice to have for
  accuracy's sake, and to enable PERQ-2 emulation).

    This is implemented by a huge wad of code under \IO\Z80.  This deserves its
    own section, really.  Subdevices that are implemented are:

        - Keyboard - This is implemented by code in \IO\Z80\IOB\Keyboard.cs.
          Now includes a platform-independent driver in 100% managed code;
          it works under Mono on both Linux and Mac OS X [see caveat, below]
        - RS232 - \IO\Z80\IOB\RS232.cs, as well as actual device interfaces
          under \IO\SerialDevices.
        - "Kriz" Tablet.  This was a proprietary electromagnetic ranging tablet
          mostly used on the PERQ-2 machines, but it was supported on the
          PERQ-1 too.  Code in \IO\Z80\IOB\Tablet.cs
        - GPIB - Has been fleshed out a bit, as it now supports talker/listener
          selection, and can discriminate between command bytes and data bytes
          passed on to bus devices (allowing for printers or other GPIB devices
          to both read and write on the bus now).  Only the Summagraphics
          BitPadOne graphics tablet is fully implemented.  \IO\Z80\IOB\GPIB.cs
          is the Z80 side; \IO\GPIB\* is the bus and BitPad client code.
        - Speech (sound chip).  Basically a stub.  \IO\Z80\IOB\Speech.cs


2.1 What's Not Implemented
--------------------------

- A True Z80 emulation.  This will be necessary in order to support the later
  PERQ 2 models.  Not impossible, just a lot of work.  Should be possible to
  adapt existing Z80 device implementations but a lot will have to be changed.
  Fortunately, disassemblies of the Z80 ROMs are available, and the hardware
  interfaces shouldn't be hard to suss out.


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


- Support for other PERQ models (the 2/T2/T4).  As noted above, this will
  require full Z80 support.  It'll also require support for the Micropolis
  and ST-506 hard drive interfaces, different video layout (portrait vs.
  landscape) and tweaks to the keyboard handling (more keys).  Would probably
  also necessitate some serious code refactoring.  [Update: short of a full-
  blown Z80 emulation, a CIO/Micropolis version of the IO Board, running the
  "new" Z80 protocol, would allow for our emulated PERQ-1 to run Accent S5/S6,
  PNX 2+, and POS G.  This could be a much quicker way to expand the amount of
  available software we can run, and some steps have been taken to investigate
  ways to make PERQemu more configurable at runtime. Watch this space.]


2.2 Operating System Support
----------------------------

As of v0.4.2, the following PERQ operating systems are known to boot:

 - POS versions D.6, F.0 and F.1 (official 3RCC releases);
 - POS version F.15 (to be released RSN by Boondoggle Heavy Industries, Ltd);
 - Accent S4 (an early version from CMU, unreleased);
 - PNX 1.0 (first public release by ICL).

NOTE: PNX drops into its microcode debugger (i.e., crashes) after booting if
TWO_MEG is defined; it runs fine with 1MB.  POS and Accent have no trouble with
a full megaword of memory.

MPOS (a multitasking version of POS, unreleased by 3RCC) is unknown, as no
complete set of installation media (or a working hard disk image) is available
at this time.  Would VERY MUCH like to track down a copy.

FLEX is a complete unknown, as it's unclear if anyone anywhere in the world
still has a copy of this obscure (mythical?) operating system.  Hello, UK?

Accent S4 now tracks the mouse, though it takes a little getting used to since
it runs in relative mode.  To simulate mouse "swipes" you have to use the Alt
(or Command on Mac) key to tell PERQemu the mouse is "off tablet", reposition,
then release the key to start tracking again.  It's a little clumsy at first.
[Accent sources would still be hugely appreciated!]


If anyone has any other software that ran on the PERQ-1 and does not run
successfully under PERQemu, send us a copy and we'll find out why!


3.0 The Basic Idea
==================

3.1 The CPU and Memory
----------------------

Under the \CPU directory you'll find the implementation of the main PERQ CPU.
If you are not familiar with it I seriously suggest reading through the
following documents on Bitsavers:

    http://bitsavers.trailing-edge.com/pdf/perq/PERQ_CPU_Tech_Ref.pdf
    http://bitsavers.trailing-edge.com/pdf/perq/pos_G5/PERQ_uProgRefMan_Mar84.pdf

The former describes the hardware in intimate detail (and was written by a
supergenius PERQ fanatic madman), and the latter describes the microcode in
detail.  In a nutshell, the PERQ CPU is a microcoded (48-bit microcode word)
machine with 4- or 16K of writeable control store, a 20-bit ALU, 256 general
purpose 20-bit registers, a 16-bit IO bus and just a ton of other neat features.

CPU.cs contains the nexus of the implementation in the PERQCpu class.  PERQCpu
implements the decoding and execution logic, writeable control store, and defers
to the following for subordinate tasks:
    - The "ALU" class implements the CPU's 20-bit ALU.
    - The "Shifter" class implements the PERQ's Shift, Rotate and bitfield
      operations.
    - The "CallStack" class implements the PERQ's hardware callstack (12 bit
      on 4K machines, 14 bit on 16K).
    - The "ExtendedRegister" class implements the odd 14 bit (12 bit with the
      "2 bit kluge" tacked on) PC and S registers on the 16K CPU.
    - The "Instruction" class caches instruction decoding to make things a bit
      faster.
    - The "Memory" class (\Memory\Memory.cs) implements the PERQ's memory store
      and memory state machine.
    - The "MemoryController" class (\Memory\MemoryController.cs) provides
      separate memory input and output queues to support the overlapped Fetch/
      Store required by RasterOp.
    - The "RasterOp" class (\Memory\RasterOp.cs) is the "real" RasterOp that
      emulates the hardware datapath.

PERQCpu.Execute() is the main entrypoint of interest; everything branches off
from here.


3.2 I/O
-------

All I/O related code lives under the \IO directory.  The IOBus class acts as
the hub (or "bus," if you will) and essentially delegates I/O reads and writes
from the CPU to the device they're intended for.  I/O devices that connect to
this bus implement the IIODevice interface.

The Z80 subsystem controls most of the I/O devices on the system; this code
lives under \IO\Z80\IOB.  Z80System implements a simulation of the real Z80
system (not an emulation) and implements the (fairly well documented) PERQ<->Z80
communication protocol.

The PERQ hardware provides both input and output FIFOs between the PERQ CPU and
the Z80 CPU.  The PERQ sends the Z80 a message, the Z80 processes that message
and returns a response.

To get better acquainted with the hardware/software here, I recommend looking
at the following:

    - The Z80 ROM assembly source -- see \Docs\v87.z80
    - The PERQ<->Z80 communication protocol -- see \Docs\perqz80.doc (plaintext,
      not a Word DOC file)

All devices hung off of the Z80 subsystem implement the IZ80Device interface.


3.3 Storage
-----------

The physical media is simulated by the PhyicalDisk implementations under
\PhysicalDisk.  RawFloppyDisk wraps a RAW floppy image.  ShugartDisk wraps an
image of a PERQ 1/1A's Shugart SA4000-series disk (12- or 24MB).

The hardware interface / controller is implemented by the ShugartController
class (under \IO\HardDisk\ShugartController) for the hard disk, and
\IO\Z80\IOB\FloppyController.cs for the floppy drive.

All modifications to disk contents are in-memory only (they are not directly
backed by files on the disk).  If changes to the disk are to be saved, they
must be flushed manually.


3.3.5 Floppy Notes
------------------

The PERQ supports two different and incompatible floppy formats: POS filesystem
floppies (which can be bootable, and mounted/used just like the hard disk) and
RT-11 compatible data floppies.  By default, all floppies are formatted as
double-sided, single-density (DSSD), giving around .5MB of storage capacity.

PNX uses a third floppy format, based on the V7/System III-era Unix filesystem.
These are incompatible with POS, but POS-compatible RT-11 floppies are
potentially usable for file exchange.  (Note that PNX also cannot co-reside on
a Shugart hard disk with POS or Accent.)

The PERQ hardware supports double-density floppies but PERQemu does not -- yet.
You can sometimes read double-density RT-11 floppies, but writing tends to blow
things up (usually hanging, or crashing, the emulator).  A new format called
.PFD adds a short header that will allow PERQemu to fully support reading and
writing SSDD or DSDD floppies; it is compatible with .RAW so PERQemu can read
them right now.  Watch this space.


3.4 The Display
---------------

The VideoController class (in \Display\VideoController.cs) implements the
hardware that controls the display and is responsible for dealing with video
timing, cursor positioning, and rendering scanlines of video from main memory
(\Memory\Memory.cs) to the host display's framebuffer.  The framebuffer is
supplied by the Display class (\Display\Display.cs) and at the moment contains
the only Windows-specific code (using Windows.Forms to provide a graphics
surface & window).  Fortunately, Mono provides an implementation so in practice
this does not prevent code from running on non-Windows platforms.

The Display class provides the display for the PERQ's bitmapped display, and it
also provides keyboard and mouse input.  (In a perfect world this code would be
factored out of this class.)  This input is funneled to the appropriate emulated
I/O device.

    The mapping from Windows .NET to PERQ key codes is now fully-managed code;
    no more need for PInvokes, which makes Mono much happier.  It lives in
    \HostInterface\KeyboardMap.cs.

    Note: the Mac OS X keyboard driver for Mono is utterly broken for our
    purposes so, I ended up basically rewriting it.  Since apparently nobody
    gives a rat's rear end about the PowerPC anymore, it's probably pointless
    to send in a patch; I'm only testing under Mono 2.10.x (last version that
    runs on it).  Please contact me for a patch if you want to try PERQemu on
    your G5. :)  [Update: still borked on the latest Mono to run on MacOS X
    Yosemite/Intel.  The same patch still applies, so obviously nobody has
    looked at that code in years.]

    Mono on Linux seems to work out of the box, though it seems Caps Lock
    events aren't passed through.  So NO SHOUTING at PERQemu on Linux, for now.
    Also, you may need to "set rs232 rsx" on Linux, at least if trying to boot
    PNX.  [The "set rs232" command doesn't yet accept Unix device paths yet,
    and I haven't done any testing on any Unix variant of hardware serial ports]


3.5 The Debugger / Console interface
------------------------------------

All Debugger code lives under \Debugger.  The DebugAttribute class defines
attributes for functions and properties that can be applied to emulator code;
this allows the main Debugger to use reflection to find methods it can call and
properties it can query or modify.  In theory this makes it possible for the
code that examines/modifies/frobs emulated objects to live alongside the actual
implementation of the emulated objects without too much work (just set an
attribute and the debugger will figure out the rest.)

The debugger builds a tree of possible commands from this reflection.  These
are exposed to the user through the debugger prompt, which provides automatic
command completion.

I had grand plans for this but I never quite finished them.  It should be
fairly easy to extend at this point, but there are some rough edges.


3.6 Extras
----------

A lot of interesting documentation can be found under \Docs.  Microcode and
Pascal sources extracted from the disks on Bitsavers, a few disassemblies I
started, and a few random odds and ends.  These can be very useful for reverse-
engineering the behavior of hardware. [Note: much of this culled from the
source tarball; should curate a nice collection of sources and documentation
as an optional add-in package.]


3.7 Miscellany
--------------

With the shift to GitHub and more active collaboration happening, this file
should be split into a ChangeLog, TODO, and more static README, undoing the
mess I've made of this. [I'm putting this on my to-do list before the next
update, 'cuz this is getting out of hand.  skz]
