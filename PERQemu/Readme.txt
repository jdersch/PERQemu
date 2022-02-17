PERQemu Readme

2/15/2022 - skeezicsb - v0.4.6 (experimental)
3/14/2019 - skeezicsb - v0.4.5beta (unreleased)
6/24/2018 - skeezicsb - v0.4 - v0.4.4
6/24/2010 - jdersch - v0.1 - v0.3


    ***********
    * NOTICE! *
    ***********

    This "experiments" branch is a pretty major divergence from the PERQemu
    master.  The skeezicsb/master should track the original jdersch/master
    but will occasionally incorporate a few tweaks to make it work on the
    Mac and Mono.  During the summer of 2021, Josh began the work to add a
    "real Z80" to the I/O board to allow for PERQ-2 emulation, and made big
    changes to switch from WinForms to SDL2 for the display.  AWESOME!


    Summary of changes on this branch since October, 2021:

    1. New SDL2 Display.cs has been hacked to allow it to run on Mono under
    macOS.  The Windows version may be able to spawn a separate thread for
    SDL's event loop, but on Mac (and Linux?) this is apparently impossible;
    thus, I've clumsily forced it back onto the main thread.
   
    2. Verified that this version runs on MacOS X 10.11 (El Capitan) AND on
    10.13 (High Sierra).  These are the newest versions I have available
    for testing.  They run in both 32- and 64-bit mode!  The horrible Mac
    keyboard hack is no longer required, which is good since we can't
    support the old Mono 4.6.1 runtime anymore. :-/  Absolutely no idea
    if anything runs on 10.14+, and I haven't tried to fire up the Linux VM
    in quite some time... will get to that when we get closer to wrapping up
    an actual release!

    3. The CPU class has been refactored to include support for the 24-bit
    processor!  Configurator allows up to 8MB (4MW) of memory, and the
    VideoController should properly decode addresses for the >2MB configs.
    Landscape display is supported but can't (yet) be tested until a newer
    OS can boot (requires CIO/EIO for "new" Z80).

    4. Enhancements to the CLI that make it more TOPS-20/Cisco IOS-like,
    with tab-expansion providing matching for enumerated options and more
    in-line help.  There are still some enhancements that Josh folded into
    the old Debugger yet to be incorporated (the ":variable" syntax is
    missing/busted at the moment).

    5. If I could back-port a 64-bit Cocoa WinForms driver (<bashes head
    on desk>) to run on 10.11-10.13, a pretty slick graphical configuration
    tool and front end is already written, but abandoned due to 32-bit app
    support being dropped by Apple.  Sigh.


    Updates in November, 2021:

    1. Removed the old Z80 and moved Z80_new into its place.  Started to
    refactor the IO so that different board types can be selected -- the
    EIO for the PERQ-2 is rather different than the old IOB.  Have quite a
    bit of work to do before we can start building virtual PERQ-2 machines
    but there's a pinprick of light at the end of that long dark tunnel. :-)

    2. Amazingly, with all of my ham-fisted shenanigans over the last month,
    it still builds and runs!  Getting ~ 32fps pretty consistently on my
    old Mac now, and really hope that moving the bulk of the CPU onto its
    own thread will finally get me closer to 170ns/60fps emulation (on my
    "old" hardware).  Then I can drop in the rate-limiting stuff for those
    of you with hardware built in the last decade, where it should really fly.


    December, 2021 update:

    1. The PERQ now runs on its own thread, and some "improvements" to speed
    up video have been removed, because apparently writing worse code somehow
    makes it go faster.  Sure.  Removing the 1bpp to 32bpp translation loop
    adds 10-12fps (shaves 80-90ns off the average microcycle time) so there
    has to be a way to speed that up and/or move it off the CPU thread that
    isn't too horrible.  Although doing it in the most idiotic way possible
    will probably make it go even faster, so I need to clearly bash myself
    over the head with a shovel and snort a fifth of Chivas Regal before
    coding that up.  Maybe throw in some GOTOs and really make it fly.

    All of the documentation clearly needs to be rewritten.  More to come.


    Holy crap it's January, 2022:

    1. PERQmedia is a new unified storage object complete with a new common
    file format for storing PERQ hard disk, floppy and even tape images.  It
    works with all the old formats, but removes ALL of the file reading and
    writing grunge from the Emulator proper.  Included as a shared subproject
    here, it is also used by PERQdisk (a POS filesystem interrogator and file
    extractor) and will soon/eventually be integrated into PERQfloppy (RT-11
    floppy utility) and Stut (which reads PERQ "STUT" QIC tapes).  Bunch o'
    docs included in the PERQmedia project folder.

    2. The first two storage devices have been converted to work with the new
    PERQmedia StorageDevice class.  Now Shugart SA4000-series hard drives and
    the SA851 floppy drive are split in half:  the "mechanical" operation and
    block-level access to the data are in HardDisk and FloppyDisk classes,
    while the ShugartDiskController and NECuPD765 (floppy disk controller)
    provide the register-level interface to the PERQ and Z80 respectively.

    3. Disk access is actually frighteningly realistic now.  The Z80 CTC,
    HardDiskSeekControl and Scheduler classes were all updated to allow the
    actual Z80 ROM code to drive the emulated disk drive using "buffered"
    seeks.  To be truly perverse, PERQemu will even mimic the startup delay
    of each drive type -- but after the first time you wait 90 actual seconds
    for a simulated disk to "spin up" you'll want to turn off that option too.
    :-)

    4. An extensible database of supported media types is loaded at runtime
    so the groundwork is there to allow all of the PERQ-2 disk types to be
    created, formatted and mounted.  Soon...

    5. There is a ton of work to do to make the new threading approach more
    robust, and completely rethink the way the main application loop works.
    While I flirted with 40-42fps performance in AoT/optimized Release mode
    builds, having to introduce locking in the Scheduler was a hit; still,
    my old Mac is consistently running at 37fps, up from ~30fps in the old
    WinForms version.  Yes, it has to go much, much faster.  Or you can try
    it on a machine that's less than a decade old.


    February, 2022:

    Lots of stuff in progress.  The goal is to get the restructured Emulator
    at least back to the level of stability as the current master branch
    before attempting to sync "experiments" back up.  Lots of plumbing to do.
    GPIB/Tablet support is a big hurdle since older OSes require the BitPad;
    working out all the kinks in the ExecutionController so that virtual
    machines can be reliably and accurately managed is top priority.  Adding
    the CIO option (executing the new Z80 code from an actual ROM dump) gives
    us the ability to finally let PERQ-1s run all the newer OSes and software!

    PERQ-2 support will then follow, gracefully and effortlessly.  bwaaaahahahahahahaha no stop it hahahaha sigh.

    1. Worked the Debugger and :var syntax back in, and integrated it with the
    new CLI.  Still some work to do to make it more friendly - like adding tab
    completion on the variable list?  Additional items on the to-do list for
    the CLI are to add global commands, pathname expansion, and a more dynamic
    way to expand the parse tree so that certain string variables can be used
    like enumerated types but updated at runtime).

    2. Hard and floppy disk support is stable again, and feeling more solid.
    May have to implement the Shugart seek ramp timing to match the Z80's
    crazy built-in timing scheme to avoid the occasional disk timeout (but
    they are mostly evident on my slow machine, are not fatal, and just add
    the authenticity of the PERQ experience!).

    3. GPIB controller and bus rewritten to work with the new Z80, and the
    BitPadOne tablet is working again.  A few rough edges and debugging to do.
    (The Kriz tablet is much more efficient but to run Accent S4 or POS F.0 or
    older OSes the BitPad is required.)

    4. All of the hard/floppy disk loading, unloading and saving commands are
    being cleaned up and revamped to work with the new storage architecture.
    A simple search path is implemented to save typing -- "load foo" will look
    for foo, Disks/foo, or apply various file known filename extensions.  New
    commands to create blank disks or even describe the geometry and specs for
    entirely new types of drives are included for advanced users.

    5. Configurations are now loaded from the Conf/ directory, and a number of
    "prefab" systems are included there.  Similar path rules apply so that
    "configure load foo" will find Conf/foo.cfg.  You can assign media files
    to disk drives and save them in the configuration so that the config can
    be loaded and run directly.  In addition, the "preferences" file will
    remember the last configuration you ran and load that by default the next
    time you start PERQemu.

    6. Believe it or not, this Readme has been trimmed substantially to create
    a new UserGuide.  This wad of verbiage will be eventually pruned, rolled
    into a history/change log...
    

Original Readme.txt follows.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~


1.0 Introduction
================

PERQemu is an attempt to emulate the venerable Three Rivers Computer
Corporation (3RCC) PERQ-1A system.

Emulating this system has been an immense challenge, but so far it's been a lot
of fun...  See section 4.0 to see what works and what doesn't.

PERQemu is written in C# under the .NET Framework 2.0.  The source code is now
available on GitHub at https://github.com/jdersch/PERQemu/.  I've been working
on it on and off since August of 2006.  Thanks for taking a look at it.

Thanks,
- J. Dersch


1.1 A Little PERQ history
-------------------------

The PERQ is an early microcoded graphical workstation with a high-resolution
bitmapped display and is arguably one of the first commercially-available
"workstation" class of computers to meet the "3 M's" criteria: a megabyte of
memory, a million pixel display, and a MIP of processing power.  Heavily
influenced by the Xerox PARC "D-machines" and taking inspiration from the DEC
PDP-11 for some of its performance tricks, it was designed and demonstrated
in 1979 but not released in production quantities until late 1980.  It is
estimated that fewer than 5,000 PERQs were built, mostly sold to universities.

The PERQ-1 hardware consisted of the following:

    - A custom bit-slice, microcoded CPU with 48-bit microinstruction words
    - 4K (PERQ-1) or 16K (PERQ-1A) of writable control store
    - 512KB to 2MB of RAM (in 16-bit words)
    - A high resolution bitmapped display at 768x1024 pixels (1bpp)
    - Custom RasterOp hardware to accelerate bitmap operations

The original PERQs featured a standard set of peripherals:

    - A 12 or 24mb Shugart 4000-series hard disk (14" platters)
    - A single 8" Shugart floppy drive
    - A GPIB interface, typically used to interface with a Summagraphics
      BitPadOne digitizer tablet
    - One programmable RS232 port, up to 9600 baud
    - A CVSD chip for audio output

Optional IO boards could be fitted, which provided:

    - 3Mbit or 10Mbit Ethernet interfaces
    - Canon LBP-10 (and later Canon CX) laser printer interface
    - QIC streamer tape connection

A later PERQ-2 series extended the original design in some significant ways
and added a number of additional IO options.


1.2 Current Status
------------------

PERQemu versions through 0.4.5 focused exclusively on PERQ-1 support.  Before
the Great Refactoring (v0.5.0 and beyond) the WinForms-based emulator could run
only the PERQ-1 "old" Z80 with a single Shugart hard disk.  While the emulation
was fairly complete, the options for OS and software to run were limited.

This current experimental branch is undergoing major structural changes to
allow all of the PERQ-1 and PERQ-2 models to be emulated.  This includes the
"new" Z80 (enabling PERQ-1 models to run newer OS versions) and support for all
of the PERQ-2 models as well.  As soon as the experimental branch is stable and
can reliably run the PERQ-1 as before, it will be merged back into "master" and
released.  Ongoing development will bring additional releases as new models,
features and peripherals are incorporated.  Please check back often for updates!


1.3 System Requirements
-----------------------

You will need a Windows machine with the .NET Framework ?.? installed, or
a Linux/UNIX/Mac OS machine with the Mono v?.? runtime installed.

    [Todo: Figure out exactly what versions are required to support the
    .NET Framework 4.8 target we build against.]

PERQemu is a nearly cycle-accurate, register-level emulation of a complex 
microcoded processor AND a Z80 subsystem -- essentially two emulations running
side-by-side.  Your computer should have a reasonably current processor with at
least 2-4 CPU cores to run the emulator at speed.  While a CPU hog, PERQemu's
memory footprint is very modest, requiring less than 100MB of RAM with a typical
24MB Shugart hard drive configured.

To gauge how faithfully your computer is able to emulate the PERQ, the title of
the display window updates every few seconds to report the frame rate ("fps")
and the average cycle time of the PERQ and Z80 processors.  At full speed the
PERQ will display 60fps, with the CPU executing at 170ns and the Z80 at 407ns.

Performance tuning is an ongoing concern.


2.0 Getting Started
===================

This is a very basic "quick start" guide.  For more information about running
the emulator, please consult the UserGuide.txt included in this distribution.

    [On-line help and/or a nicely formatted PDF will be forthcoming.]

If you've gotten this far you've unpacked the Zip archive and you have a 
directory containing the emulator executable, PERQemu.exe.

There are several subdirectories:

    Conf/
        Contains a collection of "prefab" system configurations as well as
        device data required for operation.  By default, all custom PERQ
        configurations are also saved in and loaded from this directory.

    Disks/
        Contains disk images that the emulator can access.  Included with
        the distribution are:

        d6.phd:
            A dump from my very own PERQ1's disk, which has POS D.61 and
            Accent S4 installed.
              
        f1.phd:
            A disk containing a pretty complete installation of POS F.1,
            including source, documentation, the Pascal compiler and a
            number of games, demos, and applications.  This was created
            from floppy images on Bitsavers.

       Additional "stock" hard drive or floppy images may be included as
       well.  Any custom disk images you create or import are loaded from
       and saved in the Disks/ directory by default.  Please consult the
       UserGuide for information about working the PERQemu media files.

    PROM/
        Contains dumps of PERQ ROMs necessary for operation.

        
To start the emulator, just run PERQemu.exe:

    Windows: double-click the icon.  PERQemu is a "console application,"
        so a command window will appear and you'll be at the command prompt.

    Unix/Linux/Mac: invoke "mono PERQemu.exe" from the command-line in a
        terminal window.  The PERQemu debugger will announce itself, same
        as in the Windows version.

The top-level command prompt is a single '>'.  The command-line interface (CLI)
now organizes the extensive command set into a hierarchical set of "subsystems".
The prompt will change according to the current level in the hierarchy, such as:

    configure>
or
    settings>

The CLI provides extensive prompting through tab completion and other on-line
help.  Consult the UserGuide for help navigating and running commands.

The first time you run PERQemu, the default configuration is selected.  The
default is a PERQ-1A with the POS F.1 image already assigned.  This default
configuration is the only one built-in to the emulator; all the rest are loaded
from the Conf/ directory.

To start the emulation, type the shortcut:

    > go

At this point the PERQ will "power on," the Display window will appear and the
virtual machine will load the f1.phd hard disk image and start executing.
Eventually you'll be greeted with the POS login prompt.

As with previous versions of PERQemu, pressing ^C will pause the running PERQ.
Unlike the older command interface, however, the CLI now remains active even
when the emulator is running!  You may type "stop" or other commands to control
or interact with the emulator.  Consult the UserGuide for more information.

To shut down the PERQ, you can simply close the Display window.  The emulator
will report that the machine is shutting down and return you to the prompt.
You may also type the commands:

    > stop

to pause, or just

    > power off

to immediately halt and "power down" the virtual machine.  If you are in a
real hurry:

    > quit

will exit the program, shutting down the machine if it is running.  Note that
PERQemu does NOT automatically save any disk images that you load, so if you
want to save changes to the hard drive or floppy disk images you should do so
manually before issuing the "power off" or quit commands.  However, the new
"settings" subsystem offers a set of preferences to making saving automatic.

There is a LOT more to explore in PERQemu now, with dynamic configuration, new
storage devices, extended debugging, user preference settings, and more on the
way.  Say it with me now: "Consult the UserGuide for more information!"


2.1 Operating System Support
----------------------------

As of v0.4.2, the following PERQ operating systems are known to boot:

  - POS versions D.6, F.0 and F.1 (official 3RCC releases);
  - POS version F.15 (released by Boondoggle Heavy Industries, Ltd);
  - MPOS version E.29 (unreleased by 3RCC);
  - Accent S4 (an early version from CMU, unreleased);
  - PNX 1.3 (first public release by ICL).

NOTE: PNX drops into its microcode debugger (i.e., crashes) after booting if
2MB of memory is configured; it runs fine with 1MB.  POS and Accent have no
trouble with a full megaword of memory.

Accent S4 mouse tracking takes a little getting used to since it runs in
relative mode.  To simulate mouse "swipes" you have to use the Alt key (Command
key on Mac) to tell PERQemu the mouse is "off tablet", reposition, then release
the key to start tracking again.  It's a little clumsy at first.

PERQ FLEX is an exceedingly rare OS produced in the UK by the Royal Signals &
Radar Establishment.  Based on a custom instruction set optimized for Algol-68,
efforts are underway to recover several disk images and preserve what promises
to be a unique bit of computing history.  If successful this will provide the
first real 8" Micropolis hard disk image to test with once PERQ-2 emulation
features are complete.

If anyone has any other software that ran on the PERQ-1 and does not run
successfully under PERQemu, send us a copy and we'll find out why!


3.0 What's Implemented
======================

The following hardware has been implemented in the emulator:

  Processors:
    - 4K and 16K CPUs (20-bit) are tested and complete;
    - 16K CPU (24-bit) is complete but not tested; waiting on PERQ-2/EIO;
    - The CPU, memory and video run on a separate thread.

  Memory/VideoController:
    - Now can be configured at runtime, up to 8MB in the 24-bit models;
    - Only tested for operation with the 20-bit processors (max 2MB / 1MW).

  Hard disk:
    - All of the disk support has been completely rewritten to prepare for
      the addition of PERQ-2 emulation.  Currently only the original PERQ-1
      14" Shugart SA4000-series drives and controllers are tested to work with
      the new Z80 implementation (running the original ROMs);
    - All of the suppoted 8" Micropolis and 5.25" MFM drives will be available 
      as PERQ-2 support is introduced.
      
  Floppy disk:
    - Also rewritten to work with the new Z80 and floppy disk controller (FDC);
    - Supports dynamic loading and unloading of all media types (single- and
      double-sided diskettes, in single- and double-density);
    - There are still a few bugs; testing and debugging is ongoing.

  Displays:
    - The standard 768 x 1024 portrait display is available for all models;
    - The 1280 x 1024 landscape display is not yet supported by any OS that
      runs now (with the old Z80 ROMs);  Landscape will be tested as soon as
      the "new" Z80 ROM code / PERQ-2 models are available and versions of
      POS, PNX and Accent which support it are running;
    - Now rendered by SDL2, so 32-bit WinForms is finally retired.

  Z80 I/O Processor:
    - Simulation replaced by a real Z80 emulator running actual PERQ ROM code;
    - Runs asynchronously on its own thread to improve performance;
    - Will allow different ROMs to be loaded to support CIO and EIO boards,
      with the ability to support ZBoot loading (Z80 code dynamically loaded
      by the PERQ at runtime);
    - New register-level interface written to support Z80 DMA, CTC, SIO, FDC
      and GPIB controller chips;
    - Z80 Debugger support includes single stepping and source code display
      (for the current v8.7 ROMs).

  Keyboard:
    - Now uses the SDL2 interface so no more horrible hacks required for MacOS;
    - Support for the VT100-style PERQ-2 keyboard is now included but can't be
      tested until EIO support is complete.
    - Currently caps lock is problematic and can get out of sync with the host.
      This is a minor inconvenience but it's on the bug list.  [TODO: check if
      this is still the case under SDL2.]

  RS-232:
    - The Z80 SIO chip is implemented to work with the new Z80 emulator;
    - Software running under emulation should/will be able to control a real
      physical serial port on the host, but this has not been tested yet with
      the new Z80 code, and Configurator support to assign the host device is
      not yet available.

  GPIB:
    - The TMS9914 controller chip is implemented to work with the new Z80,
      but it is very fresh and still incomplete and occasionally loses the plot;
    - Supports basic System Controller, Talker and Listener features, but just
      enough to support what the PERQ needs.  Being able to drive a real GPIB
      card in the host computer would be pretty darn cool but I wouldn't hold
      my breath on that one.

  Tablets:
    - The proprietary Kriz tablet works with the new SIO chip; it is only
      useful on POS F.1 and later (no support in D.6, F.0, PNX 1, or Accent S4);
    - The simulated Summagraphics BitPadOne works with the new GPIB; it is
      supported on all PERQ OSes;
    - The BitPad/new GPIB are buggy and occasionally get unplugged/out of sync;
      the Kriz is a far more efficient tablet and is generally preferred on
      any OS that supports it!


There is a ton of additional detail about the internals of PERQemu itself in
the source distribution.  See Readme-source.txt, or the copious notes in the
Docs/ directory for way, way more information than you need.  Way more.


4.1 What's Not
--------------
 
- Ethernet.  Unimplemented, but on the list!

- Option boards:  Canon laser, streamer tape.  On the list.
 
- PERQLink.  Unimplemented other than a stub that tells the microcode that
  there's nothing connected to it.
 
- Sound.  Implemented, but not hooked up to any sort of host output device.

- Multibus option and SMD disk/9-track tape support.  Dream on!

- A proper GUI.  Sigh.

- Some debugger commands are planned/in development but are not documented
  or complete; support for breakpoints, logging to file (in addition to or
  instead of the console), and several CLI enhancements are unfinished.


5.0 History and Roadmap
=======================

v0.7 - TBD
  Additional I/O Options once the baseline devices are complete:
  - Ethernet!
  - Canon laser printer
  - QIC streamer tape
  - Working audio output :-)

v0.6 - TBD
  Leverage the new architecture to roll out new models, new peripherals and
  open up the full range of available operating systems!
  - PERQ-1 CIO (new Z80) support: updated to run new Z80 ROMs
  - PERQ-2 EIO emulation support: expanded IO Board with faster Z80, second
    serial port, RTC chip, support for two hard disks
  - PERQ-2 peripherals: 8" and 5.25" disk drives, VT100-style keyboard,
    landscape display option, 24-bit "T4" model with larger memory
  - Get screenshots working again

v0.5 - TBD
  Merge the experiments back into the master branch once the new Z80 and all
  of the new features are reasonably stable:
  - True Z80 emulation
  - Runs on 64-bit Mono/MacOS (no 32-bit WinForms limitation)
  - SDL2 for improved display performance, in theory :-|
  - Unified PERQ media storage architecture and file format
  - Dynamic runtime configuration of all PERQ models and features
  - Enhanced command line interface with more prompts, in-line help
  - Persistent user preference settings

v0.4.6 - Current "experimental" branch
  - This version, in progress

v0.4.5beta - Unreleased
  - This was a one-off build for VCF PNW with some experimental video hacks
    to improve display performance.

v0.4.4 - Fourth major release
  - RasterOp streamlining and refinements allowed us to remove the
    "microcode bailed early" hack.  Sweet.
  - New and updated disk images for release.
  - Numerous documentation updates to support first Github tagged release.

v0.4.3 - Accent update
  - Massive one-byte change to get mouse tracking to work in Accent S4.
  - Expanded GPIB implementation to support talker/listener selection, 
    can now read and write commands and data for multiple bus targets beyond
    just the Bitpad.
  - Z80 ready/busy status reworked, and other small fixes and streamlining.
  - VideoController streamlined and timing glitch fixed to correct visual
    artifacting during vertical blanking.

v0.4.2.12 - Major Memory update
  - Rewrote the Memory implementation again, to improve performance.
  - Other small enhancements and bug fixes.

v0.4.2 - Major RasterOp update
  - Extensive reimplementation of the RasterOp pipeline; removed the "fake"
    RasterOp code entirely.
  - Additional debugging support added.

v0.4.0 - Development update
  - Completed 16K mul/divstep hardware.
  - First reimplementation of the "real RasterOp" pipeline.
  - Memory changes to support new Rasterop.
  - Platform-independent Keyboard rewrite, no more P/Invokes.
  - Somewhere between v0.3 and v0.4 is the snapshot uploaded to Github.
  - The state save/restore serialization code removed.

v0.30 - Third major release
  - Added 16K CPU support.
  - Rolled up changes for first source code release.

v0.25 - Interim release
  - Added 16K CPU support (minus mul/divstep hardware)
  - "Real" RasterOp emulation is 50% completed, still not working correctly.
  - Additional performance improvements due to caching, I/O clocking changes, and
    optimizations to the ALU.
  - Refactored debugger code, and added functionality for modifying PERQ registers
    and loading microcode
  - Fixed bug in EStack implementation that prevented PERQMan from running properly.
    (PERQMan RNG uCode leaves an extra entry on the stack with every invocation,
    which eventually overflows the eStack.  On a real PERQ this wraps around, my
    implementation was clipping the pointer into range.  This caused odd errors.)
  - Added special RSX: hook. 
  - Fixed keyboard handling: Now Ctrl+Shift+<key> registers properly.
  - Changed display update code to work around a Mono WinForms implementation issue.
    Should run without the display thread crashing under Mono now.

v0.21 - Minor update
  - Fixed small issues preventing PERQemu from running well under Mono.  Should
    run acceptably now!
  - Added some very hacky support for double-density floppy images.
  - Some minor optimizations to the CPU code result in a 5% performance gain.  

v0.2 - Second release
  - Z80 simulation is mostly complete, along with most subdevices:
    - RS232 is 99% done and will talk to a real serial port on the host machine
    - Kriz Tablet is 99% done and works sufficiently well to allow applications 
      that use the pointer to function.
    - Speech support is there but output is not hooked up to the host.    
    - GPIB support is only a stub.
    - Floppy disk support is 80% done and supports all of the functionality
      needed to deal with double-sided, single-density floppy images.
    - Keyboard support is 100% done.
      
 - Hardware cursor implementation refined, vertical and horizontal positioning
   are now correct.  Vertical positioning occasionally is thrown off by a few
   pixels.
   
 - A few minor speed improvements (nothing major) gained from only clocking the
   Z80 devices every 16 PERQ cpu clocks.
   
 - Floppy disk R/W support added, Hard disk Write support added.
 
 - Debugger commands for loading/saving and creating disk images for hard disks
   and floppies added.

v0.1 - First public release.

