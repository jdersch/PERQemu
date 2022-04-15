PERQemu Readme

4/13/2022 - skeezicsb - v0.4.6 (experimental)
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

    A log of the changes I have made here has been moved to ChangeLog.txt,
    which may (or may not) be included going forward as a running commentary
    on major updates as they happen.


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

You will need a Windows machine with the .NET Framework 4.8 installed, or
a Linux/UNIX/Mac OS machine with the Mono v6.12.0.x runtime installed.

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
       UserGuide for information about working with PERQemu media files.

    PROM/
        Contains dumps of PERQ ROMs necessary for operation.

    Output/
        When logging debug output to disk is enabled, those files go here by
        default.  When screenshots and printing are implemented, that output
        will land here too.  (Output directory will be a settable preference.)


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
      the new Z80 implementation;
    - All of the suppoted 8" Micropolis and 5.25" MFM drives will be available 
      as PERQ-2 support is introduced.
      
  Floppy disk:
    - Rewritten to work with the new Z80 and floppy disk controller (FDC);
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
    - Allows different ROMs to be loaded to support CIO and EIO boards;
    - New register-level interface written to support Z80 DMA, CTC, SIO, FDC
      and GPIB controller chips;
    - Z80 Debugger support includes single stepping and source code display
      (for the current v8.7 ROMs; v10.17 source disassembly in progress).

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
      physical serial port on the host, but this has not yet been rewritten to
      work with the new Z80 code.  Settings and Configurator support to assign
      the host device path and enable the port is in place, however;
    - The nifty RSX: pseudo-device for transferring text files from the host to
      POS will be reinstated too.

  GPIB:
    - The TMS9914 controller chip is implemented to work with the new Z80, but
      it is still incomplete and occasionally seems to confuse POS (reporting
      non-fatal errors that don't seem to negatively affect operation);
    - Supports basic System Controller, Talker and Listener features, but just
      enough to support what the PERQ needs.  Being able to drive a real GPIB
      card in the host computer would be pretty darn cool but I wouldn't hold
      my breath on that one.

  Tablets:
    - The proprietary Kriz tablet works with the new SIO chip; it is only
      useful on POS F.1 and later (no support in D.6, F.0, PNX 1, or Accent S4);
    - The simulated Summagraphics BitPadOne works with the new GPIB; it is
      supported on all PERQ OSes;
    - The BitPad/GPIB requires a ton of processing due to protocol overhead;
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
 
- Sound.  Yet to be rewritten to work with the new Z80/SIO and hooked up to
  any sort of host output device.

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
  - PERQ-2 EIO emulation support: expanded IO Board with faster Z80, second
    serial port, RTC chip, support for two hard disks
  - PERQ-2 peripherals: 8" and 5.25" disk drives, VT100-style keyboard,
    landscape display option, 24-bit "T4" model with larger memory
  - Get screenshots working again

v0.5 - TBD
  Merge the experiments back into the master branch once the new Z80 and all
  of the new features are reasonably stable:
  - True Z80 emulation
  - PERQ-1 CIO (new Z80) support: updated to run new Z80 ROMs
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

