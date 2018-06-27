PERQemu Readme

6/24/2018 - skeezicsb - v0.4 - v0.4.4
6/24/2010 - jdersch - v0.1 - v0.3


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
    - A CVSD chip for audio output (and input)

Optional IO boards could be fitted, which provided:

    - 3Mbit or 10Mbit Ethernet interfaces
    - Canon LBP-10 (and later Canon CX) laser printer interface
    - QIC streamer tape connection

A later PERQ-2 series extended the original design in some significant ways
and added a number of additional IO options.  At this stage PERQemu does not
yet incorporate those enhancements.


2.0 Getting Started
===================

You will need a Windows machine with the .NET Framework 2.0 installed, or
a Linux/UNIX/Mac OS machine with Mono installed.  It helps if the machine 
is quite fast as the emulation is currently not particularly swift.

If you've gotten this far you've unpacked the Zip archive and you have a 
directory containing the emulator executable, PERQemu.exe.

There are three subdirectories:

    Disks - Contains disk images that the emulator can access:

        - d6.phd: A dump from my very own PERQ1's disk, which has 
              POS D.61 and Accent S4 installed.
              
        - f1.phd: A disk containing a more or less complete installation
              of POS F.1, including source, documentation, the Pascal
              compiler and a number of games, demos, and applications.
              This was created from floppy images on Bitsavers.

        - f15.phd: A disk containing the first new POS release in
              over 30 years, built from scratch from a comprehensive
              source review and rebuild of POS F.1, but with several
              enhancements (and a roadmap for the future).  This disk
              is a typical "binary only" deployment with all of the
              games, demos and several apps.

        - f15dev.phd: Same as f15.phd, but with all of the source
              code and development tools included.  A fascinating look
              at a complete operating system from the 1980s.

    Floppies - Currently contains all of the floppy images for POS F.15.

    PROM  - Contains dumps of PERQ ROMs necessary for operation.

        
To start the emulator, just run PERQemu.exe:

    Windows: double-click the icon.  PERQemu is a "console application,"
        so a command window will appear and you'll be at the debugger prompt.

    Unix/Linux/Mac: invoke "mono PERQemu.exe" from the command-line in a
        terminal window.  The PERQemu debugger will announce itself, same
        as in the Windows version.


To load a disk image, type "load harddisk" or "load floppy" followed by a
pathname to the image file.  Use the pathname syntax appropriate for your
platform.  (You can boot from a POS "boot floppy" but as on the real hardware,
floppy boots are slow!)  PERQemu will report "Loaded." if the disk image file
is read in successfully.

The PERQ could boot from several sources, or use different boots from one
hard disk image.  Lower case letters select a normal hard disk boot; upper
case letters select the floppy.  You can boot the "old fashioned way" by
waiting for the VFY patterns to appear and hitting the appropriate key, or
tell PERQemu to do it for you by typing "set bootchar <c>" where <c> is the
boot you wish to select.  The default boot letter is 'a'.

Hit "g" or type "go" and hit enter and the emulation will be off and running!
A large display window will appear, and soon you should see a strange set of
comb-like patterns displayed as the microcode runs through its memory tests
and other diagnostics.

Note that the PERQ display is a fixed 768 x 1024 in a portrait orientation
and PERQemu does not decorate the window with scroll bars or support any
full-screen modes.  The mouse wheel or the PgUp/PgDn keys may be used to jump
the PERQ display up or down if it doesn't fit your display.


2.1 Booting and the DDS
-----------------------

The debugger console window's title will read "DDS" followed by a three-digit
number:  this is the PERQ's "diagnostic display," where the progress of the
boot sequence may be monitored.  Note that the DDS is specific to the operating
system being booted!  When the boot is complete, the DDS will typically show:

    PNX:    255
    POS:    999
    Accent: 400 (1MB of memory) or 450 (2MB)

The PERQ does its best to fool you into thinking it's broken, showing wierd
noise or patterns on the screen until it finally enables the video interrupt
and the OS takes over the display.  Because we don't emulate the 2-3 minute
"warm-up" time that the old Shugart hard drives required, most hard disk boots
will complete in just a few seconds.  If your PERQ really is stuck, consult
the "Fault Dictionary" in the Bitsavers on-line documentation repository (see
below) or in the PERQemu source Docs directory on GitHub.


2.2 POS
-------

Once POS has booted, enter the date and login with user "guest" and a blank
password (or alternately just hit "enter" at the login prompt.)  You'll need
to put focus on the display window (click on it) so that the keyboard input
goes to the PERQ and not the debugger.

The mouse works approximately as you'd expect with one minor difference, due
to functional differences between a digitizer tablet and a mouse.  Specifically:
a digitizer knows when the puck is off the tablet, where a mouse does not.
Normally this is not an issue, but if you need to simulate the "off the tablet"
behavior, hold down the "Alt" key (Command key on the Mac).

Full POS documentation can be found on Bitsavers at

    http://www.bitsavers.org/pdf/perq/PERQ_SysSWRefManual_Feb1982.pdf
    
(also in sys:user>doc> in the F.1 hard disk image, or :boot>docs> in F.15)
    
Interaction with POS is through the command-line interpreter called the
Shell.  Here are some basic commands to get you started:

    ? - list the commands defined in your "login profile"

    HELP - shows usage for various commands

    DIR - show contents of the current directory.  Executable files are
        suffixed with ".RUN" and directories are suffixed with ".DR".

    PATH (or CD) - change directory.  More or less like your Unix or DOS
        equivalent.  Note that the directory delimiter is ">" (this may
        freak out Unix heads! :-)
        CD foo>bar>baz> - change to directory foo>bar>baz
        CD ..           - change to parent directory
        CD sys:user>    - change to root of user partition

    TYPE - display a file on the screen, similar to "more" on modern OSes.
            
    COMPILE - run the Pascal compiler.  Produces a .SEG file which must be
        linked.

    LINK - runs the linker.  Produces a ".RUN" file which may be executed.

    COPY - copies a file from one location to another.  RSX: is a special
        file which can be used to transfer files to/from the emulator host
        machine.  See section 3.0 for more details.

    EDITOR - a fairly simple text editor (see also PEPPER, a more Emacs-like
        editor, available in most of the POS images)

Executable files end with ".RUN", just type the name (minus .RUN) to start
them.  Switches are prefaced by '/' as in most OSes of the day.  Commands
and switches are not generally case-sensitive, and can usually be abbreviated
as long as they are not ambiguous.  Most commands in POS will prompt you for
arguments if you don't provide them, and most commands won't do anything
harmful without asking you first.

In general, "Ctrl+c" twice will abort a running program, unless it's hung.

Note that control characters on the PERQ are actually case-sensitive!  A single
Ctrl-c is an interrupt; a second Ctrl-c signals an abort.  A Ctrl-Shift-C is
used to break out of command files and return you to the Shell.  The Pepper
editor makes extensive use of shifted control characters to make up for the
lack of a "meta" key that Emacs requires.  Quirky, no?

There are games under sys:user>games>, demos under sys:user>demo>, various 
utilities and OS source are under sys:boot>.  There are user directories 
(from the original users of this machine at Siemens!) under sys:>user> in the 
D.6 image that contain some interesting utilities.  Things are arranged a
little bit differently in POS F.15; "type welcome.txt" to learn more when you
first log in.

You can log out of POS by typing "bye" to the Shell.  To log out and power
down, type "bye off" or "bye /off" and POS will tidy up and power off the
machine.  In the case of PERQemu, that means returning you to the debugger.
Note that after the emulated PERQ powers itself off you must type "reset" to
reboot it.

REMEMBER:  PERQemu does not save modifications to the hard or floppy disks
automatically; if you accidentally blow up the disk image, just exit and
restart or reload the image - no harm done!  If you DO want to save your
changes, type "save harddisk <filename>" or "save floppy <filename>" before
you exit.


2.3 Accent
----------

In the included D.6 disk image, an early release of the Accent OS from CMU
is available too.  Accent is the forerunner of Mach, the kernel which was
the basis for NeXTstep, which became MacOS X.  This version, S4, is an
amazing and rare find, as it pre-dates the official S5 and S6 releases from
Three Rivers/PERQ Systems in late 1984-early 1985.

To boot Accent, either type "set bootchar z" or wait for the DDS to read 151
and hit the z key.  Accent's startup sequence is different, and this early
version of Accent isn't nearly as fast as POS; after prompting you to enter
the date and time, starting the servers, starting up the window manager and
running through all the initial command files takes a bit longer.  But once
things are initialized, Accent's shell behaves very much like the POS Shell,
with most of the same commands -- and a few exceptions:

    - windows act more like folks today expect them to, though it's a bit
      clumsy manipulating them at first
    - you have to click to select a "listener" window to direct where your
      typing will go -- the listener window has a grey border around it
    - you can use the basic Emacs-like control keys to retrieve previously
      typed commands and edit them (^p/^n for previous/next, ^f/^b forward/
      back, etc)
    - you can scroll back to see text that scrolled off the screen (^V/^v
      for backward/forward by roughly a page full of text)
    - interrupting or aborting the current program goes through the window
      manager, so ^c or ^C doesn't work as in POS

SAPPHIRE, the window manager, has a bunch of commands and some on-line help.
All window manager commands are prefixed by Ctrl-Del, then a letter.  Until
you're familiar with it, Ctrl-Del h for help brings up a command summary.
There are also pop-up menus, and the icons change depending on what SAPPHIRE
wants you to do (make a window, move a window, etc).  To interrupt a program
in Accent, use Ctrl-Del c, or Ctrl-Del k for added oomph.

Note that the mouse in Accent is in relative mode, which is awkward to emulate
with a mouse; you can hold the "off tablet" key (Windows: Alt, Mac: Command)
and relocate the host's mouse, then release.  The PERQ will compute relative
movement based on the direction of the next swipe, not the absolute coordinates
in the window.  You'll catch on with a few tries. :-)

Button, button, who's got the button?  Accent S4 only enables the 4-button
BitPad puck, though it maps the buttons to match the 3-button Kriz puck.  If
you have a 3-button mouse, left-middle-right should be mapped appropriately.
For a two-button mouse, Ctrl-Left simulates the third button; Ctrl-Right
simulates the fourth PERQ button.  If you are using an old one-button Apple
mouse, I just don't know what to tell ya...

There is no clean way to log out of Accent, so the best way to make sure you
flush buffers to disk (if you want to save your work) is to type "trap" at
the Shell.  This will bring up the kernel debugger which will scribble in
reverse video all over your screen.  From there type 'y' to exit to the pager
process, and at that point there's no going back; switch to the PERQemu
debugger window and hit Ctrl-c to stop execution, save or reset as desired.


2.4 PNX
-------

Unfortunately, we do not yet have a complete PNX 1.0 hard disk image.  It is
possible to boot from floppy, prepare the hard disk, and lay down the boot-
strap (essentially a "miniroot" in Unix parlance) which is bootable.  But the
other floppies in the Bitsavers collection are missing track 0, and the rest
of the PNX loading process cannot be completed at this time.  I hope to remedy
this very soon.

PNX, another unfortunately named PERQ operating system, was a very early Unix
V7/System III mashup that included an in-kernel window manager - possibly one
of the first Unix variants to do so.  This was primarily run on PERQs in the
UK, where PNX was developed by ICL.  Because the on-disk format is based on
the Unix filesystem, it cannot co-reside on a disk with POS or Accent (which
share a common underlying filesystem layout).  PNX also used its own language
interpreter, running an instruction set more favorable to C than the original
PERQ Q-codes.  However, PERQemu's debugger cannot accurately disassemble PNX
C-codes, since we don't currently have access to any PNX source code or
documentation.

Watch this space.


3.0 Debugger Operations
=======================

Hit Ctrl+C when the debugger window has focus to break into the debugger at any 
time.  Hit "Tab" to see a list of command completions.  You do not need to type 
in an entire command to run it, only enough to disambiguate it from other
commands.  For example "l h" is short for "load harddisk" and "d m" is short
for "disassemble microcode".

"Tab" will always show completions at any point during input.  For example,
if you type "show" and hit tab, you will be presented with a list of possible
completions for the "show" command.  Commands are not case-sensitive, though
arguments may be (such as filenames, if your filesystem respects case).

The debugger is a work-in-progress and some commands may be unintuitive or
unfinished.  The more useful (and implemented) ones are:

    GO - begins emulation execution from the current microcode PC.

    RESET - resets the virtual PERQ.  Disk images are NOT reloaded, which makes
        this useful for rebooting the PERQ when installing an OS, for example.

    SET BOOTCHAR <char> - helper to make it easier to boot from an alternate
        boot.  The D.6 image has two available boots: 'a' and 'z'.  'a' is the
        default (POS), 'z' is Accent S4.                   

    LOAD FLOPPY/HARDDISK <image> - loads a given hard disk or floppy image into
        memory.
                     
    SAVE FLOPPY/HARDDISK <image> - saves the current in-memory hard disk or
        floppy to the specified image file.
                     
    CREATE FLOPPY/HARDDISK - creates a blank disk image in memory.

    SAVE SCREENSHOT <name> - saves a JPEG image of the current PERQ display.

    SET RS232 <port name> - selects the serial port on the host machine to use
        for the emulated PERQ.  "RSX:" is a special port which allows transfer
        of files to/from the emulation host.  If RSX: is selected as the port,
        from POS one can "COPY RSX:c:\path\to\hostfile.pas sys:foo>bar>baz.pas"
        and the file will be copied from the host OS to the emulated PERQ; this
        works for text (7-bit ASCII) only.  (It's also quite slow, since it has
        to run at serial speeds).

In the Debug build, there are a huge number of additional commands to inspect
the state of the emulator and assist with debugging PERQ microcode.  These
are not yet documented.


3.1 Disk Operations
-------------------

The basic thing to bear in mind when dealing with PERQemu and disk images is
that PERQemu only makes changes to in-memory copies of the images.  That is:
if you make changes to the disk, you must MANUALLY dump the image back to an
image file when you are done or your changes WILL BE LOST when you exit the 
emulator or load another image.

Floppy disk images must be in RAW format (I am working on implementing DMK
support).

Hard disk images are in my extremely primitive dump format (which I intend
to improve and document).  Currently only images of the 24mb Shugart drive are
supported (mostly because I have so few images of other disks to work with...)

To load a disk image, use the LOAD FLOPPY/LOAD HARDDISK command from the 
debugger.

To save a disk image, use the SAVE FLOPPY/SAVE HARDDISK command (go figure).
You (currently) have to specify the pathname each time you save.

To create a blank disk, use the CREATE FLOPPY/CREATE HARDDISK command.  This
creates a blank, in-memory image of a disk.  You MUST save this image if you
want to use it again.

Note that in the current builds, disks are way, way faster than the actual
hardware, so it's less of a chore to format a floppy or partition a Shugart
hard disk than IRL.  Maybe in future releases we'll offer the 100% authentic
83ms access times and the kerchunk-kerchunk-kachunk audio clips of the SA851
floppy drive banging away on floppy boots... :-)


4.0 What's Implemented
======================

The following hardware has been implemented in the emulator to an extent that
it is reasonably functional (though there may still be issues):

- PERQ 4K CPU - Complete.  Everything seems to work, all diagnostics pass
  and it works enough to allow POS to boot.
  
- PERQ 16K CPU - Complete.  Mulstep/Divstep is implemented but not
  exhaustively tested.  Pretty sure we've got this nailed.
  
- Memory and RasterOp - Complete.  Though in daily use and run through a
  fairly rigorous variety of tests, three different OSes and every bit of
  working software I can get my hands on, there's just the tiniest lingering
  doubt that a weird edge case may still crop up.  However, both Memory and
  RasterOp are now table driven and small tweaks to the ROM images have been
  able to correct those when they're (painstakingly) identified.
  
- Hard and Floppy disk - 95% complete.  Enough is implemented to allow 
  bringing up an "empty" hard disk with a new POS install from floppy images.
  Double-density floppy support is notably spotty, and there are some very
  low-level utilities that aren't fooled by our emulation, so work is ongoing.

- Keyboard - 99% complete.  Except for a HUGE CAVEAT FOR MacOS/Mono USERS:

    The Mac OS X keyboard driver for Mono is utterly broken for our
    purposes; I could not divine any way to update its mapping at a
    low enough level to allow the emulated PERQ to work properly, so
    I ended up basically rewriting it.  Since first patching this on
    a PowerPC under 10.5.8 (Mono 2.10.x), the exact same patch still
    applies to Mono 4.6.x on MacOS X Yosemite/Intel.  Obviously nobody
    has looked at that code in years, or PERQemu just has requirements
    that nobody else running Mono on a Mac has ever run across, ever.  :-|

    The fix, if you feel like sitting through a rebuild of Mono (or
    trying to pester the right people in the right place to get this
    applied to the actual Mono distribution?) is included in my fork
    of PERQemu at
        https://github.com/skeezicsb/PERQemu/Docs/KeyboardHandler.cs.Mac

    Currently caps lock is problematic and can get out of sync with the host.
    This is a minor inconvenience but it's on the bug list...

- Display and hardware cursor - Complete.  The streamlined code trusts the
  microcode to drive the vertical blanking, and the last of the mouse Y-pos
  artifacts are gone.

- Z80 Subsystem - 85% complete.  Currently only a simulation of the real 
  thing; as this is completely transparent (the PERQ1 can't upload custom
  software to the Z80 so a simulation is sufficient) a full emulation of 
  the Z80 hardware is very low on my list of priorities (but it would be nice 
  to have for accuracy's sake, or to allow PERQ-2 emulation).
   
- RS232 - Complete.  Talks to a real serial port on the host (where 
  available).  Software running under emulation can control the physical
  port.
   
- Tablets (Kriz and GPIB).  Complete.  Any software that requires a pointing
  device should function with either input device selected.  (See note above
  regarding button mapping between the 4-button GPIB and 3-button Kriz mice.)
  Though the Kriz tablets were rarely connected to a PERQ-1, they were supported
  and are FAR more efficient for the OS to handle than the BitPadOne's ASCII-
  serial data format so we enable them by default.  Note that Accent S4 only
  uses the GPIB tablet; PNX is untried at this point.  In POS F, you can choose
  which one to use with a switch to LOGIN (or in your login profile):
        LOGIN /TABLETTYPE=BITPAD    selects GPIB
        LOGIN /TABLETTYPE=TABLET    selects Kriz
  In the updated DETAILS program included with POS F.15, you can see which one
  is active for your current session by typing "DETAILS /ALL".
    

4.1 What's Not
--------------
 
- Ethernet.  Unimplemented, but on the list!
 
- PERQLink.  Unimplemented other than a stub that tells the microcode that
  there's nothing connected to it.
 
- Sound.  Implemented, but not hooked up to any sort of host output device.    

- Option boards: Canon, streamer tape.  On the list.
   
- Some debugger commands, including a set of configuration commands to allow
  more dynamic PERQ configurations.  A work in progress.
   
  
5.0 Changes
===========

v0.5 - TBD
  - Ethernet would be awwwwwwesommme!
  - Double-density floppy and PFD header support
  - Dynamic memory size configuration; other configuration options
  - Display/CPU sync for fast platforms, speed improvements for slow ones :-/
  - Split off the Getting Started section into a Users Guide, move this into
    a HISTORY or CHANGES file, restrain my verbosity and make the Readme far
    more succinct.

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

