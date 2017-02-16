Pegtop Delphi Component Library

Copyright 2004, 2005 by Jens Gruschel
For more information visit http://www.pegtop.net/delphi
Original date of publication: 01 Nov 2004
Latest change:                15 Aug 2005

This file contains some information about the installation, some additional
documentation (not finished yet), and some final words from the author.

________________________________________________________________________________


INSTALLATION

________________________________________________________________________________


This library was written using Delphi 5 / Windows XP (some parts originally were
written using Delphi 3 / Windows 98). While I believe that many parts should
work with other versions of Delphi, too (maybe with minor modifications), I have
not tested it.

To use the classes and functions declared within this library, you have to copy
all files to a folder Delphi can access (add a library path in the Delphi
environment options).

To use the components declared within this library, you also have to add all
relevant files (.pas / .dcr) to a Delphi package. You can make use of the
PegtopCommonD5.dpk, which already contains these files. After you have compiled
and installed the package, you should see all new components in the Delphi
component palette.

All components by default are installed to following palette pages:

Pegtop Controls: visual controls
Pegtop System:   non-visual components
Pegtop Dialogs:  dialogs

Of course you can reconfigure the palette if you don't like it this way. Maybe
you think there are too many pages, that's because I have much more components,
which are not published (yet).

You can also install single components simply by creating your own packages and
adding whatever you like. But of course some components rely on other ones.
That's the main reason why I put them all together.

________________________________________________________________________________


DOCUMENTATION

________________________________________________________________________________


List of components:

Pegtop Controls
* TPegtopButtonEdit              edit control with button inside
* TPegtopCheckBox                check box with additional method
* TPegtopComboBox                combo box with additional styles (icons etc.)
* TPegtopDesktopMagnifier        desktop magnifier
* TPegtopFileLabel               label for filenames (with icon)
* TPegtopFireButton              button fireing events while hold down
* TPegtopFloatEdit               edit control for floating point numbers
* TPegtopGradientPanel           panel with color gradient
* TPegtopIntEdit                 edit control for integer numbers
* TPegtopItemSwitch              another combo box
* TPegtopLineLabel               label with line and optinonal annex
* TPegtopLink                    clickable link
* TPegtopLogListBox              list box for multi line texts
* TPegtopPanel                   panel with event do draw background
* TPegtopPasswordEdit            password edit control with Windows XP support
* TPegtopProgressBar             very flexible progress bar (supports XP themes)
* TPegtopQualityLabel            label (supports 2x and 4x antialiasing)
* TPegtopRadioGroup              very flexible radio group
* TPegtopRangeBar                track bar to define a range (minimum / maximum)
* TPegtopScroller                scroller (as known from motion pictures)
* TPegtopThumbnail               thumbnail (supports 2x and 4x antialiasing)
* TPegtopTrackBar                track bar with label
* TPegtopVirtualListBox          list box with events to draw items
* TPegtopWebLink                 clickable link which opens a URL

Pegtop System
* TPegtopAlarmSchedule           priority queue based alarm schedule
* TPegtopBroadcast               inter application message network
* TPegtopFormHook                message hook for forms
* TPegtopFormMagnet              component that makes forms stick at each other
* TPegtopMessageReceiver         message component with own window handle
* TPegtopNetworkTimeSocket       socket to connect to time server (NTP)
* TPegtopSystemImages            image list for system images (file types etc.)
* TPegtopWindowHook              message hook for any windowed control

Pegtop Dialogs
* TPegtopExtendedOpenDialog      file dialog with user defined controls on it
* TPegtopExtendedSaveDialog      file dialog with user defined controls on it
* TPegtopGraphicOpenDialog       file dialog for graphics (sup. fast jpg decod.)
* TPegtopGraphicSaveDialog       file dialog for graphics (sup. fast jpg decod.)
* TPegtopOpenDialog              file dialog (with some enhancements)
* TPegtopSaveDialog              file dialog (with some enhancements)
* TPegtopTextOpenDialog          file dialog for text files (sup. rich text)
* TPegtopTextSaveDialog          file dialog for text files (sup. rich text)
* TPegtopWaveOpenDialog          file dialog for waves (with play button)
* TPegtopWaveSaveDialog          file dialog for waves (with play button)

Pegtop Color
* TPegtopColorBox                control to select a color
* TPegtopColorDialog             dialog to select a color
* TPegtopColorGradientBox        control to modify a color gradient
* TPegtopColorGradientDialog     dialog to modify a color gradient
* TPegtopColorGradientList       container for a number of color gradients
* TPegtopColorGradientListBox    list box for a color gradient list
* TPegtopColorGradientOpenDialog file dialog for color gradients
* TPegtopColorGradienSavetDialog file dialog for color gradients
* TPegtopColorTrackBar           track bar with a colored slider button

Further documentation is only available at http://www.pegtop.net/delphi
(it's also a good idea to take a look at the source code itself).

________________________________________________________________________________


SOME FINAL WORDS

________________________________________________________________________________


What's coming next?

I hope I can add some other controls of mine soon, including a color gradient
designer.

Then I plan to finish my work on my wave components, which allow to create your
own sound "studio", including playback and recording devices, mixers, different
sources and effects (single components which can easily be connected to each
other). These components will be published in a separate library, because they
are independant from this one.

If you like this work, I'd like to hear about it. Please use my mail form at
http://www.pegtop.net.

You have changed parts of this library and want to share your extensions or
modifications with others? I'm very busy quite frequently, but of course I will
try to update the library as often as I can, and your code is welcome, too.

If you've written some cool applications using this library, tell me and / or
send them to me (you don't have to if you don't want to). I'm really curious how
people are using this library.

You'd like to sell your commercial software using this library, but you don't
want to give credits to the author? Well, you have to. But if you really think
you have reasons, please contact me (for other questions or comments, too), if
I can see your reasons, maybe I agree with you.

Donations? No, I wrote this library because I need it, too. I alredy earn money
with applications using this library, and I'm glad if it is helpful to you, too.
So simply give some credits (in your application's about box or online help or
something similar). This won't make me rich, but famous ;-)

I hope you enjoy this library!

________________________________________________________________________________


Jens Gruschel

www.pegtop.net/delphi
