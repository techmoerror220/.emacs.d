## Emacs Starter Kit for the Social Sciences

A drop-in starter-kit for Emacs 24 on the Mac, derived from
[Phil Hagelberg's](https://github.com/technomancy) and
[Eric Schulte's](https://github.com/eschulte/) originals, but now
somewhat different from those. Designed for social scientists
researching and writing in plain text using some combination of LaTeX,
R, ESS, Git, Markdown, and Org-Mode. Support also for Python,
Javascript, CSV file editing, and other goodies.

## Motivation

[![](http://kieranhealy.org/img/emacs-starter-kit.jpg)](http://kieranhealy.org/img/emacs-starter-kit.jpg)

Emacs is a very powerful
editor but it is less useful out-of-the-box than it might be, in part
because many convenient settings and modes are not activated by default.
The starter kit is a drop-in set of nice default settings. The idea is
for you to be able to download GNU Emacs, put the starter kit into
`~/.emacs.d/`, and get to work. If you already use Emacs and have a
`.emacs` file or `~/.emacs.d` directory, the starter kit is designed to
replace them, while leaving a place for you to easily append your own
customizations.

The starter kit is designed to be used with GNU Emacs. Version 24.4
(released October 2014) or later is required. It will not work with
[Aquamacs](http://aquamacs.org/ "Aquamacs: Emacs for Mac OS X") without
modification. More detailed commentary and documentation is provided
inside the kit's `.org` files.

## Before you Begin

If you want to use the tools that the starter-kit works with—LaTeX, R,
Git, Pandoc, and all the rest—then you will need to install them on your
Mac. The ground floor is Apple's own suite of Developer Tools, which
allow you to compile software yourself and include things like Git. The
most straightforward way to get these tools is to **install Xcode**.
Xcode is what software developers use to write Mac and iOS applications.
As such it comes with a bunch of things we are not so interested in, but
it makes it easy to install what we do need. It's available [for free
via the Mac App Store](https://developer.apple.com/xcode/). Once
downloaded, launch Xcode, go to Xcode &gt; Preferences &gt; Downloads
and install the **Command Line Tools for Xcode**. At that point you can
quit Xcode and never use it again. Alternatively, go to [Apple's
Developer Site](https://developer.apple.com/), login with your Apple ID
and download the Command Line Tools for Xcode package by itself, without
the Xcode application.

If you have used Emacs before and already have a `.emacs` file or
`.emacs.d` directory, back them up and then delete them. See below for
details on how to add customizations to the kit.

## Getting the Starter Kit ##

You have two choices. You can [download a `.zip` file of the
kit](https://github.com/kjhealy/emacs-starter-kit/archive/master.zip).
This will give you a static snapshot of the most recent version. But if
you want to keep up with changes to the kit, you should clone the source
code using git rather than simply copying a static version of it. You
should be using version control on your plain-text documents anyway, so
I recommend this second option.

**0.1 Get Emacs**. The starter kit requires Emacs 24.4 (released October
2014). [Download Emacs
here](http://emacsformacosx.com/ "GNU Emacs For Mac OS X").
Alternatively, if you are comfortable with Homebrew, compile and install
it.

**0.2 Install a Modern TeX Distribution and the Skim PDF reader**. If
you are using OS X, [download MacTeX
here](http://www.tug.org/mactex/ "MacTeX - TeX Users Group") and install
it. The kit is set up to use the [Skim PDF
reader](http://skim-app.sourceforge.net) to display PDF files created
from `.tex` files. You can use other readers, but will need to modify
the setup in the `starter-kit-latex.org` file.

**0.3 Install R and Pandoc**. These are not strictly required for the
installation to work, and you can skip this step if you like. But you
will probably be using them anyway, if you're not already. If you're
doing statistical work you will probably want to use R or Stata. R is
free and you can [download it here](http://www.r-project.org). Pandoc is
a very useful utility that lets you convert easily between many
different forms of plain-text markup (such as Markdown, HTML, LaTeX, and
others). [Get Pandoc here](http://johnmacfarlane.net/pandoc/).

**0.4 Note your user name** or the name of your computer. If you don't
know either, open the Terminal application and do

    $ whoami

for your user name and

    $ hostname

for the system name. You will need to know your login name to activate
the final customization file properly. You can use the system name as
well (or instead).

## Installation ##

**1.** If you downloaded a `.zip` file of the kit, you must uncompress
it, move the resulting folder to the top level of your home director and
rename it `.emacs.d`. Assuming the downloaded zip file is in your
`~/Downloads` folder, open a Terminal window and do this:

    $ cd ~/Downloads
    $ unzip emacs-starter-kit-master.zip
    $ mv emacs-starter-kit-master ~/.emacs.d

Alternatively, if you are using git (the preferred method), then clone
the starter kit from github. Open a Terminal window and do this:

    $ git clone git://github.com/kjhealy/emacs-starter-kit ~/.emacs.d

**2.** Inside the file `kjhealy.org`, change the paths to any BibTeX
databases as described at the top of that file.

**3.** Rename the starter kit's `kjhealy.org` file to that of
`%your-username%.org` or `%your-systemname%.org`, based on the
information you noted in 0.4 above. This is where you can add in any of
your own further customizations to Emacs.

**4.** Launch Emacs.

When you first start Emacs after installing the starter-kit, it will try
to contact several package repositories, so make sure you have an
internet connection. The kit will download packages mainly from the
official [GNU ELPA
repository](http://http://elpa.gnu.org "Welcome to ELPA") and the [MELPA
Repo](http://melpa.milkbox.net "MELPA"). Each package will be fetched,
compiled by Emacs, and stored in the `~/.emacs.d` directory. This
process is sometimes prone to hiccups as packages are fetched from the
servers, so please be patient with it. If it doesn't get everything
first time around, quit and relaunch Emacs, and it will try again. If
the problem persists—especially if you get a message saying "The package
'auctex' is not available for installation"—you can manually install
packages as follows. Open Emacs, do `M-x list-packages` and in the
resulting buffer search or scroll down the list to, e.g. Auctex, mark it
for installation by pressing `i` and then install it (or them) by
hitting `x`. With the packages in place, restart Emacs and the starter
kit will finish setting itself up. Unfortunately, I can't control these
intermittent installation errors. They seem to have something to do with
the way Emacs talks to the GNU ELPA package server.

**5.** (Optional.) Once Emacs is up and running, do
`M-x starter-kit-compile` to byte-compile the starter-kit's files, for
slightly faster loading.

