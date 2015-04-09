# LispMDS

LispMDS is a [Common Lisp](https://en.wikipedia.org/wiki/Common_Lisp)
implementation of
[Antigenic Cartography](http://www.pathogenevolution.zoo.cam.ac.uk/antigeniccartography)
developed at the
[Centre for Pathogen Evolution](http://www.pathogenevolution.zoo.cam.ac.uk/)
(CPE) at the [University of Cambridge](http://www.cam.ac.uk/).

# Installation on Mac OS X via brew

The following is known to work on OS X Yosemite 10.10.2. We do not yet have
installation instructions for other flavors of UNIX or other operating systems.

## Install Allegro Common Lisp

We run LispMDS under
[Allegro Common Lisp](http://franz.com/products/allegrocl/) (ACL). LispMDS
is unlikely to run out-of-the-box on other Lisp implementations.

If you're not at the CPE, you're on your own as to obtaining ACL. The
install instructions below may be of some use, though.

If you're at the CPE,
[download ACL](https://notebooks.antigenic-cartography.org/eu/acl80-mac-intel.tar.bz2)
and save it to a file (we'll assume you save it to `~/Downloads/acl80-mac-intel.tar.bz2`).

Open the OS X `Terminal` application and type the following commands to
install ACL:

```
$ cd /usr/local
$ tar xfj ~/Downloads/acl80-mac-intel.tar.bz2
```

Then
[download the license file](https://notebooks.antigenic-cartography.org/eu/acl80-mac-intel-enterprise.lic)
(here saved as `~/Downloads/acl80-mac-intel-enterprise.lic`) and install
it:

```
$ mv ~/Downloads/acl80-mac-intel-enterprise.lic /usr/local/acl80/devel.lic
```

## Install the lispmds source

*If you are not working on LispMDS development*, get a copy of the LispMDS
source code:

```
$ cd /usr/local
$ git clone https://github.com/acorg/lispmds
```

*If you are working on LispMDS development*, you will already have a copy
of the `lispmds` repository checked out. In this case, make a symbolic link
called `lispmds` in `/usr/local` that points to your locally checked out
git repository.  E.g., via `ln -s $HOME/ac/lispmds /usr/local/lispmds`.

## Install brew

First, if you're not already using it, install [Homebrew](http://brew.sh/),
an OS X package manager.

We make use of brew [Caskroom](http://caskroom.io/), so you'll need that
too:

```
$ brew install caskroom/cask/brew-cask
```

## Install X11 via brew

You will be prompted for your password while the first command below is
running if brew needs to create `/opt/homebrew-cask`.

The second command below will present you a normal Mac OS X dialog to
install the XQuartz application that brew downloaded.  You'll be asked to
accept the license and for your password.

```
$ brew cask install Caskroom/cask/xquartz
$ open /opt/homebrew-cask/Caskroom/xquartz/2.7.7/XQuartz.pkg
```

## Install various tools via brew

```
$ brew tap homebrew/dupes
$ brew install homebrew/dupes/tcl-tk --enable-threads --with-x11
$ brew link -f tcl-tk
$ brew install python --with-brewed-tk --enable-threads --with-x11
$ brew linkapps python

$ brew install gnuplot --with-x11
$ brew install imagemagick
$ brew install gs
```

## Install PyMOL via brew

LispMDS can interact with [PyMOL](https://www.pymol.org/). Set that up via:

```
$ brew tap homebrew/science
$ brew install pymol
```

# Running LispMDS

Run the LispMDS GUI via:

```
$ /usr/local/lispmds/bin/mds-gui
```

It will be more convenient for you if you make a shell alias for this, or
put `/usr/local/lispmds/bin` into your shell's `PATH`.  If you don't know
how to do that, ask a friendly system administrator for help :-)

# Upgrading lispmds

If changes have been made to LispMDS, and you're not a LispMDS developer,
you can install them via:

```
$ cd /usr/local/lispmds
$ git pull origin master
```

Restart LispMDS (if you're already running it) for the changes to take
effect.

If you are a LispMDS developer, you'll probably already know what to do.
Note that you can try out different branches just by switching to them (via
`git checkout branch-name`) in your local LispMDS repository.
