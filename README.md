# LispMDS

LispMDS is a Common Lisp implementation of
[Antigenic Cartography](http://www.pathogenevolution.zoo.cam.ac.uk/antigeniccartography)
developed at the
[Centre for Pathogen Evolution](http://www.pathogenevolution.zoo.cam.ac.uk/) (CPE)
at the [University of Cambridge](http://www.cam.ac.uk/).

# Installation on Mac OS X

The following is known to work on OS X Yosemite 10.10.2.

## Allegro Common Lisp

We run LispMDS under
[Allegro Common Lisp](http://franz.com/products/allegrocl/) (ACL) and is
unlikely to run out-of-the-box on other Lisp implementations.

If you're not at the CPE, you're on your own as to obtaining ACL. The
install instructions below may be of some use, though.

### CPE members

If you're at the CPE,
[download ACL](https://notebooks.antigenic-cartography.org/eu/acl80-mac-intel.tar.bz2)
and save it to a file (we'll assume you save it to `~/Downloads/acl80-mac-intel.tar.bz2`).

Then on the command line (i.e., using the `Terminal` app) install it:

```
$ mkdir -p /usr/local/acl80
$ cd /usr/local/acl80
$ tar xfj ~/Downloads/acl80-mac-intel.tar.bz2
```

Then
[download the license file](https://notebooks.antigenic-cartography.org/eu/acl80-mac-intel-enterprise.lic)
(here saved as `~/Downloads/acl80-mac-intel-enterprise.lic`) and install
it:

```
$ mv ~/Downloads/acl80-mac-intel-enterprise.lic /usr/local/acl80/devel.lic
```

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

## Mac OS X

First, if you're not already using it, install [Homebrew](http://brew.sh/),
an OS X package manager.

## Install X11

Get brew to install XQuartz.

You will be prompted for your password while the first command below is
running if brew needs to create `/opt/homebrew-cask`.

The second command below will present you a normal Mac OS X dialog to
install the XQuartz application that brew downloaded.  You'll be asked to
accept the license and for your password.

```
$ brew cask install Caskroom/cask/xquartz
$ open /opt/homebrew-cask/Caskroom/xquartz/2.7.7/XQuartz.pkg
```

## Install tcl/tk and python

```
$ brew tap homebrew/dupes
$ brew install homebrew/dupes/tcl-tk --enable-threads --with-x11
$ brew link -f tcl-tk
$ brew install python --with-brewed-tk --enable-threads --with-x11
$ brew linkapps python
```

## Installing Pymol

LispMDS can interact with [Pymol](https://www.pymol.org/). Set that up via:

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
