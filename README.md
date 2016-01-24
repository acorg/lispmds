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

These instructions also assume you're using the `bash` shell. If you have
no idea what that means, relax - you're probably using bash.

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
install ACL (note that you may be asked to enter your password for the
second command):

```
$ cd /usr/local
$ [ -d acl80 ] || { sudo mkdir acl80; sudo chown $USER acl80; }
$ tar xfj ~/Downloads/acl80-mac-intel.tar.bz2
```

Then
[download the license file](https://notebooks.antigenic-cartography.org/eu/acl80-mac-intel-enterprise.lic)
(here saved as `~/Downloads/acl80-mac-intel-enterprise.lic` but, depending
on your browser, you may have `acl80-mac-intel-enterprise.lic.txt`) and
install it:

```
$ mv ~/Downloads/acl80-mac-intel-enterprise.lic /usr/local/acl80/devel.lic
```

## Install the lispmds source

*If you are not working on LispMDS development*, you'll need to get a copy of the LispMDS
source code. First of all, create a [Github](http://github.com) account. Send either
Barbara, Derek, Eugene or Terry your Github username and ask to be added to the team that
can see the lispmds repository.

Follow [the instructions for adding an ssh key](https://help.github.com/articles/generating-ssh-keys/)
and uploading it into Github.  Once you've done that and you have been added to the lispmds team (last paragraph)
you should be able to clone the Github repository:

```
$ cd /usr/local
$ [ -d lispmds ] || { sudo mkdir lispmds; sudo chown $USER lispmds; }
$ git clone git@github.com:acorg/lispmds
```

*If you are working on LispMDS development*, you will already have a copy
of the `lispmds` repository checked out. In this case, make a symbolic link
called `lispmds` in `/usr/local` that points to your locally checked out
git repository.  E.g., via `ln -s $HOME/ac/lispmds /usr/local/lispmds`.

## Install brew

First, if you're not already using it, install [Homebrew](http://brew.sh/),
an OS X package manager.

### Fix /usr/local permissions

It's possible you'll need to change some permissions under `/usr/local` (I
think this happens on systems that had some `/usr/local` directories before
brew was installed). Run this command in any case (you may need your
password):

```
$ sudo chown -R $USER /usr/local/share /usr/local/lib
```

## Install Caskroom

We make use of brew [Caskroom](http://caskroom.io/), so you'll need that
too:

```
$ brew install caskroom/cask/brew-cask
```

## Set up your shell environment

### Don't use Mac Ports

If you're using Mac ports, I (Terry) recommend you stop. You can tell by
looking at your `PATH` variable:

```
$ echo $PATH
```

This will show all the directories your shell searches when you run a
command. If you see `/opt/local/bin` listed in there, you're using Mac
Ports.

You can stop using Mac Ports by getting rid of any mention of
`/sw/bin/init.sh` in either (or both) your `~/.bashrc` or `~/.bash_profile`
files. If you don't know what you're doing, ask for help. If you know how
to use a text editor and you think you know what you're doing, comment out
any such lines (i.e., put a `#` at the start of the lines).

### Set your shell's PATH

Add this line to your `~/.bashrc`:

```
PATH="/usr/local/acl80:/usr/local/lispmds/bin:$PATH"
```

There shouldn't be any other lines altering your `PATH` in that file. If
there are, either merge them or delete the other line.  If you don't know
how to do that, ask for help.

### See if it works

At this point it's probably easiest to log out and log back in. If you
repeat the `echo $PATH` command above, you should see no mention of
`/opt/local/bin` in your `PATH`.

## Install X11 via brew

You will be prompted for your password while the first command below is
running if brew needs to create `/opt/homebrew-cask`.

The second command below will present you a normal Mac OS X dialog to
install the XQuartz application that brew downloaded.  You'll be asked to
accept the license and for your password.

```
$ brew cask install Caskroom/cask/xquartz
$ open /opt/homebrew-cask/Caskroom/xquartz/*/XQuartz.pkg
```

## Install tools via brew

Pay particular attention to the `$ brew link -f --overwrite tcl-tk` in the
following. If it fails or gives any kind of warning or error, `wish` will
not be linked in and starting pymol from the MDS gui will not work.

```
$ brew tap homebrew/dupes
$ brew install homebrew/dupes/tcl-tk --enable-threads --with-x11
$ brew link -f --overwrite tcl-tk
$ brew install python --with-tcl-tk --enable-threads --with-x11
$ brew linkapps python

$ brew install gnuplot --with-x11
$ brew install imagemagick
$ brew install gs
```

To make sure the `wish` linking worked correctly:

```
$ type wish
wish is /usr/local/bin/wish
```

If you insted see `/usr/bin/wish`, the `$ brew link -f --overwrite tcl-tk`
command must have failed in some way. You *have* to get this right! If it
doesn't work, ask for help.

## Install PyMOL via brew

LispMDS can interact with [PyMOL](https://www.pymol.org/). Set that up via:

```
$ brew tap homebrew/science
$ brew install pymol
```

### Check that pymol works

It may be that your pymol relies on an older version of the GLEW library
(1.11) that you don't have installed (you may have 1.12).  Try running
`pymol` at the terminal. If you see an error like the following

```
$ pymol
Traceback (most recent call last):
  File "/usr/local/Cellar/pymol/1.7.4.0/libexec/lib/python2.7/site-packages/pymol/__init__.py", line 71, in <module>
    import pymol
  File "/usr/local/Cellar/pymol/1.7.4.0/libexec/lib/python2.7/site-packages/pymol/__init__.py", line 533, in <module>
    import pymol._cmd
ImportError: dlopen(/usr/local/Cellar/pymol/1.7.4.0/libexec/lib/python2.7/site-packages/pymol/_cmd.so, 2): Library not loaded: /usr/local/lib/libGLEW.1.11.0.dylib
  Referenced from: /usr/local/Cellar/pymol/1.7.4.0/libexec/lib/python2.7/site-packages/pymol/_cmd.so
  Reason: image not found
```

you'll need to install version 1.11 of GLEW manually. Go to
[the download page for GLEW 1.11](http://sourceforge.net/projects/glew/files/glew/1.11.0/)
and download the `glew-1.11.0.tgz` file. Then:

```
$ cd /tmp
$ tar xfz ~/Downloads/glew-1.11.0.tgz
$ cd glew-1.11.0
$ make
$ sudo make install
```

After this, you should be able to run `pymol` successfully.

### If pymol flickers like crazy

If pymol causes your screen to flicker crazily try running `pymol -M` to
force mono mode. If that works, you can set an environment variable so that
the lisp MDS gui (when you finally run it - see below) will know to invoke
pymol with the `-M` flag. Put this into your `~/.bashrc` file:

```
export MDS_PYMOL_FORCE_MONO=1
```

Then in the terminal:

```
$ source ~/.bashrc
```

# Running LispMDS

Run the LispMDS GUI via:

```
$ /usr/local/lispmds/bin/mds-gui
```

It will be more convenient for you if you make a shell alias for this, or
put `/usr/local/lispmds/bin` into your shell's `PATH`.  If you don't know
how to do that, ask a friendly system administrator for help :-)

When you're looking at a map window in the GUI, all the buttons on the left
hand side should have equal width. If they do not, you are not running the
correct version of wish. See the warning above about making sure brew links
the right version of wish!

# Upgrading lispmds

If changes have been made to LispMDS, and you're not a LispMDS developer,
you can install them via:

```
$ cd /usr/local/lispmds
$ git pull origin master
```

Restart LispMDS (if you're already running it) for the changes to
take effect.

If you are a LispMDS developer, you'll probably already know what to do.
Note that you can try out different branches just by switching to them (via
`git checkout branch-name`) in your local LispMDS repository.
