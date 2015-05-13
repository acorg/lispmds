#!/usr/bin/env python

import sys
import os
from pymol import cmd

debug = os.environ.get('USER', '') == 'terry':

try:
    import tkFileDialog
    haveTk = True
except ImportError:
    haveTk = False

if sys.platform == 'darwin':
    mdsLibDir = os.path.join(
        os.environ.get('MDS_ROOT', '/usr/local/lispmds'),
        'src', 'mds', 'pymol', 'lib')
elif sys.platform[0:3] == 'win':
    mdsLibDir = os.path.join('C:\\', 'Program Files', 'mds', 'pymol', 'lib')
else:
    raise NotImplementedError, "Unsupported platform '%s'." % os.platform

if not os.path.exists(mdsLibDir):
    raise ImportError, "MDS pymol library directory '%s' does not exist." % mdsLibDir

if not os.path.isdir(mdsLibDir):
    raise ImportError, "MDS pymol library '%s' exists but is not a folder." % mdsLibDir

sys.path.append(mdsLibDir)

import MdsPymol
from Options import options, defaultMapOptions, perMapOptions
from ACmap import ACmap
from Axes import Axes
from BaseGrid import BaseGrid
from CGO import simpleCGOGroup # May be called in the options file we exec.
from pymol.cgo import * # For constants that may appear in the options file we exec.
from TiterPlane import TiterPlane # May be called in the options file we exec.
if debug:
    from Debug import triangle_debug

if haveTk:
    filetypes = [
	("ABL files", "*.pml", "TEXT"),
	("Python files", "*.py", "TEXT"),
	("All Files", "*")]

    def __init__(self):
	if debug:
	    self.menuBar.addmenuitem('Plugin', 'command',
				     'ABL debug',
				     label='ABL debug',
				     command=lambda s=self : fetch_abl_data_debug(s))

	    self.menuBar.addmenuitem('Plugin', 'command',
				     'Triangle debug',
				     label='Triangle debug',
				     command=lambda s=self : triangle_debug(s))

	self.menuBar.addmenuitem('Plugin', 'command',
				 'Antibody Landscape',
				 label='Antibody Landscape',
				 command=lambda s=self : fetch_abl_data(s))

	self.menuBar.addmenuitem('Plugin', 'command',
				 'Antigenic Map',
				 label='Antigenic Map',
				 command=lambda s=self : fetch_am_data(s))

	self.menuBar.addmenuitem('Plugin', 'command',
				 'Get View',
				 label='Show view',
				 command=lambda s=self : get_view(s))

    def get_view(app):
	print "View = %s" % str(cmd.get_view())

    def fetch_am_data(app):
	opendialog = tkFileDialog.Open(parent=app.root, filetypes=filetypes)
	file = opendialog.show(initialdir=options['initial directory'])
	if file:
	    abl(file)

    def fetch_abl_data(app):
	files = []
	while True:
	    opendialog = tkFileDialog.Open(parent=app.root, filetypes=filetypes)
	    file = opendialog.show(initialdir=options['initial directory'])
	    if file:
		files.append(file)
	    else:
		break
	if files:
	    abl(*files)

    if debug:
	def fetch_abl_data_debug(app):
	    abl('/Users/terry/mds/src/mds/pymol/map-viewer-plugin/L1.pml',
		'/Users/terry/mds/src/mds/pymol/map-viewer-plugin/L2.pml',
		optionsFile='/Users/terry/mds/src/mds/pymol/map-viewer-plugin/SAMPLE-OPTIONS')

def restrictedUpdate(d1, d2):
    '''Update dict d1 with keys from d2, but only if the keys already exist in d1.'''
    for key in d2:
	if key not in d1:
	    raise KeyError, "Key '%s' not present in destination dictionary." % key
	d1[key] = d2[key]

def abl(*files, **kwargs):
    MdsPymol.reset(options)

    if 'optionsFile' in kwargs:
	execfile(kwargs['optionsFile'], globals(), locals())

    for file in files:
	print "Processing file '%s'." % file
	fileOptions = perMapOptions.get(file, {})
	# Add a CGO prefix if we have multiple files and a prefix was not given.
	if len(files) > 1 and 'cgo prefix' not in fileOptions:
	    basename, _ = os.path.splitext(os.path.basename(file))
	    fileOptions['cgo prefix'] = basename + '_'
	mapOptions = defaultMapOptions.copy()
	restrictedUpdate(mapOptions, fileOptions)
	ACmap(file, mapOptions)

    # Process global options.

    cmd.clip('far', -20.0)
    #cmd.clip('near', 100.0)

    if options['axes compute']:
	Axes(options)
    if options['base grid compute']:
	BaseGrid(options)
    if options['view']:
	cmd.set_view(options['view'])
    if options['ray']:
	cmd.ray()
    if options['png file']:
	cmd.png(options['png file'])
    if options['quit']:
	cmd.quit()

# Add abl as a new pymol command.
cmd.extend('abl', abl)
