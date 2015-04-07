import os
import math

# The follwing initDir variable is just for easing file selection via the
# Tk file dialog, in the case that you're using the X11 plugin.  It is not
# used to otherwise locate files.

if os.environ.get('USER') == 'terry':
    initDir = os.path.expandvars(os.path.join('$MDS_ROOT', 'src', 'mds', 'pymol', 'map-viewer-plugin'))
else:
    initDir = os.path.curdir

#
# These are the global (usually image/control related) options.
#   
options = {
    'background color' : (0.0, 0.0, 0.0),
    'light-colored background' : False,
    'initial directory' : initDir,
    'ray' : False,
    'view' : None,
    'png file' : None,
    'quit' : False,

    'base grid compute' : True,
    'base grid hidden' : False,
    'base grid cgo name' : 'base grid',
    'base grid transparency' : 0.8,
    'base grid line width' : 1.0,
    'base grid color' : (0.7, 0.1, 0.3),
    'base grid x min' : -25,
    'base grid x max' : 25,
    'base grid y min' : -25,
    'base grid y max' : 25,

    'axes compute' : True,
    'axes hidden' : False,
    'axes cgo name' : 'axes',
    'axes transparency' : 0.0,
    'axes line width' : 1.0,
    'axes color' : (0.3, 0.3, 0.3),
    'axes length' : 30.0,
    'axes label' : True,
    'axes label color' : (0.9, 0.3, 0.3),
    'axes label offset' : 1.0,
    }

#
# These are the options applied to all maps, unless overridden by an options file.
#   
defaultMapOptions = {
    'cgo prefix' : '', # Note that if the options file does not override this, it will be set from each map file name.

    'strains compute' : True,
    'strains hidden' : False,
    'strains cgo name' : 'strains',
    'strains transparency' : 0.0,
    'strains radius' : 0.3, # If a sphere.
    'strains side length' : None, # If a cube (this is computed below, from the radius).
    'strains color' : (1.0, 1.0, 1.0),
    'strains shape' : 'sphere',

    'strains names compute' : True,
    'strains names hidden' : True,
    'strains names cgo name' : 'strain names',
    'strains names transparency' : 0.0,
    'strains names text width' : 0.01,
    'strains names color' : (1.0, 1.0, 1.0),
    'strains names offset' : (2.0, 0.0, 0.0),

    'strains blobs compute' : True,
    'strains blobs hidden' : False,
    'strains blobs cgo name' : 'strain blobs',
    'strains blobs style' : 'dots', # One of 'dots', 'triangleFan' or 'contours'.
    'strains blobs contours line width' : 1.0,
    'strains blobs dots radius' : 0.05,
    'strains blobs z inc' : 0.01,

    'antisera compute' : True,
    'antisera hidden' : False,
    'antisera cgo name' : 'antisera',
    'antisera transparency' : 0.0,
    'antisera radius' : 0.3, # If a sphere.
    'antisera side length' : None, # If a cube (this is computed below, from the radius).
    'antisera color' : (1.0, 1.0, 1.0),
    'antisera shape' : 'cube',

    'antisera names compute' : True,
    'antisera names hidden' : True,
    'antisera names cgo name' : 'antisera names',
    'antisera names transparency' : 0.0,
    'antisera names text width' : 0.01,
    'antisera names color' : (1.0, 1.0, 1.0),
    'antisera names offset' : (2.0, 0.0, 0.0),

    'antisera blobs compute' : True,
    'antisera blobs hidden' : False,
    'antisera blobs cgo name' : 'sera blobs',
    'antisera blobs style' : 'contours', # One of 'dots', 'triangleFan' or 'contours'.
    'antisera blobs contours line width' : 1.0,
    'antisera blobs dots radius' : 0.1,
    'antisera blobs z inc' : 0.01,

    'titer compute' : True,
    'titer hidden' : True,
    'titer cgo name' : 'titer',
    'titer transparency' : 0.0,
    'titer line width' : 1.0,

    'procrustes names compute' : True,
    'procrustes names hidden' : True,
    'procrustes names cgo name' : 'procrustes names',
    'procrustes names transparency' : 0.0,
    'procrustes names text width' : 0.01,
    'procrustes names color' : (1.0, 1.0, 1.0),
    'procrustes names offset' : (2.0, 0.0, 0.0),

    'procrustes spheres compute' : True,
    'procrustes spheres hidden' : True,
    'procrustes spheres cgo name' : 'procrustes spheres',
    'procrustes spheres transparency' : 0.0,
    'procrustes spheres color' : (1.0, 1.0, 1.0),
    'procrustes spheres radius' : 0.2,

    'procrustes lines compute' : True,
    'procrustes lines hidden' : True,
    'procrustes lines cgo name' : 'procrustes lines',
    'procrustes lines transparency' : 0.0,
    'procrustes lines color' : (1.0, 1.0, 1.0),
    'procrustes lines width' : 1.0,

    'bounding box compute' : True,
    'bounding box hidden' : False,
    'bounding box cgo name' : 'bb',
    'bounding box transparency' : 0.0,
    'bounding box buffer' : 0.03,
    'bounding box color' : (1.0, 1.0, 1.0),
    'bounding box bound names' : False,

    'grid compute' : True,
    'grid hidden' : True,
    'grid cgo name' : 'grid',
    'grid transparency' : 0.0,
    'grid spacing' : 1,
    'grid color' : (0.05,) * 3,

    'notches compute' : True,
    'notches hidden' : True,
    'notches cgo name' : 'notches',
    'notches transparency' : 0.0,
    'notches length' : 0.2,
    'notches spacing' : 1,
    'notches color' : (1.0, 1.0, 1.0),

    'dots compute' : True,
    'dots hidden' : True,
    'dots cgo name' : 'dots',
    'dots transparency' : 0.0,
    'dots radius' : 0.02,
    'dots spacing' : 1,
    'dots color' : (1.0, 1.0, 1.0),

    'surface compute' : True,
    'surface method' : 'r', # One of 'r', 'gnuplot', 'precomputed'.
    'surface precomputed data' : [], # A list of (x, y, z) triples in Gnuplot order (see long comment in ACmap.py)
    'surface rows' : 20, # Generic number of rows to use in the surface (not used if 'surface method' is 'precomputed').
    'surface cols' : 20, # Generic number of cols to use in the surface (not used if 'surface method' is 'precomputed').
    # If gnuplot is being used to plot a surface, we need the following:
    'surface gnuplot density' : 2,
    'surface gnuplot norm' : 1,
    # If R is being used to plot a surface, we need the following:
    # 'surface r fit' : '2slope.cone.1', # The name of the R fitting function to use.
    'surface r fit' : 'loess', # The name of the R fitting function to use.
    'surface vaccine' : None, # The name of the map strain (if any) that was the vaccine.

    'surface spheres compute' : True,
    'surface spheres hidden' : True,
    'surface spheres cgo name' : 'surface spheres',
    'surface spheres transparency' : 0.0,
    'surface spheres radius' : 0.1,
    'surface spheres color' : (0.8, 0.0, 0.5),

    'surface triangles compute' : True,
    'surface triangles hidden' : True,
    'surface triangles cgo name' : 'surface triangles',
    'surface triangles transparency' : 0.0,
    'surface triangles color' : (0.8, 0.0, 0.5),

    'surface quads compute' : True,
    'surface quads hidden' : True,
    'surface quads cgo name' : 'surface quads',
    'surface quads transparency' : 0.0,
    'surface quads color' : (0.8, 0.0, 0.5),
    
    'error lines compute' : True,
    'error lines hidden' : True,
    'error lines cgo name' : 'error lines',
    'error lines transparency' : 0.0,
    'error lines width' : 1.0,
    
    'connection lines compute' : True,
    'connection lines hidden' : True,
    'connection lines cgo name' : 'connection lines',
    'connection lines transparency' : 0.0,
    'connection lines width' : 1.0,
    
    'prediction lines compute' : True,
    'prediction lines hidden' : True,
    'prediction lines cgo name' : 'prediction lines',
    'prediction lines transparency' : 0.0,
    'prediction lines width' : 1.0,

    'titer plane compute' : True,
    'titer plane hidden' : False,
    'titer plane cgo name' : 'titer plane',
    'titer plane transparency' : 0.8,
    'titer plane titer' : 40.0,
    'titer plane line width' : 1.0,
    'titer plane color' : (0.8, 0.8, 0.8),
    'titer plane x min' : -15,
    'titer plane x max' : 15,
    'titer plane y min' : -15,
    'titer plane y max' : 15,
    }

# Compute the side length of strain and antisera when they're cubes.
# This preserves volume. Doesn't look great, doesn't look bad.
for type in ('strains', 'antisera'):
    type += ' '
    radius = defaultMapOptions[type + 'radius']
    # Preserve volume - this makes cubes too too big
    # defaultMapOptions[type + 'side length'] = pow(math.pi * radius * radius, 1.0 / 3.0)
    # Make cube diagonal be the same as the sphere diameter
    defaultMapOptions[type + 'side length'] = 2.0 * radius / math.sqrt(3.0)

#
# These are the per-map (i.e., per-file) options. Key is filename,
# value is a dictionary in the form of defaultMapOptions above.
#   
perMapOptions = {
    }
