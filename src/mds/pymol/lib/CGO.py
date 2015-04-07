from pymol.cgo import *
from pymol.vfont import plain
from Coord import Coord

if hasattr(cmd, 'get_version'):
    _, version = cmd.get_version()
else:
    # Set to something old.
    version = 0.98
    
transparencyUnimplementedWarningIssued = False
transparencyMinVersion = 0.99

class independentPymolCgoGroup(object):
    '''

    A group of Pymol CGO objects. They are independent in the sense that
    pymol will let you turn display of them all on/off as you like.

    '''
    
    def __init__(self, name):
	self._name = name.replace(' ', '_')
	self.cgo = []
	self.lastTextWidth = None
	self.lastColor = None
	self.lastLineWidth = None

    def name(self):
	return self._name

    def setTextWidth(self, width):
	if width != None and self.lastTextWidth != width:
	    cmd.set('cgo_line_radius', width)
	    self.lastTextWidth = width

    def setLineWidth(self, width):
	if width != None and self.lastLineWidth != width:
	    self.cgo.extend([LINEWIDTH, width])
	    self.lastLineWidth = width

    def addWiretext(self, text, coords, color=None, textWidth=None):
	self.setColor(color)
	self.setTextWidth(textWidth)
	wire_text(self.cgo, plain, list(coords), text)
	
    def addLine(self, start, end):
	self.cgo.extend([BEGIN, LINES])
	self.addVertex(start, end)
	self.cgo.append(END)

    def startTriangleFan(self):
	self.cgo.extend([BEGIN, TRIANGLE_FAN])

    def startTriangleStrip(self):
	self.cgo.extend([BEGIN, TRIANGLE_STRIP])

    def startTriangles(self):
	self.cgo.extend([BEGIN, TRIANGLES])

    def startLineStrip(self):
	self.cgo.extend([BEGIN, LINE_STRIP])

    def startLineLoop(self):
	self.cgo.extend([BEGIN, LINE_LOOP])

    def addVertex(self, *vertices):
	for vertex in vertices:
	    self.cgo.extend([VERTEX, vertex[0], vertex[1], vertex[2]])

    def addNorm(self, norm):
	self.cgo.extend([NORMAL, norm[0], norm[1], norm[2]])

    def endStrip(self):
	self.cgo.append(END)

    endLoop = endStrip
    endLineLoop = endStrip
    endLineStrip = endStrip
    endTriangles = endStrip
    endTriangleStrip = endStrip
    endTriangleFan = endStrip

    def addSphere(self, center, radius):
	self.cgo.extend([SPHERE, center[0], center[1], center[2], radius])

    def addAlpha(self, value):
	self.cgo.extend([ALPHA, value])

    def addCube(self, center, sideLength):
	radius = sideLength / 2.0
	# Make two line loops, one around the bottom of the cube, other around the top (i.e., with Z incremented by sideLength)
	for zOffset in (0, sideLength):
	    start = center - (radius, radius, radius - zOffset)
	    self.startLineLoop()
	    self.addVertex(
		start,
		start + (sideLength, 0, 0),
		start + (sideLength, sideLength, 0),
		start + (0, sideLength, 0),
		)
	    self.endLoop()

	# Now draw lines up the 4 sides.
	self.addLine(start, start - (0, 0, sideLength))
	start += (sideLength, 0, 0)
	self.addLine(start, start - (0, 0, sideLength))
	start += (0, sideLength, 0)
	self.addLine(start, start - (0, 0, sideLength))
	start -= (sideLength, 0, 0)
	self.addLine(start, start - (0, 0, sideLength))

    def addTriangle(self, X, Y, Z, normX, normY, normZ, colorX, colorY, colorZ):
	self.cgo.extend([TRIANGLE,
			 X[0], Y[0], Z[0],
			 X[1], Y[1], Z[1],
			 X[2], Y[2], Z[2],
			 normX[0], normY[0], normZ[0],
			 normX[1], normY[1], normZ[1],
			 normX[2], normY[2], normZ[2],
			 colorX[0], colorY[0], colorZ[0],
			 colorX[1], colorY[1], colorZ[1],
			 colorX[2], colorY[2], colorZ[2]])
			

    def setColor(self, rgb):
	if rgb and self.lastColor != rgb:
	    self.cgo.extend([COLOR, rgb[0], rgb[1], rgb[2]])
	    self.lastColor = rgb

    def load(self):
	cmd.delete(self._name)
	cmd.load_cgo(self.cgo, self._name)
	
    def hide(self):
	cmd.disable(self._name)

    if version < transparencyMinVersion:
	# Transparency not supported.
	def setTransparency(self, level):
	    global transparencyUnimplementedWarningIssued
	    if not transparencyUnimplementedWarningIssued:
		print "WARNING: CGO transparency is not supported in Pymol versions prior to %.2f" % transparencyMinVersion
		transparencyUnimplementedWarningIssued = True
    else:
	def setTransparency(self, level):
	    cmd.set('cgo_transparency', level, self._name)

class simpleCGOGroup(independentPymolCgoGroup):
    def __init__(self, name, data=[], hidden=False):
	super(simpleCGOGroup, self).__init__(name)
	self.cgo = data
	self.load()
	if hidden:
	    self.hide()
