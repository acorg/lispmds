import copy
from CGO import independentPymolCgoGroup

class BoundingBox:
    X, Y, Z = range(3)
    MIN, MAX = range(2)

    def __init__(self, options=None):
	# The min/max values seen for X, Y, Z coords
	self.bb = [[None, None], [None, None], [None, None]]
	self.options = options

    def xmin(self, box = None):
	if not box: box = self.bb
	return box[self.X][self.MIN]
    
    def xmax(self, box = None):
	if not box: box = self.bb
	return box[self.X][self.MAX]
    
    def ymin(self, box = None):
	if not box: box = self.bb
	return box[self.Y][self.MIN]
    
    def ymax(self, box = None):
	if not box: box = self.bb
	return box[self.Y][self.MAX]
    
    def zmin(self, box = None):
	if not box: box = self.bb
	return box[self.Z][self.MIN]
    
    def zmax(self, box = None):
	if not box: box = self.bb
	return box[self.Z][self.MAX]

    def union(self, other):
	'''Expand our bounding box to incorporate the other bounding box.'''
	for axis in (self.X, self.Y, self.Z):
	    if other.bb[axis][self.MIN] < self.bb[axis][self.MIN]:
		self.bb[axis][self.MIN] = other.bb[axis][self.MIN]
	    if other.bb[axis][self.MAX] > self.bb[axis][self.MAX]:
		self.bb[axis][self.MAX] = other.bb[axis][self.MAX]
	    
    def add(self, pos, radius=0):
	buffer = self.options['bounding box buffer']
	for axis in (self.X, self.Y, self.Z):
	    low = pos[axis] - radius - buffer
	    if self.bb[axis][self.MIN] == None:
		self.bb[axis][self.MIN] = low
	    else:
		if self.bb[axis][self.MIN] > low:
		    self.bb[axis][self.MIN] = low

	    high = pos[axis] + radius + buffer
	    if self.bb[axis][self.MAX] == None:
		self.bb[axis][self.MAX] = high
	    else:
		if self.bb[axis][self.MAX] < high:
		    self.bb[axis][self.MAX] = high

    def computeGridRowsCols(self, density):
	'''The density is the number of rows/cols we want per unit of distance.'''
	return int((self.ymax() - self.ymin()) * density) + 1, int((self.xmax() - self.xmin()) * density) + 1
    
    def computeBoundingBox(self):
	cgo = independentPymolCgoGroup(self.options['cgo prefix'] + self.options['bounding box cgo name'])
	cgo.setColor(self.options['bounding box color'])

	cgo.addLine((self.xmin(), self.ymin(), self.zmin()), (self.xmax(), self.ymin(), self.zmin())) # Bottom back
	cgo.addLine((self.xmin(), self.ymin(), self.zmin()), (self.xmin(), self.ymin(), self.zmax())) # Bottom left
	cgo.addLine((self.xmax(), self.ymin(), self.zmin()), (self.xmax(), self.ymin(), self.zmax())) # Bottom right
	cgo.addLine((self.xmin(), self.ymin(), self.zmax()), (self.xmax(), self.ymin(), self.zmax())) # Bottom front
	cgo.addLine((self.xmin(), self.ymax(), self.zmin()), (self.xmax(), self.ymax(), self.zmin())) # Top back
	cgo.addLine((self.xmin(), self.ymax(), self.zmin()), (self.xmin(), self.ymax(), self.zmax())) # Top left
	cgo.addLine((self.xmax(), self.ymax(), self.zmin()), (self.xmax(), self.ymax(), self.zmax())) # Top right
	cgo.addLine((self.xmin(), self.ymax(), self.zmax()), (self.xmax(), self.ymax(), self.zmax())) # Top front
	cgo.addLine((self.xmin(), self.ymin(), self.zmin()), (self.xmin(), self.ymax(), self.zmin())) # Left back
	cgo.addLine((self.xmin(), self.ymin(), self.zmax()), (self.xmin(), self.ymax(), self.zmax())) # Left front
	cgo.addLine((self.xmax(), self.ymin(), self.zmin()), (self.xmax(), self.ymax(), self.zmin())) # Right back
	cgo.addLine((self.xmax(), self.ymin(), self.zmax()), (self.xmax(), self.ymax(), self.zmax())) # Right front

	return cgo
	    
    def computeGrid(self):
	cgo = independentPymolCgoGroup(self.options['cgo prefix'] + self.options['grid cgo name'])
	cgo.setColor(self.options['grid color'])
	spacing = self.options['grid spacing'];

	cube = copy.deepcopy(self.bb)
	for i in (self.X, self.Y, self.Z):
	    cube[i][self.MIN] = int(cube[i][self.MIN] - 1.0)
	    cube[i][self.MAX] = int(cube[i][self.MAX] + 1.0)

	for x in range(self.xmin(cube), self.xmax(cube), spacing):
	    if x < self.xmin() or x > self.xmax(): continue
	    for y in range(self.ymin(cube), self.ymax(cube), spacing):
		if y < self.ymin() or y > self.ymax(): continue
		cgo.addLine((x, y, self.zmin()), (x, y, self.zmax()))

	    for z in range(self.zmin(cube), self.zmax(cube), spacing):
		if z < self.zmin() or z > self.zmax(): continue
		cgo.addLine((x, self.ymin(), z), (x, self.ymax(), z))

	for z in range(self.zmin(cube), self.zmax(cube), spacing):
	    if z < self.zmin() or z > self.zmax(): continue
	    for y in range(self.ymin(cube), self.ymax(cube), spacing):
		if y < self.ymin() or y > self.ymax(): continue
		cgo.addLine((self.xmin(), y, z), (self.xmax(), y, z))

	return cgo
	    
    def computeNotches(self):
	cgo = independentPymolCgoGroup(self.options['cgo prefix'] + self.options['notches cgo name'])
	cgo.setColor(self.options['notches color'])
	spacing = self.options['notches spacing'];
	length = self.options['notches length'];

	cube = copy.deepcopy(self.bb)
	for i in (self.X, self.Y, self.Z):
	    cube[i][self.MIN] = int(cube[i][self.MIN] - 1.0)
	    cube[i][self.MAX] = int(cube[i][self.MAX] + 1.0)

	for x in range(self.xmin(cube), self.xmax(cube), spacing):
	    if x < self.xmin() or x > self.xmax(): continue
	    for y in range(self.ymin(cube), self.ymax(cube), spacing):
		if y < self.ymin() or y > self.ymax(): continue
		cgo.addLine((x, y, self.zmin()), (x, y, self.zmin() + length))
		cgo.addLine((x, y, self.zmax()), (x, y, self.zmax() - length))

	    for z in range(self.zmin(cube), self.zmax(cube), spacing):
		if z < self.zmin() or z > self.zmax(): continue
		cgo.addLine((x, self.ymin(), z), (x, self.ymin() + length, z))
		cgo.addLine((x, self.ymax(), z), (x, self.ymax() - length, z))

	for z in range(self.zmin(cube), self.zmax(cube), spacing):
	    if z < self.zmin() or z > self.zmax(): continue
	    for y in range(self.ymin(cube), self.ymax(cube), spacing):
		if y < self.ymin() or y > self.ymax(): continue
		cgo.addLine((self.xmin(), y, z), (self.xmin() + length, y, z))
		cgo.addLine((self.xmax(), y, z), (self.xmax() - length, y, z))

	return cgo
	    
    def computeDots(self):
	cgo = independentPymolCgoGroup(self.options['cgo prefix'] + self.options['dots cgo name'])
	cgo.setColor(self.options['dots color'])
	radius = self.options['dots radius'];
	spacing = self.options['dots spacing'];

	cube = copy.deepcopy(self.bb)
	for i in (self.X, self.Y, self.Z):
	    cube[i][self.MIN] = int(cube[i][self.MIN] - 1.0)
	    cube[i][self.MAX] = int(cube[i][self.MAX] + 1.0)

	for x in range(self.xmin(cube), self.xmax(cube), spacing):
	    if x < self.xmin() or x > self.xmax(): continue
	    for y in range(self.ymin(cube), self.ymax(cube), spacing):
		if y < self.ymin() or y > self.ymax(): continue
		for z in range(self.zmin(cube), self.zmax(cube), spacing):
		    if z < self.zmin() or z > self.zmax(): continue
		    cgo.addSphere((x, y, z), radius)

	return cgo
