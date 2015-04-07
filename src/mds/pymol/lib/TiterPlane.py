from CGO import independentPymolCgoGroup
from Utils import frange, log2

class TiterPlane:
    def __init__(self, titer, cgoName, color, lineWidth, xMin, xMax, yMin, yMax, log=True):
	cgo = independentPymolCgoGroup(cgoName) 
	cgo.setColor(color)
	cgo.setLineWidth(lineWidth)

	if log:
	    titer = log2(titer)

	for x in frange(xMin, xMax):
	    cgo.addLine((x, yMin, titer), (x, yMax, titer))
	for y in frange(yMin, yMax):
	    cgo.addLine((xMin, y, titer), (xMax, y, titer))

	self._cgo = cgo

    def cgo(self):
	return self._cgo

    def load(self):
	self._cgo.load()
