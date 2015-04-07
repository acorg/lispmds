from CGO import independentPymolCgoGroup
from Utils import frange

class BaseGrid:
    def __init__(self, options):
	cgo = independentPymolCgoGroup(options['base grid cgo name'])
	cgo.setColor(options['base grid color'])
	cgo.setLineWidth(options['base grid line width'])

	xmin = options['base grid x min']
	xmax = options['base grid x max']
	ymin = options['base grid y min']
	ymax = options['base grid y max']

	for x in frange(xmin, xmax):
	    cgo.addLine((x, ymin, 0.0), (x, ymax, 0.0))
	for y in frange(ymin, ymax):
	    cgo.addLine((xmin, y, 0.0), (xmax, y, 0.0))

	cgo.load()
	if options['base grid hidden']:
	    cgo.hide()
