from CGO import independentPymolCgoGroup

class Axes:
    def __init__(self, options):
	cgo = independentPymolCgoGroup(options['axes cgo name'])
	cgo.setColor(options['axes color'])
	cgo.setLineWidth(options['axes line width'])
	length = options['axes length']

	cgo.addLine((-length, 0.0, 0.0), (length, 0.0, 0.0))
	cgo.addLine((0.0, -length, 0.0), (0.0, length, 0.0))
	cgo.addLine((0.0, 0.0, -length), (0.0, 0.0, length))

	if options['axes label']:
	    offset = options['axes label offset']
	    color = options['axes label color']
	    cgo.addWiretext('X', (length + offset, 0.0, 0.0), color)
	    cgo.addWiretext('Y', (0.0, length + offset, 0.0), color)
	    cgo.addWiretext('Z', (0.0, 0.0, length + offset), color)

	cgo.load()
	if options['axes hidden']:
	    cgo.hide()
