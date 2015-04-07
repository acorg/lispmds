import sys
from pymol import cmd
from BoundingBox import BoundingBox
from TiterPlane import TiterPlane
from Coord import Coord
from CGO import independentPymolCgoGroup
from Utils import log2, frange
import MdsError

class ACmap:
    def __init__(self, file, options):
        execfile(file)

        if not hasattr(self, 'map_data'):
            raise MdsError.ExecError, "After loading map data from '%s' no map_data has been defined." % file

        print "Read %d points from '%s'" % (len(self.map_data.keys()), file)

        self.file = file
        self.options = options

        # Under some circumstances we need to maintain a bounding box
        if options['bounding box compute'] or options['grid compute'] or options['notches compute'] or options['dots compute']:
            self.bb = BoundingBox(options)
        else:
            self.bb = None
            
        cgoData = {} # A collection of independent CGO groups.
        types = [] # The point types we have seen (usually this will just be 'strains' and 'antisera').
        titerCoords = [] # A list of xyz-tuples, being the coords that will be used to make the titer surface (if one is being made).

        pointsOrder = self.map_data.keys()
        pointsOrder.sort(lambda a,b: cmp(self.map_data[a]['transparency'], self.map_data[b]['transparency']))
        for pointName in pointsOrder:
            point = self.map_data[pointName]
            
            type = point['type']

            # Get the defaults for this point type (i.e., strain, serum).
            for option in ('color', 'shape'):
                if option not in point:
                    point[option] = options[type + ' ' + option]
                    
            pointCoords = Coord(point['coords'])
            pointColor = point['color']
            pointRadius = point.get('radius', options[type + ' radius'])
            pointAlpha = point.get('transparency', None)
            if pointAlpha is not None:
                pointAlpha = 1.0 - pointAlpha # convert transparency to Alpha
				
            # Show the point (strain or serum) shape.
            if options[type + ' compute']:
                if type not in types:
                    types.append(type)
                if type not in cgoData:
                    cgoData[type] = independentPymolCgoGroup(options['cgo prefix'] + options[type + ' cgo name'])

                if pointAlpha is not None:
                    cgoData[type].addAlpha(pointAlpha)
                cgoData[type].setColor(pointColor)
                
                if point['shape'] == 'sphere':
                    cgoData[type].addSphere(pointCoords, pointRadius)
                    if self.bb:
                        self.bb.add(pointCoords, pointRadius)
                elif point['shape'] == 'cube':
                    sideLength = point.get('side length', options[type + ' side length'])
                    cgoData[type].addCube(pointCoords, sideLength)
                    if self.bb:
                        self.bb.add(pointCoords, sideLength / 2.0)
                else:
                    raise MdsError.PointError, "Point '%s' in file '%s' has an unrecognized shape option (%s)." % (pointName, file, point['shape'])
            # cgoData[type].addAlpha(1.0) # reset transparency

            # Show the point (strain or serum) name.
            if options[type + ' names compute']:
                cgoKey = type + ' names'
                if cgoKey not in types:
                    types.append(cgoKey)
                if cgoKey not in cgoData:
                    cgoData[cgoKey] = independentPymolCgoGroup(options['cgo prefix'] + options[cgoKey + ' cgo name'])
                pointNameCoords = pointCoords + options[cgoKey + ' offset']
                cgoData[cgoKey].addWiretext(pointName, pointNameCoords, options[cgoKey + ' color'], options[cgoKey + ' text width'])
                if self.bb:
                    # TODO: this is broken - we don't know the extent of the name.
                    self.bb.add(pointNameCoords)

            # Add the titer line.
            pointTiter = point.get('titer')
            
            if pointTiter is not None:
                # The titer coords (which we compute in any case, as we may use them later in surface computations)
                # are this point's coords, with the titer added to the Z coord.
                titerEndCoords = pointCoords + [0, 0, pointTiter]
                titerCoords.append(titerEndCoords)
                if options['titer compute']:
                    if 'titer' not in cgoData:
                        cgoData['titer'] = independentPymolCgoGroup(options['cgo prefix'] + options['titer cgo name'])
                    cgoData['titer'].setLineWidth(options['titer line width'])
                    cgoData['titer'].addLine(pointCoords, titerEndCoords)
                    if self.bb:
                        self.bb.add(titerEndCoords)

            # Add the Procrustes name, sphere and line, if any
            pointProcrustesCoords = point.get('procrustes coords')
            
            if pointProcrustesCoords is not None:
                if options['procrustes names compute']:
                    if 'procrustes name' in point:
                        pointProcrustesName = point['procrustes names']
                    else:
                        pointProcrustesName = pointName
                    if 'procrustes names' not in cgoData:
                        cgoData['procrustes names'] = independentPymolCgoGroup(options['cgo prefix'] + options['procrustes names cgo name'])
                    procrustesNameCoords = pointProcrustesCoords + options['procrustes names offset']
                    cgoData['procrustes names'].addWiretext(pointProcrustesName, procrustesNameCoords,
                                                            options['procrustes names color'], options['procrustes names text width'])
                    if self.bb:
                        # TODO: this is broken - we don't know the extent of the name.
                        self.bb.add(procrustesNameCoords)

                if options['procrustes spheres compute']:
                    if 'procrustes spheres' not in cgoData:
                        cgoData['procrustes spheres'] = independentPymolCgoGroup(options['cgo prefix'] + options['procrustes spheres cgo name'])
                    cgoData['procrustes spheres'].setColor(options['procrustes spheres color'])
                    cgoData['procrustes spheres'].addSphere(pointProcrustesCoords, options['procrustes spheres radius'])
                    if self.bb:
                        self.bb.add(pointProcrustesCoords, options['procrustes spheres radius'])

                if options['procrustes lines compute']:
                    if 'procrustes lines' not in cgoData:
                        cgoData['procrustes lines'] = independentPymolCgoGroup(options['cgo prefix'] + options['procrustes lines cgo name'])
                    cgoData['procrustes lines'].setColor(options['procrustes lines color'])
                    cgoData['procrustes lines'].setLineWidth(options['procrustes lines width'])
                    cgoData['procrustes lines'].addLine(pointCoords, pointProcrustesCoords)
                    if self.bb:
                        self.bb.add(pointProcrustesCoords)

            # Add the blobs around the point.
            blobs = point.get('blobs')
            
            if blobs is not None:
                cgoKey = type + ' blobs'
                if options[cgoKey + ' compute']:
                    if cgoKey not in cgoData:
                        cgoData[cgoKey] = independentPymolCgoGroup(options['cgo prefix'] + options[cgoKey + ' cgo name'])
                    blobStyle = options[cgoKey + ' style']
                    blobZ = 0.0 # How much to add to the z component of each blob
                    blobZInc = options[cgoKey + ' z inc']
                    for blob in blobs:
                        blobZ += blobZInc
                        if blobStyle == 'triangleFan':
                            # The first blob vertex is at the center (i.e., where the strain/serum is).
                            cgoData[cgoKey].startTriangleFan()
                            alpha = blob.get('transparency', None)
                            if alpha is not None:
                                cgoData[cgoKey].addAlpha(alpha)
                            coords = Coord(pointCoords)
                            coords += (0.0, 0.0, blobZ)
                            cgoData[cgoKey].setColor(pointColor)
                            cgoData[cgoKey].addVertex(coords)
                            for vertex in blob['points']:
                                coords = Coord(vertex['coord'])
                                coords += (0.0, 0.0, blobZ)
                                cgoData[cgoKey].setColor(vertex['color'])
                                cgoData[cgoKey].addVertex(coords)
                            # Add the first point in the fan again (in order to close it).
                            vertex = blob['points'][0]
                            coords = Coord(vertex['coord'])
                            coords += (0.0, 0.0, blobZ)
                            cgoData[cgoKey].setColor(vertex['color'])
                            cgoData[cgoKey].addVertex(coords)
                            # Finish the fan.
                            cgoData[cgoKey].endTriangleFan()
                        elif blobStyle == 'contours':
                            cgoData[cgoKey].startLineLoop()
                            cgoData[cgoKey].setLineWidth(options[cgoKey + ' contours line width'])
                            alpha = blob.get('transparency', None)
                            if alpha is not None:
                                cgoData[cgoKey].addAlpha(alpha)
                            for vertex in blob['points']:
                                coords = Coord(vertex['coord'])
                                coords += (0.0, 0.0, blobZ)
                                cgoData[cgoKey].setColor(vertex['color'])
                                cgoData[cgoKey].addVertex(coords)
                            # Finish the loop.
                            cgoData[cgoKey].endLineLoop()
                        elif blobStyle == 'dots':
                            alpha = blob.get('transparency', None)
                            radius = options[cgoKey + ' dots radius']
                            if alpha is not None:
                                cgoData[cgoKey].addAlpha(alpha)
                            for vertex in blob['points']:
                                coords = Coord(vertex['coord'])
                                coords += (0.0, 0.0, blobZ)
                                cgoData[cgoKey].setColor(vertex['color'])
                                cgoData[cgoKey].addSphere(coords, radius)
                        else:
                            raise Exception, "Unrecognized %s blob style (%s)." % (type, blobStyle)

            # Do error lines, prediction lines, connection lines, etc.
            for lineType in ('error lines', 'prediction lines', 'connection lines'):
                lines = point.get(lineType)

                if lines and options[lineType + ' compute']:
                    if lineType not in cgoData:
                        cgoData[lineType] = independentPymolCgoGroup(options['cgo prefix'] + options[lineType + ' cgo name'])
                    for errorSpec in lines:
                        cgoData[lineType].setColor(errorSpec['color'])
                        cgoData[lineType].setLineWidth(options['error lines width'])
                        cgoData[lineType].addLine(pointCoords, errorSpec['coord'])

                        
        # All points have now been seen.
                        
        if options['surface compute']:
            gridData = None
            if options['surface method'] == 'precomputed':
                gridData = options['surface precomputed data']
            elif titerCoords:
                if options['surface method'] == 'gnuplot':
                    from Gnuplot import grid
                    # Use a bounding box to get the limits on the titer data.
                    bb = BoundingBox(options)
                    for point in titerCoords:
                        bb.add(point)
                    rows, cols = bb.computeGridRowsCols(options['surface gnuplot density'])
                    gridData = grid(options['surface raw data'], rows, cols, options['surface gnuplot norm'])
                    # print "coords: %s\nrows %s\ncols %s" % (str(titerCoords), rows, cols)
                elif options['surface method'] == 'r':
                    from R import rGrid
                    # Use R to generate a surface.
                    rows, cols = options['surface rows'], options['surface cols']
                    if options['surface vaccine'] and options['surface vaccine'] in self.map_data:
                        focus = self.map_data[options['surface vaccine']]['coords'][0:2]
                    else:
                        # We weren't give an vaccine, just use the coords from the
                        # first of the titer values and issue a warning.
                        print >>sys.stderr, "Surface vaccine not specified (or not found)! Using coords of first point with a titer for the R fitting focal position. Pass a 'surface vaccine' option to change this."
                        focus = titerCoords[0][0:2]
                    gridData = rGrid(titerCoords, options['surface r fit'], focus, rows, cols)
                else:
                    raise MdsError.NoSuchMethod, "Unknown surface method option: '%s'." % options['surface method']
        
            # print "rows, cols = %d, %d" % (rows, cols)
            # print "gridded data is: %s" % str(gridData)

            # The points come back from gnuplot with Y changing first (increasing)
            # and then X changing (decreasing).
            #
            # So if you think of the typical X,Y plane, the order of points from gnuplot
            # is start at the bottom right, go up to max y, then step left & return to
            # bottom, and continue until you reach x min, and move up to the top left point.
            #
            # Data that is given to us in options['surface gridded data'] is expected to
            # follow the same format.
            #
            # With that understanding, we know how to get triangles, quadrilaterals, etc.

            if gridData:
                # Add surface spheres.
                if options['surface spheres compute']:
                    radius = options['surface spheres radius']
                    cgoData['surface spheres'] = independentPymolCgoGroup(options['cgo prefix'] + options['surface spheres cgo name'])
                    cgoData['surface spheres'].setColor(options['surface spheres color'])
                    for point in gridData:
                        # print "grid point = %s" % str(point)
                        cgoData['surface spheres'].addSphere(point, radius)
                        if self.bb:
                            self.bb.add(point, radius)

                # Add the triangle mesh.
                if options['surface triangles compute']:
                    cgoData['surface triangles'] = independentPymolCgoGroup(options['cgo prefix'] + options['surface triangles cgo name'])
                    cgoData['surface triangles'].setColor(options['surface triangles color'])
                    for col in xrange(cols - 1):
                        cgoData['surface triangles'].startTriangleStrip()
                        # Add the bottom right and bottom left vertices of the initial triangle.
                        cgoData['surface triangles'].addVertex(gridData[col * rows])
                        cgoData['surface triangles'].addVertex(gridData[(col + 1) * rows])

                        for row in xrange(1, rows):
                            # Add the right and left vertices of the next row up.
                            cgoData['surface triangles'].addVertex(gridData[col * rows + row])
                            cgoData['surface triangles'].addVertex(gridData[(col + 1) * rows + row])

                        cgoData['surface triangles'].endStrip()

                # Add the quadrilateral mesh.
                if options['surface quads compute']:
                    cgoData['surface quads'] = independentPymolCgoGroup(options['cgo prefix'] + options['surface quads cgo name'])
                    cgoData['surface quads'].setColor(options['surface quads color'])
                    for col in xrange(cols):
                        cgoData['surface quads'].startLineStrip()
                        for row in xrange(rows):
                            cgoData['surface quads'].addVertex(gridData[col * rows + row])
                        cgoData['surface quads'].endStrip()
                    for row in xrange(rows):
                        cgoData['surface quads'].startLineStrip()
                        for col in xrange(cols):
                            cgoData['surface quads'].addVertex(gridData[col * rows + row])
                        cgoData['surface quads'].endStrip()

                # Add the titer plane (note that this only happens if we're computing a surface & there's surface data).
                if options['titer plane compute']:
                    cgoData['titer plane'] = TiterPlane(options['titer plane titer'],
                                                        options['cgo prefix'] + options['titer plane cgo name'],
                                                        options['titer plane color'],
                                                        options['titer plane line width'],
                                                        options['titer plane x min'],
                                                        options['titer plane x max'],
                                                        options['titer plane y min'],
                                                        options['titer plane y max'],
                                                        options['titer plane titer']).cgo()

                
        # Deal with bounding box related options.
        if self.bb:
            for what, func in (('bounding box', self.bb.computeBoundingBox),
                               ('grid', self.bb.computeGrid),
                               ('notches', self.bb.computeNotches),
                               ('dots', self.bb.computeDots)):
                if options[what + ' compute']:
                    cgoData[what] = func()

        # Load all the cgo sets (sorted by cgo name, so they appear alphabetically down the GUI rhs).
        def cgoCmp(a, b):
            return cmp(cgoData[a].name(), cgoData[b].name())
        
        cgoSets = cgoData.keys()
        cgoSets.sort(cgoCmp)
        for cgo in cgoSets:
            cgoData[cgo].load()

        # Set transparency.
        for cgo in cgoData:
            if cgo + ' transparency' in options:
                cgoData[cgo].setTransparency(options[cgo + ' transparency'])

        # Hide those that should be initially hidden.

        # TODO: make sure we really need to do this in two phases now that the type names
        #       have had the extra space removed from their cgoData keys.

        # 1) the types we found (e.g., strains, antisera), and their names.
        #
        # NOTE: we remove these cgo sets from cgoData at this point. We're done with them,
        # and it makes the subsequent CGO group hiding code a little simpler.
        for type in types:
            if options[type + ' hidden']:
                cgoData[type].hide()
            cgoData.pop(type)

        # 2) the other cgo sets created above.
        for cgo in cgoData:
            if options[cgo + ' compute'] and options[cgo + ' hidden']:
                cgoData[cgo].hide()

    # Unused
    def boundingBox(self):
        return self.bb
