from CGO import independentPymolCgoGroup
from Axes import Axes
from Options import options
from Coord import Coord, Vec

red = (1.0, 0.0, 0.0)
green = (0.0, 1.0, 0.0)
blue = (0.0, 0.0, 1.0)
white = (1.0, 1.0, 1.0)

def triangle_debug(app):
    Axes(options)
    cgo = independentPymolCgoGroup('triangle')
    crossProductTriangles(cgo)
    cgo.load()

def crossProductTriangles(cgo):
    X = Vec(2, 1, 3)
    Y = Vec(4, 3, 1)
    Z = Vec(3, 6, 3)
    
    cgo.addWiretext('x', X)
    cgo.addWiretext('y', Y)
    cgo.addWiretext('z', Z)
    
    cgo.startTriangleStrip()

    cgo.setColor(red)
    cgo.addVertex(X)
    norm = X.crossProduct(Y)
    cgo.addNorm(norm)
    
    cgo.setColor(green)
    cgo.addVertex(Y)
    norm = X.crossProduct(Y)
    cgo.addNorm(norm)
    
    cgo.setColor(blue)
    cgo.addVertex((0.0, 0.0, 1.0))
    norm = X.crossProduct(Y)
    cgo.addNorm(norm)
    
    cgo.setColor(red)
    cgo.addVertex((-1.0, 0.0, 0.0))
    norm = X.crossProduct(Y)
    cgo.addNorm(norm)
    
    cgo.endStrip()

def triangles(cgo):
    cgo.startTriangleStrip()
    cgo.setColor(red)
    cgo.addVertex(1.0, 0.0, 0.0)
    #cgo.addNorm((1.0, 0.0, 0.0))
    cgo.setColor(green)
    cgo.addVertex((0.0, 1.0, 0.0))
    #cgo.addNorm((0.0, 1.0, 0.0))
    cgo.setColor(blue)
    cgo.addVertex((0.0, 0.0, 1.0))
    #cgo.addNorm((0.0, 0.0, 1.0))
    cgo.setColor(red)
    cgo.addVertex((-1.0, 0.0, 0.0))
    #cgo.addNorm((-1.0, 0.0, 0.0))
    cgo.endStrip()

def twoSimpleTriangles(cgo):
    X = (1.0, 0.0, 0.0)
    Y = (0.0, 1.0, 0.0)
    Z = (0.0, 0.0, 1.0)
    
    cgo.addTriangle(X, Y, Z,
		    (1.0, 0.0, 0.0), # normX
		    (0.0, 1.0, 0.0), # normY
		    (0.0, 0.0, 1.0), # normZ
		    red, # colorX
		    green, # colorY
		    blue) # colorZ
    
    triangle(cgo, X, Y, Z, red)
    
    X = (-1.0, 0.0, 0.0)
    Y = (0.0, 1.0, 0.0)
    Z = (0.0, 0.0, 1.0)
    
    cgo.addTriangle(X, Y, Z,
		    (-1.0, 1.0, 0.0), # normX
		    (0.0, 1.0, 0.0), # normY
		    (0.0, 0.0, 1.0), # normZ
		    red, # colorX
		    green, # colorY
		    blue) # colorZ
    
    triangle(cgo, X, Y, Z, green)

def triangle(cgo, X, Y, Z, color):
    cgo.setColor(color)
    cgo.startLineLoop()
    cgo.addVertex(X, Y, Z)
    cgo.endLoop()
