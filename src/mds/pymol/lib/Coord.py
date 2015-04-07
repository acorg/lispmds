#!/usr/bin/env python

import MdsError

class Coord(object):
    def __init__(self, *coords):
	super(Coord, self).__init__()
	if coords is None:
	    self.coords = (0.0,) * 3
	elif len(coords) == 1:
	    if isinstance(coords[0], list) or isinstance(coords[0], tuple):
		self.coords = map(float, coords[0])
	    elif isinstance(coords[0], Coord):
		self.coords = [coords[0].coords[0],
			       coords[0].coords[1],
			       coords[0].coords[2]]
	    else:
		raise MdsError.TypeError, "Unknown type passed to __init__."
	else:
	    self.coords = map(float, coords)

    def __getitem__(self, i):
	return self.coords[i]

    def __nonzero__(self):
	return self.coords[0] != 0.0 or self.coords[1] != 0.0 or self.coords[2] != 0.0

    def __iadd__(self, other):
	for i in range(3):
	    self.coords[i] += other[i]
	return self
    
    def __add__(self, other):
	return self.__class__([self.coords[0] + other[0],
			       self.coords[1] + other[1],
			       self.coords[2] + other[2]])

    def __isub__(self, other):
	for i in range(3):
	    self.coords[i] -= other[i]
	return self
    
    def __sub__(self, other):
	return self.__class__([self.coords[0] - other[0],
			       self.coords[1] - other[1],
			       self.coords[2] - other[2]])
    
    def __imul__(self, val):
	for i in range(3):
	    self.coords[i] *= val
	return self
    
    def __mul__(self, val):
	return self.__class__([self.coords[0] * val,
			       self.coords[1] * val,
			       self.coords[2] * val])

    def __neg__(self):
	return self.__mul__(-1.0)

    def __str__(self):
	return str(self.coords)
    
    def __call__(self):
	return self.coords
    
    def __eq__(self, other):
	# return self.coords[0] == other[0] and self.coords[1] == other[1] and self.coords[2] == other[2]
	if isinstance(other, list):
	    return self.coords == other
	elif isinstance(other, tuple):
	    return self.coords == list(other)
	else:
	    return self.coords == other.coords

class Vec(Coord):
    def __init__(self, *coords):
	super(Vec, self).__init__(*coords)

    def crossProduct(self, other):
	# a x b = [a2b3 - a3b2, a3b1 - a1b3, a1b2 - a2b1].
	return self.__class__([(self[1] * other[2]) - (self[2] * other[1]),
			       (self[2] * other[0]) - (self[0] * other[2]),
			       (self[0] * other[1]) - (self[1] * other[0])])
    
			 
    
if __name__ == '__main__':
    a = Coord((3, 4, 5))
    b = Coord([6, 7, 9])
    c = a + b
    d = Coord(3, 4, 5)
    assert(a[0] == 3)
    assert(a[1] == 4)
    assert(a[2] == 5)
    assert(c == [9, 11, 14])
    assert(a + b + c == [18, 22, 28])
    assert(a == d)
    assert(a * 2 == [6, 8, 10])
    a *= 3
    assert(a == [9, 12, 15])

    a = Vec([1, 0, 0])
    b = Vec([0, 1, 0])
    c = Vec([0, 0, 1])
    assert(a.crossProduct(b) == c)
    assert(c.crossProduct(a) == b)
    assert(b.crossProduct(c) == a)

    assert(b.crossProduct(a) == -c)
    assert(a.crossProduct(c) == -b)
    assert(c.crossProduct(b) == -a)

    assert(a.crossProduct(b) + b.crossProduct(a) == (0, 0, 0))
    assert(not (a.crossProduct(b) + b.crossProduct(a)))

    a -= (1, 0, 0)
    a += (1, 0, 0)
    assert(a.crossProduct(b) == c)

    d = a - (1, 0, 0)
    d += (1, 0, 0)
    assert(d.crossProduct(b) == c)

    # Test creation of an instance from an existing instance
    e = Coord(a)
    assert(e is not a)
    assert(e == a)
