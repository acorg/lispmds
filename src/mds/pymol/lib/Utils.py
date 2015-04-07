#!/usr/bin/env python

import math
import os

def frange(first, last, step=1.0):
    while first <= last:
	yield first
	first += step

def log2(x):
    # Let regular exceptions catch errors.
    return math.log10(x) / math.log10(2.0)

def _getDirs(dirList):
    indices = []
    lens = []
    lists = []
    
    for d in dirList:
	if (isinstance(d, str)):
	    lists.append((d,))
	else:
	    lists.append(d)
	indices.append(0)
	lens.append(len(lists[-1]))

    while True:
	# Yield the current path.
	path = ''
	j = 0
	while j < len(lists):
	    path = os.path.join(path, lists[j][indices[j]])
	    j += 1
	yield path

	# Now increment the counter to prepare for next time. Raise StopIteration if we're done.
	col = len(lists) - 1
	while col >= 0:
	    indices[col] += 1
	    if indices[col] < lens[col]:
		break
	    # We just exhausted this column. Set it to 0 and move left if possible.
	    indices[col] = 0
	    col -= 1
	else:
	    raise StopIteration

def findFileInDir(fileList, dirList):
    '''
    
    fileList is a list of acceptable file names. If any one of these can be
    found, it is returned.

    dirList is a list of lists. Each element of the list gives a set of
    alternate (string) directory names. Any one of these is ok to use. As a
    convenience, an element of dirList may be a string instead of a list of
    length one.

    I know, not a very clear description of what we do. Simpler: find a file
    in a given set of possible directories.
    
    '''
    if (isinstance(fileList, str)):
	fileList = (fileList,)

    for dir in _getDirs(dirList):
	for file in fileList:
	    full = os.path.join(dir, file)
	    if os.path.exists(full):
		return full

if __name__ == '__main__':
    import sys
    got = list(_getDirs((
		('a', 'b'),
		"c",
		('d', 'e', 'f')
		)))
    correct = (('a', 'c', 'd'),
	       ('a', 'c', 'e'),
	       ('a', 'c', 'f'),
	       ('b', 'c', 'd'),
	       ('b', 'c', 'e'),
	       ('b', 'c', 'f'))

    result = []
    for c in correct:
        result.append(os.path.join(*c))

    assert got == result

    if not sys.platform.startswith('win'):
	assert '/etc/passwd' == findFileInDir('passwd',
					      ('/',
					       ('bin', 'usr', 'etc'),
					       ))
    
	assert '/usr/bin/env' == findFileInDir(('env',),
					       ('/',
						('bin', 'usr', 'etc'),
						('usr', 'bin', 'etc'),
						))
    
