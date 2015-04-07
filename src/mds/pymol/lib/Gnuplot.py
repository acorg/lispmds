#!/usr/bin/env python

import os
import re
import sys
import MdsError

digits = re.compile(r'\d+')

def readResults(f):
    result = f.readlines()
    ret = []
	
    for line in result:
        if line[0] == '#' or len(line) == 1:
	    continue
	d = line.split()
	if len(d) != 4:
	    continue
	ret.append([float(i) for i in d[0:3]])

    return ret

if sys.platform[0:3] == 'win':
    import Utils
    import MdsError

    progname = 'pgnuplot.exe'
    dirs = (('C:\\', 'D:\\'),
	    ('Program Files', 'Progra~1', 'Archivos de Programa', 'Archiv~1'),
	    ('gnuplot', 'Gnuplot'),
	    'bin')
    
    gnuplot = Utils.findFileInDir(progname, dirs)

    if gnuplot is None:
	# Make the error message a bit more friendly by digging back into Utils.
	raise MdsError.NoSuchFile, "Could not find file '%s'. Looked in all of:\n%s" % (progname, '\n'.join(list(Utils._getDirs(dirs))))
    
    print "Found gnuplot in:", gnuplot
				  
    def grid(data, rows, cols, norm=1):
	import tempfile
	import subprocess
	dir = tempfile.mkdtemp()
	infile = os.path.join(dir, 'gnuplot.in')
	outfile = os.path.join(dir, 'gnuplot.out')
	f = file(infile, 'w')
	print >>f, "set dgrid3d %d, %d, %d\nset term table\nset out '%s'\nsplot '-'" % (rows, cols, norm, outfile)
	for d in data:
	    print >>f, d[0], d[1], d[2]
	print >>f, 'e\n'
	f.close()
	subprocess.call([gnuplot, infile])
	assert(os.path.isfile(outfile))
	f = file(outfile, 'r')
	ret = readResults(f)
	f.close()
	os.unlink(infile)
	os.unlink(outfile)
	os.rmdir(dir)
	return ret

elif sys.platform == 'darwin': # Other unices could be added here.
    locations = ('/sw/bin/gnuplot',
		 '/opt/local/bin/gnuplot',
		 '/usr/local/bin/gnuplot',
		 '/usr/bin/gnuplot',
		 '/opt/gnuplot/bin/gnuplot',
                 )

    for gnuplot in locations:
	if os.path.isfile(gnuplot):
	    break
    else:
	raise MdsError.NoSuchFile, "Could not find gnuplot. Tried all of:\n%s" % ('\n'.join(locations))
    
    print "Found gnuplot:", gnuplot

    def grid(data, rows, cols, norm=1):
	fi, fo = os.popen2(gnuplot, 't')
	print >>fi, "set dgrid3d %d, %d, %d\nset term table\nsplot '-'" % (rows, cols, norm)
	for d in data:
	    print >>fi, d[0], d[1], d[2]
	print >>fi, 'e\n'
	fi.close()
	ret = readResults(fo)
	fo.close()
	return ret

else:
    raise NotImplementedError, "Unsupported platform '%s'." % sys.platform

if __name__ == '__main__':
    test = [
	[0.71507080000000001,  -2.7813376999999999, 2.0],
	[-0.88893849999999996, -5.8135032999999998, 2.0],
	[0.25863617999999999,  -4.1207510000000003, 2.0],
	[1.9478784,            -2.9061368000000001, 2.0],
	[-1.0556562,           -5.7530894000000004, 2.0],
	[3.3132891999999998,   -3.0860609999999999, 10.0],
	[3.2769550000000001,   -3.9305973000000001, 2.0],
	[6.1207466000000004,   -7.1687503000000001, 2.0],
	[4.1027484000000003,   -6.9892963999999997, 2.0],
	[3.6995543999999998,   -8.2458150000000003, 2.0],
	[4.7147135999999996,   -12.361901,          2.0],
	[2.5320217999999999,   -12.897228999999999, 2.0],
	[3.5206978000000002,   -10.611929999999999, 2.0],
	[4.3228207000000003,   -5.1519922999999999, 2.0],
	[4.5038660000000004,   -11.105539,          2.0],
	[4.4438496000000001,   -10.388685000000001, 2.0],
	[5.4247810000000003,   -10.350070000000001, 2.0],
	[1.7455409,            -12.825664,          2.0],
	[3.356687,             -10.920032000000001, 2.0],
	[4.8698654000000001,   -11.695454,          2.0],
	[2.8393640000000002,   -11.976704,          2.0],
	[3.6082996999999999,   -12.809428,          2.0],
	[3.8872599999999999,   -10.390299000000001, 2.0],
	[1.903688,             -11.333399,          2.0],
	[1.1013854000000001,   -11.061118,          2.0],
	[4.1304135000000004,   -13.298382999999999, 2.0],
	[5.5940580000000004,   -9.4534789999999997, 2.0]
	]

    g = grid(test, 3, 4)

    print g
