import Utils

dirs = (('C:\\', 'D:\\'),
        ('Program Files', 'Progra~1', 'Archivos de Programa', 'Archiv~1'))

locations = '\n'.join(list(Utils._getDirs(dirs)))

def getMdsRoot():
    # Return the dir we found (might be None) and a string of the locations
    # we looked in (for error messages etc).
    return (Utils.findFileInDir('mds', dirs), locations)
