class MdsError(Exception): pass
class ExecError(MdsError): pass
class PointError(MdsError): pass
class NoSuchFile(MdsError): pass
class NoSuchDir(MdsError): pass
class NoSuchMethod(MdsError): pass
class TypeError(MdsError): pass
class NoRootError(MdsError): pass
