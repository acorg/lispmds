import os
import sys
import optparse
import re

try:
    from subprocess import *
except ImportError:
    print >>sys.stderr, 'You need Python >= 2.4 to run this code. Sorry.'
    raise

selfKeyword = '__self__'
lispKeyword = '__lisp__'

numberRegex = re.compile('^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?$')

def lispQuote(s):
    if s in ('t', 'nil'):
        return s
    if re.search(numberRegex, s):
        return s
    if not s.startswith("'") and not s.startswith('"'):
        return '"' + s.replace('\\', '\\\\').replace('"', '\"') + '"'
    return s


class ClInterface(object):
    def __init__(self, lisp='alisp', loadClInit=True, filterSemicolonLines=True, filterWarnings=True, quietLoad=True, forwardStdin=True, wrapFunction='format'):
        self.cmd = [lisp]
        if quietLoad:
            self.cmd.extend(['-e', '(setq *load-verbose* nil)'])
        if loadClInit:
            try:
                clinit = os.path.join(os.environ['HOME'], '.clinit.cl')
            except KeyError:
                print >>sys.stderr, 'Your environment has no HOME variable.'
                sys.exit(1)
            else:
                if not os.access(clinit, os.R_OK):
                    print >>sys.stderr, 'Your clinit file (%s) cannot be read.' % clinit
                    sys.exit(1)
            self.cmd.extend(['-L', clinit])
                
        self.lispCmds = []
        self.filterSemicolonLines = filterSemicolonLines
        self.filterWarnings = filterWarnings
        self.wrapFunction = wrapFunction

    def finalizeCmd(self):
        progn = []
        if self.lispCmds:
            s = '(progn %s)' % ' '.join(self.lispCmds)
            if self.wrapFunction == 'format':
                s = '(format t "~a~%" ' + s + ')'
            elif self.wrapFunction:
                s = '(%s %s)' % (self.wrapFunction, s)
            progn.extend(['-e', s])
        return self.cmd + progn + ['-kill']

    def cmdString(self):
        return ' '.join(self.finalizeCmd())
            
    def run(self):
        cmd = self.finalizeCmd()
        try:
            p = Popen(cmd, stdin=None, stdout=PIPE, stderr=PIPE, close_fds=True, universal_newlines=True, env=os.environ)
            stdout = [line[:-1] for line in p.stdout.readlines()]
            stderr = [line[:-1] for line in p.stderr.readlines()]
            status = p.wait()
            p.stdout.close()
            p.stderr.close()
            
            if status < 0:
                print >>sys.stderr, ('Child lisp exited with error status %d.' % status) + \
                    ('Lisp command was: %s' % self.cmdString()) + \
                    ('Standard error starts:\n%s\nStandard error ends.\n' % '\n'.join(stderr)) + \
                    ('Standard out starts:\n%s\nStandard out ends.\n' % '\n'.join(stdout))
                sys.exit(status)
        except OSError, e:
            print >>sys.stderr, 'Execution failed:', e
            raise

        if self.filterSemicolonLines:
            stdout = filter(lambda line: len(line) and line[0] != ';', stdout)
        if self.filterWarnings:
            stdout = filter(lambda line: len(line) and not line.startswith('Warning:'), stdout)
            
        return stdout

    def addLispCommand(self, cmd):
        '''Add a lisp command. These will all eventually be wrapped in a (progn ...).'''
        self.lispCmds.append(cmd)

class ClCommand(object):
    def __init__(self, lispCommand, argv, **kwargs):
        self.c = ClInterface(**kwargs)
        self.p = optparse.OptionParser(conflict_handler='resolve')
        self.p.disable_interspersed_args()
        self.lispCommand = '(' + lispCommand
        self.argv = argv
        self.positionalArgs = []
        self.hasOptions = False

    # @staticmethod
    def callback(option, optStr, value, parser, *args, **kwargs):
        if 'keyword' in kwargs:
            kwargs[selfKeyword].lispCommand += ' :' + kwargs[lispKeyword]
        if value is None:
            raise ValueError, "Oops, no value given for option '%s'." % optStr
        kwargs[selfKeyword].lispCommand += ' ' + lispQuote(value)

    callback = staticmethod(callback)

    def addInitialArg(self, arg):
        '''Add a mandatory initial (positional) argument.'''
        self.positionalArgs.append(arg)

    def addOption(self, *args, **kwargs):
        if selfKeyword in kwargs:
            raise KeyError, "The reserved key '%s' appears in the passed options." % selfKeyword
        kwargs[selfKeyword] = self
        if lispKeyword in kwargs:
            raise KeyError, "The reserved key '%s' appears in the passed options." % lispKeyword
        args = list(args)
        kwargs[lispKeyword] = args[0]
        # Put the lower case version of each keyword into the possible args
        for arg in args:
            if not arg.islower():
                lower = arg.lower()
                if lower not in args:
                    args.append(lower)
        kw = { 'action' : 'callback', 'callback' : self.callback, 'type' : 'string', 'callback_kwargs' : kwargs }
        self.p.add_option(*args, **kw)
        self.hasOptions = True
        
    def run(self):
        initialArgsError = False
        if self.positionalArgs:
            positionalArgsUsage = ' ' + ' '.join(self.positionalArgs)
        else:
            positionalArgsUsage = ''
        i = 1
        nargs = len(self.argv)
        while i < nargs:
            if self.argv[i].startswith('-'):
                break
            self.lispCommand += ' ' + lispQuote(self.argv[i])
            i += 1
        if i - 1 < len(self.positionalArgs):
            print >>sys.stderr, 'You have supplied only %d of the %d mandatory initial arguments:%s' % \
                (i - 1, len(self.positionalArgs), positionalArgsUsage)
            initialArgsError = True
            
        if self.hasOptions:
            self.p.set_usage('%%prog%s [options]' % positionalArgsUsage)
        else:
            self.p.set_usage('%%prog%s' % positionalArgsUsage)

        self.p.add_option('--verbose', action='store_true', dest='verbose', default=False)
        self.p.add_option('--noexec', action='store_true', dest='noexec', default=False)
        
        (options, args) = self.p.parse_args(self.argv[i:])

        if initialArgsError:
            sys.exit(1)

        for a in args:
            self.lispCommand += ' ' + lispQuote(a)
        self.lispCommand += ')'
        self.c.addLispCommand(self.lispCommand)
        
        if options.noexec:
            options.verbose = True
        if options.verbose:
            print >>sys.stderr, 'Command to run: %s' % self.c.cmdString()
        if not options.noexec:
            stdout = self.c.run()
            if stdout:
                print '\n'.join(stdout)
        
if __name__ == '__main__':
    c = ClInterface()
    c.addLispCommand('(setq hello "world")')
    c.addLispCommand('(format t "Yes yes yes~%")')
    # c.addLispCommand('(setq x (read *STANDARD-INPUT*)) (format t "Read: ~a~%" x)')
    # print c.finalizeCmd()
    results = c.run()
    for i in results:
        print i
