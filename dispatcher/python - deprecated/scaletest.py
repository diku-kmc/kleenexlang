# coding=utf-8

import datetime
import getopt
import json
import os
import sys
import cStringIO
from subprocess import Popen, PIPE

DEFAULT_SIZE = 1024
PRINT_HELP_RET_CODE = 1
PRINT_ERROR_RET_CODE = 2
PROG_ERROR_RET_CODE = 3

stateTable = []
name = os.path.basename(__file__)


def u_line(s):
    return '\033[4m' + s + '\033[0m'


def bold(s):
    return '\033[1m' + s + '\033[0m'


def usage():
    print bold("Usage:")
    print 'Normal usage: ' + name + ' < program > < in-file >'
    print '- ' + name + ' < program > < in-file > --size < n >   : Chunks input into chunks of size n bytes.'
    print '- ' + name + ' < program > < in-file > --chunks < n > : Chunks input into n chunks of equal size.'
    print bold('- default usage = ') + name + ' < program > < in-file > --size ' + str(DEFAULT_SIZE)


def usage_exit(code):
    usage()
    sys.exit(code)


def get_file_size(filename):
    stat_info = os.stat(filename)
    return stat_info.st_size


def flush_close(s):
    s.flush()
    s.close()


def did_accept(phase, state):
    if phase >= len(stateTable):
        print "Error: Invalid phase number '" + str(len(stateTable)) + "'. " \
              + name + " only has " + str(len(stateTable) - 1) + " phases."
        sys.exit(PROG_ERROR_RET_CODE)

    if state == -1:
        return False

    for s in stateTable[phase]:
        if s["state"] == state:
            return s["accepting"]

    print "Error: State '" + str(state) + "' does not exist in phase '" + str(phase) + "'."
    raise sys.exit(PROG_ERROR_RET_CODE)


def handle_cmd_args(args):
    """
    Add description

    :param args: Collection of command line arguments. 
    :return: An instance of the CommandLineArguments class representing the parsed arguments. 
    """
    if len(args) < 2:
        print "Invalid number of arguments."
        usage_exit(PRINT_ERROR_RET_CODE)

    prog = args[0]
    inp = args[1]

    if len(args) == 2:
        return CommandLineArgs(prog, inp)

    size = chunks = 0

    try:
        opts, args = getopt.getopt(args[2:], "hs:c:", ["help", "size=", "chunks="])
    except getopt.GetoptError as err:
        print err
        usage()
        sys.exit(PRINT_ERROR_RET_CODE)
    for opt, arg in opts:
        if opt in ('-h', '--help'):
            usage_exit(PRINT_HELP_RET_CODE)
        if opt in ('-s', '--size'):
            if arg.isdigit():
                size = int(arg)
                if size <= 0:
                    print 'Invalid value for ' + bold('--size') + ' value must be greater than 0.'
                    usage_exit(PRINT_ERROR_RET_CODE)
            else:
                print 'Invalid argument for ' + bold('--size') + ' value must be an integer.'
                usage_exit(-1)
        elif opt in ('-c', '--chunks'):
            if arg.isdigit():
                chunks = int(arg)
                if chunks <= 0:
                    print 'Invalid value for ' + bold('--chunks') + ' value must be greater than 0.'
                    usage_exit(PRINT_ERROR_RET_CODE)
            else:
                print 'Invalid argument for ' + bold('--chunks') + ' value must be an integer.'
                usage_exit(PRINT_ERROR_RET_CODE)
        else:
            if args == "":
                print "Unknown argument '" + bold(opt) + "'."
            else:
                print "Unknown argument '" + bold(opt) + "', with value '" + bold(arg) + "'."
            usage_exit(PRINT_ERROR_RET_CODE)

    if size != 0 and chunks != 0:
        print 'Invalid combination of arguments: [ ' + bold('-s | --size') + ' ] and [' + bold('-c | --chunks') + ' ].'
        print 'Only one argument can be specified at any time.'
        usage_exit(PRINT_ERROR_RET_CODE)

    return CommandLineArgs(prog, inp, size, chunks, size > 0)


def start_progs(progs, s):
    for p in progs:
        p.stdin.write(s)
        flush_close(p.stdin)


def state_arg(p, s):
    return ['-p', str(p), '-s', str(s["state"])]


def map_output(chunks):
    out = []

    for ps in chunks:
        o = []
        for p in ps:
            p.stdin.close()
            p.wait()
            res = p.stdout.read().split()
            o.append(int(res[3]))
        out.append(o)
    return out


def find_path(state, paths):
    if state == -1:
        return -1

    if len(paths) == 0:
        return state
    print state, paths[0]

    if len(paths[0]) <= state:
        print ("Error: State '" + str(state) + "' was not found")
        sys.exit(PROG_ERROR_RET_CODE)
    return find_path(paths[0][state], paths[1:])


def handle_single_chunk(args):
    global stateTable
    start = datetime.datetime.now()
    f = open(args.input, 'r')
    s = f.read()
    p = Popen([args.prog], stdin=PIPE, stdout=PIPE)
    start_progs([p], s)

    #flush_close(p.stdin)
    p.wait()
    state = int(p.stdout.read().split()[3])
    #print ("accept" if did_accept(1, state) else "reject")
    print datetime.datetime.now() - start
    sys.exit()


def main(argv):
    """
    Add description

    :param argv: 
    :return: 
    """
    global stateTable

    args = handle_cmd_args(argv[1:])
    file_size = get_file_size(args.input)

    if args.useSize:    # Calculate number of chunks needed
        args.chunks = file_size / args.size

    else:               # Calculate size of each chunk
        args.size = file_size / args.chunks

    p = Popen([args.prog, '-o'], stdout=PIPE)
    p.wait()

    stateTable = json.loads(p.stdout.read())

    if args.chunks == 1:
        handle_single_chunk(args)

    f = open(args.input, 'r')
    start = datetime.datetime.now()
    chunks = [[Popen([args.prog], stdin=PIPE, stdout=PIPE)]]

    start_progs(chunks[0], f.read(args.size))

    for c in range(1, args.chunks-1):
        #s = f.read(args.size)
        cs = []
        cs.append(Popen([args.prog] + state_arg(1, stateTable[1][0]), stdin=PIPE, stdout=PIPE))
        #cs[-1].stdin = cStringIO.StringIO(s)

        chunks.append(cs)
        start_progs(cs, f.read(args.size))

    cs = []
    cs.append(Popen([args.prog] + state_arg(1, stateTable[1][0]), stdin=PIPE, stdout=PIPE))

    chunks.append(cs)
    start_progs(cs, f.read())

    paths = map_output(chunks)

    # print paths
    s = paths[-1][0] # find_path(0, paths)
    print datetime.datetime.now() - start

    # print ("accept" if did_accept(1, s) else "reject")


class CommandLineArgs:
    """ 
    Simple struct representing the parsed command line arguments.
    """

    def __init__(self, prog, input, size=DEFAULT_SIZE, chunks=0, use_size=True):
        self.size = size
        self.chunks = chunks
        self.useSize = use_size
        self.prog = prog
        self.input = input

    def __str__(self):
        return '{ "size" : ' + str(self.size) + \
               ', "chunks" : ' + str(self.chunks) + \
               ', "useSize" : ' + str(self.useSize) + \
               ', "prog" : ' + self.prog + \
               ', "input" : ' + self.input + \
               ' }'


# Check if file is run as main
if __name__ == "__main__":
    main(sys.argv)
