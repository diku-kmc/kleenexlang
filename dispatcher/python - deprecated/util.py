# coding=utf-8

import os
import sys
from subprocess import Popen, PIPE
import json


DEFAULT_SIZE = 1024
PRINT_HELP_RET_CODE = 1
PRINT_ERROR_RET_CODE = 2
PROG_ERROR_RET_CODE = 3
SUFFIX_SIZE = 4


def u_line(s):
    return '\033[4m' + s + '\033[0m'


def bold(s):
    return '\033[1m' + s + '\033[0m'


def get_file_size(filename):
    stat_info = os.stat(filename)
    return stat_info.st_size


def flush_close(s):
    s.flush()
    s.close()


def state_arg(p, s):
    return ['-p', str(p), '-s', str(s["state"])]


def gather(s, t):
    g = []
    for elm in s:
        g.append(t[elm])

    return g


def factor(s):
    u = unique(s)
    l = []
    for elm in s:
        l.append(u.index(elm))

    return l, u


def unique(s):
    seen = set()
    seen_add = seen.add
    return [x for x in s if not (x in seen or seen_add(x))]


def map_output(chunks):
    out = []

    for ps in chunks:
        o = []
        for s, p in ps:
            p.wait()
            res = p.stdout.read().split()
            o.append((s, int(res[3])))
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


def get_state_table(prog_name):
    p = Popen([prog_name, '-o'], stdout=PIPE)
    p.wait()

    return json.loads(p.stdout.read())


class CommandLineArgs:
    """ 
    Simple struct representing the parsed command line arguments.
    """

    def __init__(self, prog, input, size=DEFAULT_SIZE, chunks=0, use_size=True, suffix_size=4):
        self.size = size
        self.chunks = chunks
        self.useSize = use_size
        self.prog = prog
        self.input = input
        self.suffix = suffix_size

    def __str__(self):
        return '{ "size" : ' + str(self.size) + \
               ', "chunks" : ' + str(self.chunks) + \
               ', "useSize" : ' + str(self.useSize) + \
               ', "prog" : ' + self.prog + \
               ', "input" : ' + self.input + \
               ', "suffix" : ' + self.suffix + \
               ' }'
