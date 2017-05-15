# coding=utf-8

import getopt
from util import *

DEFAULT_SIZE = 1024
PRINT_HELP_RET_CODE = 1
PRINT_ERROR_RET_CODE = 2
PROG_ERROR_RET_CODE = 3


stateTable = []
name = os.path.basename(__file__)
suffixStr = ""
suffix_size = 4


def usage():
    print bold("Usage:")
    print 'Normal usage: ' + name + ' < program > < in-file >'
    print '- ' + name + ' < program > < in-file > --size < n >   : Chunks input into chunks of size n bytes.'
    print '- ' + name + ' < program > < in-file > --chunks < n > : Chunks input into n chunks of equal size.'
    print bold('- default usage = ') + name + ' < program > < in-file > --size ' + str(DEFAULT_SIZE)


def usage_exit(code):
    usage()
    sys.exit(code)


def did_accept(p, s):
    """
    Checks if state s of phase p is accepting
    
    :param p: integer representing a phase
    :param s: integer representing a state
    :return: 
    True, if state s in phase p is accepting,
    False, otherwise. 
    """
    if p >= len(stateTable):
        print "Error: Invalid phase number '" + str(len(stateTable)) + "'. " \
              + name + " only has " + str(len(stateTable) - 1) + " phases."
        sys.exit(PROG_ERROR_RET_CODE)

    if s == -1:
        return False

    for state in stateTable[p]:
        if state["state"] == s:
            return state["accepting"]

    print "Error: State '" + str(s) + "' does not exist in phase '" + str(p) + "'."
    raise sys.exit(PROG_ERROR_RET_CODE)


def handle_cmd_args(args):
    """
    Parse command line arguments

    :param args: Collection of command line arguments. 
    :return: An instance of the CommandLineArguments class representing the parsed arguments. 
    """
    global suffix_size

    if len(args) < 2:
        print "Invalid number of arguments."
        usage_exit(PRINT_ERROR_RET_CODE)

    prog = args[0]
    inp = args[1]

    if len(args) == 2:
        return CommandLineArgs(prog, inp)

    size = chunks = 0

    try:
        opts, args = getopt.getopt(args[2:], "hs:c:", ["help", "size=", "chunks=", "suffix="])
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
        elif opt == "--suffix":
            if arg.isdigit():
                suffix_size = int(arg)
                if suffix_size <= 0:
                    print 'Invalid value for ' + bold('--suffix') + ' value must be greater than 0.'
                    usage_exit(PRINT_ERROR_RET_CODE)
            else:
                print 'Invalid argument for ' + bold('--suffix') + ' value must be an integer.'
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
    global suffixStr
    suffixStr = s[-suffix_size:]
    for _, p in progs:
        p.stdin.write(s)
        flush_close(p.stdin)


def handle_single_chunk(args):
    global stateTable
    f = open(args.input, 'r')
    p = Popen([args.prog], stdin=f, stdout=PIPE)

    p.wait()
    state = int(p.stdout.read().split()[3])
    print ("accept" if did_accept(1, state) else "reject")
    sys.exit()


def suffix_analysis(suffix, args):
    global stateTable
    state_map = []

    for state in stateTable[1]:
        p = Popen([args.prog] + state_arg(1, state), stdin=PIPE, stdout=PIPE)
        p.stdin.write(suffix)
        flush_close(p.stdin)
        p.wait()
        s = p.stdout.read().split()
        s = s[3]
        state_map.append((state["state"], int(s)))

    #print suffixStr
    #print state_map

    final_states = unique([t[1] for t in state_map if t[1] > 0])
    print final_states
    return final_states


def find_path2(state, paths):
    if state == -1:
        return -1

    if len(paths) == 0:
        return state
    print state, paths[0]

    elm = [p for p in paths[0] if p[0] == state]
    if len(elm) == 1:
        return find_path2(elm[0][1], paths[1:])

    if len(elm) > 0:
        print ("Error: more than one process started at state '" + str(state) + "'.")
        sys.exit(PROG_ERROR_RET_CODE)
    else:
        print ("Error: State '" + str(state) + "' was not found")
        sys.exit(PROG_ERROR_RET_CODE)


def main(argv):
    """
    Add description

    :param argv: 
    :return: 
    """
    global stateTable
    global suffixStr
    args = handle_cmd_args(argv[1:])
    file_size = get_file_size(args.input)

    if args.useSize:  # Calculate number of chunks needed
        args.chunks = file_size / args.size

    else:  # Calculate size of each chunk
        args.size = file_size / args.chunks

    p = Popen([args.prog, '-o'], stdout=PIPE)
    p.wait()

    stateTable = json.loads(p.stdout.read())

    if args.chunks == 1:
        handle_single_chunk(args)

    f = open(args.input, 'r')

    chunks = [[(0, Popen([args.prog], stdin=PIPE, stdout=PIPE))]]

    start_progs(chunks[0], f.read(args.size))

    for c in range(1, args.chunks - 1):
        cs = []
        for s in suffix_analysis(suffixStr, args):
            cs.append((s, Popen([args.prog, '-p', '1', '-s', str(s)], stdin=PIPE, stdout=PIPE)))

        chunks.append(cs)
        start_progs(cs, f.read(args.size))

    cs = []
    for s in suffix_analysis(suffixStr, args):
        cs.append((s, Popen([args.prog, '-p', '1', '-s', str(s)], stdin=PIPE, stdout=PIPE)))

    chunks.append(cs)
    start_progs(cs, f.read())

    paths = map_output(chunks)

    print paths
    s = find_path2(0, paths)
    print ("accept" if did_accept(1, s) else "reject")


# Check if file is run as main
if __name__ == "__main__":
    main(sys.argv)
