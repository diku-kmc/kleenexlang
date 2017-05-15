# coding=utf-8

import getopt
import datetime
from numpy import mean, sqrt, abs, array

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


def handle_cmd_args(args):
    """
    Add description

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

    lower = 0
    upper = 1
    count = 100
    step = 1
    try:
        opts, args = getopt.getopt(args[2:], "hl:u:c:s:", ["help", "min=", "max=", "step=", "count="])
    except getopt.GetoptError as err:
        print err
        usage()
        sys.exit(PRINT_ERROR_RET_CODE)
    for opt, arg in opts:
        if opt in ('-h', '--help'):
            usage_exit(PRINT_HELP_RET_CODE)
        if opt in ('-u', '--max'):
            if arg.isdigit():
                upper = int(arg)
                if upper <= 0:
                    print 'Invalid value for ' + bold('--max') + ' value must be greater than 0.'
                    usage_exit(PRINT_ERROR_RET_CODE)
            else:
                print 'Invalid argument for ' + bold('--max') + ' value must be an integer.'
                usage_exit(-1)
        elif opt in ('-l', '--min'):
            if arg.isdigit():
                lower = int(arg)
                if lower < 0:
                    print 'Invalid value for ' + bold('--min') + ' value must be non negative.'
                    usage_exit(PRINT_ERROR_RET_CODE)
            else:
                print 'Invalid argument for ' + bold('--min') + ' value must be an integer.'
                usage_exit(PRINT_ERROR_RET_CODE)
        elif opt in ('-c', '--count'):
            if arg.isdigit():
                count = int(arg)
                if count < 0:
                    print 'Invalid value for ' + bold('--count') + ' value must be non negative.'
                    usage_exit(PRINT_ERROR_RET_CODE)
            else:
                print 'Invalid argument for ' + bold('--count') + ' value must be an integer.'
                usage_exit(PRINT_ERROR_RET_CODE)
        elif opt in ('-s', '--step'):
            if arg.isdigit():
                step = int(arg)
                if step < 0:
                    print 'Invalid value for ' + bold('--step') + ' value must be non negative.'
                    usage_exit(PRINT_ERROR_RET_CODE)
            else:
                print 'Invalid argument for ' + bold('--step') + ' value must be an integer.'
                usage_exit(PRINT_ERROR_RET_CODE)
        else:
            if args == "":
                print "Unknown argument '" + bold(opt) + "'."
            else:
                print "Unknown argument '" + bold(opt) + "', with value '" + bold(arg) + "'."
            usage_exit(PRINT_ERROR_RET_CODE)

    return prog, inp, lower, upper, step, count


def start_progs(progs, s):
    global suffixStr
    suffixStr = s[-suffix_size:]
    for _, p in progs:
        p.stdin.write(s)
        flush_close(p.stdin)


def suffix_analysis(suffix, prog):
    global stateTable
    state_map = []

    for state in stateTable[1]:
        p = Popen([prog] + state_arg(1, state), stdin=PIPE, stdout=PIPE)
        p.stdin.write(suffix)
        flush_close(p.stdin)
        p.wait()
        r = p.stdout.read().split()[3]
        state_map.append((state["state"], int(r)))

    final_states = unique([t[1] for t in state_map])
    return final_states


def main(argv):
    """
    Add description

    :param argv: 
    :return: 
    """
    global stateTable
    prog, path, lower, upper, step, count = handle_cmd_args(argv[1:])
    file_size = get_file_size(path)

    stateTable = get_state_table(prog)

    f = open(path)

    content = f.read()
    f.close()

    # 2D array ['suffix length', 'mean', 'min', 'max', 'std']
    res = []

    for i in range(lower, upper+1, step):
        print i, datetime.datetime.now()
        state_count = []

        # Handles lower = 0
        if i == 0:
            n = len(stateTable[1])
            res.append([0, n, n, n, 0])
            continue

        for j in range(count):
            states = suffix_analysis(content[j*i:(j+1)*i], prog)
            state_count.append(len(states))

        nparr = array(state_count)
        std = sqrt(mean(abs(nparr - nparr.mean())**2))
        avg = sum(state_count) / float(len(state_count))
        res.append([i, avg, min(state_count), max(state_count), std])

    print res
    name = path[:-4] + "_".join([str(lower), str(upper), str(step), str(count)]) + ".csv"

    f = open(name,'w')

    f.write("size; mean; min; max; std\n")
    f.writelines([";".join([str(cell) for cell in row]) + "\n" for row in res])


# Check if file is run as main
if __name__ == "__main__":
    main(sys.argv)
