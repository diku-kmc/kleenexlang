import matplotlib.pyplot as plt
import os
import sys
import getopt
from math import floor, ceil
from decimal import Decimal

plt.interactive(False)

PRINT_HELP_RET_CODE = 1
PRINT_ERROR_RET_CODE = 2
PROG_ERROR_RET_CODE = 3
name = os.path.basename(__file__)


def tab(n):
    return '\t' * n


def u_line(s):
    return '\033[4m' + s + '\033[0m'


def bold(s):
    return '\033[1m' + s + '\033[0m'


def usage():
    print bold("Usage:")
    print '- ' + name + ' < in-file > [options]: Chunks input into chunks of size n bytes.\n'
    print '-- Options:'
    print tab(1) + '[ -u | --usr ] : Removes user time from plot.'
    print tab(1) + '[ -s | --sys ] : Removes system time from plot.'
    print tab(1) + '[ -w | --wall ] : Removes wall time from plot.'
    print '\nNOTE: -' + name + '< in-file > -u -s -w : will not produce any plot.'


def plot_curve(title, x_label, y_label, graph_labels, x, *ys):
    """
    Plots and displays a graph.
    :param graph_labels: List of labels for each graph, should contain one for each graph in ys.
    :param title: Title of graph.
    :param x_label: Label for x-axis
    :param y_label: Label for y-axis
    :param x: X-axis values
    :param ys: N lists of y-axis values
    """
    if title:
        plt.title(title)
    if x_label:
        plt.xlabel(x_label)
    if y_label:
        plt.ylabel(y_label)

    for val in x:
        plt.axvline(x=val, ls='-', color='#E8E8E8')

    min_y = 1000000
    max_y = -100000
    for y in ys:
        min_y = (min(y) if min(y) < min_y else min_y)
        max_y = (max(y) if max(y) > max_y else max_y)

    for val in range(int(floor(min_y)),int(ceil(max_y))):
        plt.axhline(y=val, ls='-', color='#E8E8E8')

    for y, l in zip(ys, graph_labels):
        plt.plot(x, y, '-', label=l)

    plt.grid(True, which='both')
    plt.legend(loc=4)
    plt.show()


def file_exists(file_path):
    return file_path and os.path.isfile(file_path)


def usage_exit(code):
    usage()
    sys.exit(code)


def handle_cmd_args(args):
    if len(args) == 0:
        print "Invalid number of arguments."
        usage_exit(PRINT_ERROR_RET_CODE)

    path = args[0]
    system = True
    wall = True
    user = True


    try:
        opts, args = getopt.getopt(args[2:], "husw", ["help", "usr", "sys", "wall"])
    except getopt.GetoptError as err:
        print err
        usage()
        sys.exit(PRINT_ERROR_RET_CODE)
    for opt, arg in opts:
        if opt in ('-h', '--help'):
            usage_exit(PRINT_HELP_RET_CODE)
        if opt in ('-s', '--sys'):
            system = False
        elif opt in ('-w', '--wall'):
            wall = False
        elif opt in ('-u','--usr'):
            user = False
        else:
            if arg == "":
                print "Unknown argument '" + bold(opt) + "'."
            else:
                print "Unknown argument '" + bold(opt) + "', with value '" + bold(arg) + "'."
            usage_exit(PRINT_ERROR_RET_CODE)

    return path, user, system, wall


def parse_next(f):
    f.readline()
    f.readline()
    wall = Decimal(f.readline().split()[1])
    user = Decimal(f.readline().split()[1])
    syss = Decimal(f.readline().split()[1])
    return wall, user, syss


def parse_file(f):
    interval = f.readline().split(':')
    xs = range(int(interval[0]), int(interval[1])+1)
    walls = []
    syss = []
    users = []

    for x in xs:
        wall, user, syst = parse_next(f)
        walls.append(wall)
        users.append(user)
        syss.append(syst)

    return xs, walls, syss, users


def main(argv):
    path, user, system, wall = handle_cmd_args(argv[1:])

    if not file_exists(path):
        print "Unable to located file '" + path + "'."
        sys.exit(PRINT_ERROR_RET_CODE)

    f = open(path)

    x, w, u, s = parse_file(f)

    plot_curve("Timing: " + path, "chunks", "sec.", ['wall', 'user', 'sys'], x, w, u, s)


if __name__ == "__main__":
    main(sys.argv)