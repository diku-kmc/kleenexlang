#!/usr/bin/env python
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np
import argparse
import glob
import re
import locale
import os
import errno
import warnings
import csv

def colored(msg, color):
    color_code = { 'green'   : "\033[32m",
                   'yellow'  : '\033[33m',
                   'red'     : '\033[31m',
                   'cyan'    : '\033[36m'
    }
    try:
        beg = color_code[color]
        end = '\033[0m'
        return beg + msg + end
    except KeyError:
        return msg

def trans_Gbit_per_s(inputsize_bytes):
    return { "median_format_string" : "%.3f",
             "trans_fun" : lambda ps : map(lambda p : ((inputsize_bytes * 8) / 1e9) / (p / 1000.0), ps),
             "yaxis_label" : "GBit/s",
             "title" : lambda prog, inputname : "%s (%s %.2f MB)" %
             (prog, inputname, inputsize_bytes / 2.0**20)
         }
def trans_Mbit_per_s(inputsize_bytes):
    return { "median_format_string" : "%.1f",
             "trans_fun" : lambda ps : map(lambda p : ((inputsize_bytes * 8) / 1e6) / (p / 1000.0), ps),
             "yaxis_label" : "MBit/s",
             "title" : lambda prog, inputname : "%s (%s %.2f MB)" %
             (prog, inputname, inputsize_bytes / 2.0**20)
         }
def trans_s(inputsize_bytes):
    return { "median_format_string" : "%d",
             "trans_fun" : lambda ps : map(lambda p : p / 1000, ps),
             "yaxis_label" : "Time (s)",
             "title" : lambda prog, inputname : "%s (%s %.2f MB)" %
             (prog, inputname, inputsize_bytes / 2.0**20)
         }
def trans_ms(inputsize_bytes):
    return { "median_format_string" : "%d",
             "trans_fun" : lambda ps : ps,
             "yaxis_label" : "Time (ms)",
             "title" : lambda prog, inputname : "%s (%s %.2f MB)" %
             (prog, inputname, inputsize_bytes / 2.0**20)
         }

transformations = {
    "Gbit/s" : trans_Gbit_per_s,
    "Mbit/s" : trans_Mbit_per_s,
    "s"      : trans_s,
    "ms"     : trans_ms
}

# Default base_dir is the directory of this script.
base_dir = os.path.realpath("__file__")
# Default plot_dir is plots/
plot_dir = os.path.join(os.path.dirname(os.path.realpath("__file__")), "plots")
# Don't override plots if they already exist.
force_override = False
# Don't print so much gunk.
is_verbose = False

def verbose_print(msg):
    if is_verbose:
        print colored(msg, 'yellow')

def warning_print(msg):
    print colored(msg, 'red')

def notice_print(msg):
    print colored(msg, 'cyan')

def default_version_name():
    return "DEFAULT"

def data_dir(impl, prog):
    return os.path.join(os.path.dirname(base_dir), impl, "times", prog)

def test_data_dir():
    return os.path.join(os.path.dirname(base_dir), "..", "test", "data")

def go(progs = [], skip = None, default_transformation = "ms"):
    conf, inputs, skips = get_benchmark_configuration()
    if skip != None: # Override whatever is read from plots.txt
        if type(skip) == list:
            skipFun = lambda p, i, n : i in skip
        elif type(skip) == dict:
            skipFun = lambda p, i, n : i in skip[p]
        else:
            raise Exception("If set, skip must be a list or a dictionary!")
    else: # Then we do not override the skip map from plots.txt
        def f(p, i, n): # program name, implementation name, output name
            try: return i in skips[p][n]['skip']
            except KeyError: return False
        skipFun = f
    
    def getTransFun(p, n):
        try: return transformations[skips[p][n]['transformation']]
        except KeyError:
            return transformations[default_transformation]

    plot_all(get_data(conf, progs), inputs, skips, skipFun, getTransFun)

def get_data(conf, only_progs = []):
    benchmarks = {}
    for prog, impls in conf.iteritems():
        if only_progs != [] and not prog in only_progs:
            continue
        benchmarks[prog] = {}
        for impl in impls:
            benchmarks[prog][impl] = {}
            # Really ugly hack!  Because of csv2json and csv2json_nows, which only
            # overlap for Kleenex as they are only implemented there...
            if impl == "kleenex" and prog == "csv2json":
                sp = "%s.kex" % prog
            else:
                sp = prog
            # Get timing info from impl/time/prog/*
            timing_dirs = filter(os.path.isdir, glob.glob(data_dir(impl, sp) + "*"))
            verbose_print("Getting data for %s with %s from %s" % (str(prog), impl, timing_dirs))
            for time_dir in timing_dirs:
                if len(timing_dirs) > 1:
                    version = time_dir.split(os.path.sep)[-1]
                else:
                    version = default_version_name()
                timingfiles = os.listdir(time_dir)
                benchmarks[prog][impl][version] = {}
                for timingfile in timingfiles:
                    fp = os.path.join(time_dir, timingfile)
                    bench_out = read_benchmark_output(fp)
                    benchmarks[prog][impl][version][timingfile] = bench_out
    return benchmarks

# data is of the form
#  data["kleenex"][version] = {inputfilename: [1,2,3,4]}
#  data["gawk"] = {"DEFAULT" : {inputfilename: [1,2,3,4]}}
def plot_all(benchmarks, inputNames, plotConfMap, skipFun, getTransformation):
    for prog, benchs in benchmarks.iteritems():
        try: output_names = plotConfMap[prog].keys()
        except KeyError: output_names = [prog + ".pdf"]
        for output_name in output_names:
            inputfile = ""
            try: inputfile = plotConfMap[prog][output_name]['indata']
            except KeyError: pass
            try:
                if inputfile == "" or inputfile == "DEFAULT":
                    inputfile = inputNames[prog][0]
            except KeyError:
                print "Skipping %s; no input data specified!" % prog
                continue
            inputname = os.path.basename(inputfile)
            inputsize = get_input_file_size(inputfile)
            verbose_print("Size: %s" % str(inputsize))
            def sf(i, n): # Specialise the skip function to this program.
                try: return skipFun(prog, i, n)
                except KeyError: return False
            transFun = getTransformation(prog, output_name)
            fig = plot_benchmark(prog, benchs, inputname, output_name, sf, transFun(inputsize))

def strip_input_file_suffix(s):
    input_suffix = ".runningtime"
    return s[:-len(input_suffix)]

def get_benchmark_configuration(conf_file = "benchmarks.txt",
                                inputs_file = "inputs.txt",
                                plots_file = "plots.txt"):
    def read_conf(fp, sep):
        m = {}
        with open(fp, 'r') as f:
            for line in f:
                if line[0] == '#' or line.strip() == "": continue
                ps = filter(lambda x : len(x) > 0, line.split(" "))
                prog = ps[0].strip()
                vector = map(lambda x : x.strip(), ps[1].split(sep))
                m[prog] = vector
        return m
    conf = read_conf(conf_file, ',')
    inputs = read_conf(inputs_file, ';')
    plots = {}
    with open(plots_file, 'r') as f:
        for line in f:
            if line[0] == '#' or line.strip() == "": continue
            ps = filter(lambda x : len(x) > 0, line.split(" "))
            prog = ps[0].strip()
            skip_list = map(lambda x : x.strip(), ps[1].split(','))
            inputdata_file_name = ps[2].strip()
            plot_transform = ps[3].strip()
            output_file_name = ps[4].strip()
            if not plots.has_key(prog):
                plots[prog] = {}
            plots[prog][output_file_name] = {'skip': skip_list,
                                             'indata' : inputdata_file_name,
                                             'transformation' : plot_transform}
    return (conf, inputs, plots)

def get_input_file_size(inpf):
    f = os.path.join(test_data_dir(), inpf)
    try:
        return os.lstat(f).st_size
    except OSError: # File not found...
        warning_print("Could not get size of \"%s\", file not found!" % f)
        return 0

def read_benchmark_output(fn):
    times = []
    magic_word = "matching (ms):"   # ouch!
    other_magic_word = "time (ms):" #
    with open(fn, 'r') as f:
        verbose_print("Looking in file %s" % fn)
        for line in f:
            if line.startswith(magic_word):
                l = int(line[len(magic_word):].strip())
                times.append(l)
            elif line.startswith(other_magic_word):
                l = int(line[len(other_magic_word):].strip())
                times.append(l)
    return times

# TODO:  Make histogram plots of data sets to see distribution.

# data is of the form
#  data["kleenex"][version] = {inputfilename: [1,2,3,4]}
#  data["gawk"] = {"DEFAULT" : {inputfilename: [1,2,3,4]}}
# the skipThis returns True on an impl name if it should be skipped!
def plot_benchmark(prog, data, inputname, output_name, skipThis, data_trans):
    trans_fun = data_trans["trans_fun"]
    median_format_string = data_trans["median_format_string"]
    yaxis_label = data_trans["yaxis_label"]
    title = data_trans["title"](prog, inputname)
    fig, ax = plt.subplots()
    lbls = []
    plot_data = []
    # Add the bars
    for impl, versions in iter(sorted(data.iteritems())):
        color_idx = 0
        if skipThis(impl, output_name):
            continue
        for version, inputfiles in iter(sorted(versions.iteritems())):
            for inputfile, times in iter(sorted(inputfiles.iteritems())):
                if strip_input_file_suffix(inputfile) != inputname:
                    verbose_print("Skipping %s because the file name is wrong: %s" % (inputname, inputfile))
                    continue
                if times == []:
                    verbose_print("Skipping %s because there are no time data." % inputfile)
                    continue
                if version == default_version_name(): v = impl
                else: v = version
                verbose_print("Adding data from '%s', file \"%s\", to plot." % (v, inputfile))
                try:
                    d = trans_fun(times)
                except ZeroDivisionError:
                    warning_print("Zero division for the data from %s in '%s'!" % (v, prog))
                    continue
                plot_data.append(d)
                if version == default_version_name():
                    v = None # I.e., there is only one version of the implementation
                else:
                    v = version
                lbls.append((impl, v))
    numBoxes = len(plot_data)
    if numBoxes == 0:
        plt.close()
        notice_print("Not writing %s - nothing on the plot." % output_name)
        return False
    # Make the actual boxplot
    bp = ax.boxplot(x=plot_data, sym='+', vert=1)
    plt.setp(bp['boxes'], color='black')
    plt.setp(bp['whiskers'], color='darkgrey')
    plt.setp(bp['fliers'], color='black', marker='+')
    plt.setp(bp['caps'], color='darkgrey')
    boxColors = ['steelblue', 'darkseagreen']
    for i in xrange(numBoxes):
        boxCoords = get_box_coords(bp["boxes"][i])
        boxWidth = boxCoords[1][0] - boxCoords[0][0]
        # Alternate between the colors
        k = i % (len(boxColors))
        ax.add_patch(plt.Polygon(boxCoords, facecolor=boxColors[k]))
        # Now draw the median lines back over what we just filled in
        med = bp['medians'][i]
        medianX = []
        medianY = []
        median = None
        for j in xrange(2):
            medianX.append(med.get_xdata()[j])
            medianY.append(med.get_ydata()[j])
            plt.plot(medianX, medianY, 'k')
            median = medianY[0]
        # Compute the point on the middle of the upper horizontal line of the box.
        # Translate this value from the data coordinate system into the "pixel" coordinate system.
        boxTop = ax.transData.transform((boxCoords[0][0] + boxWidth / 2.0, boxCoords[2][1]))
        # That way we can express that a point is 7 pixels above the box.
        (txtX, txtY) = ax.transData.inverted().transform((boxTop[0], boxTop[1] + 7))
        # And write the median value there.
        ax.text(txtX, txtY, median_format_string % median, horizontalalignment='center',
                size='x-small', weight="bold", color=boxColors[k])

    # Set properties of plot and make it look nice.
    ax.xaxis.grid(True, linestyle='-', which='major', color='lightgrey', alpha=0.5)
    ax.yaxis.grid(True, linestyle='-', which='major', color='lightgrey', alpha=0.5)
    ax.set_axisbelow(True)
    ax.set_xlim(0, numBoxes + 0.5)
    ax.spines["right"].set_visible(False)
    ax.spines["top"].set_visible(False)
    ax.spines["left"].set_color('lightgrey')
    ax.spines["bottom"].set_color('lightgrey')
    ax.yaxis.set_ticks_position('left')
    ax.set_ylabel(yaxis_label)
    ax.set_title(title)
    ax.set_xticks(np.arange(numBoxes) + 1)
    ax.tick_params(axis = 'x', length = 0)
    ax.tick_params(axis = 'y', colors = "grey")
    locale.setlocale(locale.LC_ALL, 'en_US')
    def locale_formatter(x, p):
        return locale.format("%d", x, grouping=True)
    ax.get_yaxis().set_major_formatter(ticker.FuncFormatter(locale_formatter))
    ax.set_ylim(bottom=0)
    ax.set_xticklabels(map(lambda (x,y):format_label(x,y), lbls), rotation = 45, horizontalalignment="right")
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        plt.tight_layout() # Throws an annoying warnings about renderers.
    save_plot(fig, lbls, plot_data, plot_dir, output_name)
    plt.close()
    return True


def save_plot(fig, labels, plot_data, directory, name):
    make_sure_path_exists(directory)
    fn = os.path.join(directory, name)
    csv_fn = (lambda (n, _) : n + ".csv")(os.path.splitext(fn))
    if not force_override and os.path.exists(fn):
        warning_print("File %s already exists, skipping.  Use -f to override." % fn)
    else:
        fig.savefig(fn)
        col_count = len(plot_data)
        row_count = max(map(lambda l : len(l), plot_data))
        with open(csv_fn, 'w') as csvfile:
            csvwriter = csv.writer(csvfile)
            row = []
            for i in xrange(0, col_count):
                nm,vs=labels[i]
                row.append(format_label(nm, vs))
            csvwriter.writerow(row)
            for i in xrange(0, row_count):
                row = []
                for j in xrange(0, col_count):
                    try: row.append(plot_data[j][i])
                    except IndexError:
                        row.append(None)
                csvwriter.writerow(row)
        print colored("Case '%s' -- wrote files:\n  %s\n  %s" % (name, fn, csv_fn), 'green')

        
def make_sure_path_exists(path):
    try:
        os.makedirs(path)
    except OSError as exception:
        if exception.errno != errno.EEXIST:
            raise

def get_box_coords(box):
    boxX = []
    boxY = []
    for j in xrange(5):
        boxX.append(box.get_xdata()[j])
        boxY.append(box.get_ydata()[j])
    return zip(boxX,boxY)
        
def format_label(name, version):
    if version != None:
        v = format_version(version)
        n = " "
    else:
        v = ""
        n = ""
    return "%s%s%s" % (name, n, v)

def format_version(vstring):
    if vstring == None:
        return ""
    ret_string = ""
    # This is really quite ugly.
    try:
        m = re.match(".*__(.+)__(.*)", vstring)
        opt_level = m.group(1)
        compiler = m.group(2)
        ret_string = "%s, %s" % (opt_level, compiler)
    except AttributeError:
        try:
            m = re.match(".*-(.*).awk", vstring)
            vname = m.group(1)
            ret_string = "%s" % (vname)
        except AttributeError:
            try:
                m = re.match(".*-(.*)", vstring)
                vname = m.group(1)
                ret_string = "%s" % (vname)
            except AttributeError:
                ret_string = "" 
    return ret_string

# Start main program; do everything!
if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="""
Make some plots!  
If no arguments are given, all programs are plotted.
        """)
    parser.add_argument('-p', nargs='+',
                        help='Name of the program to plot')
    parser.add_argument('-t',
                        help = "Data transform (mbs=Mbit/s [DEFAULT], gbs=Gbit/s, s=Seconds, ms=Milliseconds)")
    parser.add_argument('-c', 
                        help = "Alternate config file")
    parser.add_argument('-s', nargs='+', help = "Skip implementation")
    parser.add_argument('-b', nargs=1,
                        help = "Alternate base bench/ directory than current dir.")
    parser.add_argument('-v', action='count', help = "Be more verbose.")
    parser.add_argument('-d', nargs=1,
                        help = "Destination directory for plots.  Default plots/")
    parser.add_argument('-f', action='count', help = "Force overwrite of existing plots.")
    args = parser.parse_args()

    if args.p == None: progs = []
    else:              progs = args.p

    if args.t == None:    transform = "Mbit/s"
    elif args.t == "mbs": transform = "Mbit/s"
    elif args.t == "gbs": transform = "Gbit/s"
    elif args.t == "ms":  transform = "ms"
    elif args.t == "s":   transform = "s"
    else:
        warning_print("Unknown transformation: '%s'!." % args.t)
        exit(2)

    if args.b != None:
        a = args.b[0]
        if not a.endswith(os.path.sep): a = a + os.path.sep
        new = os.path.dirname(a)
        if new == "":
            warning_print("Could not use %s as a base directory." % a)
            exit(2)
        else:
            old = base_dir
            base_dir = "%s/" % new
            notice_print("Using %s as base directory instead of %s." % (base_dir, old))

    if args.v != None:
        is_verbose = True
        print "Entering verbose mode."

    if args.d != None:
        a = args.d[0]
        if not a.endswith(os.path.sep): a = a + os.path.sep
        new = os.path.dirname(a)
        if new == "":
            warning_print("Could not use %s as a plots directory." % a)
            exit(2)
        else:
            old = plot_dir
            plot_dir = "%s/" % new
            notice_print("Using %s as a plot directory instead of %s." % (plot_dir, old))

    if args.f != None:
        force_override = True

    go(progs, skip = args.s, default_transformation = transform)
