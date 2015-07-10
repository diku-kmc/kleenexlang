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
import json as json

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

def shorten(label):
    return label.replace("kleenex", "kex").replace("-nola", "").replace(", gcc", " gcc").replace(", clang", " clang")

def trans_Gbit_per_s(inputsize_bytes):
    return { "median_format_string" : "%.3f",
             "trans_fun" : lambda ps : map(lambda p : ((inputsize_bytes * 8) / 1e9) / (p / 1000.0), ps),
             "yaxis_label" : "Gbit/s",
             "title" : lambda prog, inputname : "%s (%s %.2f MB)" %
             (prog, inputname, inputsize_bytes / 2.0**20),
             "inputsize_bytes" : inputsize_bytes
         }
def trans_Mbit_per_s(inputsize_bytes):
    return { "median_format_string" : "%.1f",
             "trans_fun" : lambda ps : map(lambda p : ((inputsize_bytes * 8) / 1e6) / (p / 1000.0), ps),
             "yaxis_label" : "Mbit/s",
             "title" : lambda prog, inputname : "%s (%s %.2f MB)" %
             (prog, inputname, inputsize_bytes / 2.0**20),
             "inputsize_bytes" : inputsize_bytes
         }
def trans_s(inputsize_bytes):
    return { "median_format_string" : "%d",
             "trans_fun" : lambda ps : map(lambda p : p / 1000, ps),
             "yaxis_label" : "Time (s)",
             "title" : lambda prog, inputname : "%s (%s %.2f MB)" %
             (prog, inputname, inputsize_bytes / 2.0**20),
             "inputsize_bytes" : inputsize_bytes
         }
def trans_ms(inputsize_bytes):
    return { "median_format_string" : "%d",
             "trans_fun" : lambda ps : ps,
             "yaxis_label" : "Time (ms)",
             "title" : lambda prog, inputname : "%s (%s %.2f MB)" %
             (prog, inputname, inputsize_bytes / 2.0**20),
             "inputsize_bytes" : inputsize_bytes
         }

transformations = {
    "Gbit/s" : trans_Gbit_per_s,
    "Mbit/s" : trans_Mbit_per_s,
    "s"      : trans_s,
    "ms"     : trans_ms
}

# def get_line_color_and_style(impl, version):
#     label = format_label(impl, version)
#     if label == "kex 0 clang":      return ("y", None)
#     elif label == "kex 0 gcc":      return ("darkolivegreen", None)
#     elif label == "kex 3 clang":    return ("sage", None)
#     elif label == "kex 3 gcc":      return ("limegreen", None)
#     elif label == "kex 0-la clang": return ("turquoise", None)
#     elif label == "kex 0-la gcc":   return ("aqua", None)
#     elif label == "kex 3-la clang": return ("olive", None)
#     elif label == "kex 3-la gcc":   return ("yellowgreen", None)
#     elif label == "gawk":           return ("gold", None)
#     elif label == "oniguruma":      return ("darkkhaki", None)
#     elif label == "perl":           return ("darkblue", None)
#     elif label == "python":         return ("dodgerblue", None)
#     elif label == "re2":            return ("magenta", None)
#     elif label == "re2j":           return ("purple", None)
#     elif label == "sed":            return ("indigo", None)
#     elif label == "tcl":            return ("lightcoral", None)
#     elif label == "ragel F1":       return ("firebrick", None)
#     elif label == "ragel G2":       return ("sienna", None)
#     elif label == "ragel T1":       return ("sandybrown", None)
#     else:
#         raise Error("I don't know the color/style of %s" % label)


# Default base_dir is the directory of this script.
base_dir = os.path.realpath("__file__")
# Default plot_dir is plots/
plot_dir = os.path.join(os.path.dirname(os.path.realpath("__file__")), "plots")
# Default folder used for data is the latest generated.
# Is populated once the real base_dir is known.
data_folder = None
# Don't override plots if they already exist.
force_override = False
# Don't print so much gunk.
is_verbose = False
# Alternate directory for benchmark input data
# Default is None, so ../test/data/ is used.
# Useful when plotting stuff from another machine (razorblade)
alternate_input_data_dir = None

def verbose_print(msg):
    if is_verbose:
        print colored(msg, 'yellow')

def warning_print(msg):
    print colored(msg, 'red')

def notice_print(msg):
    print colored(msg, 'cyan')

def default_version_name():
    return "DEFAULT"

def data_base_dir():
    return os.path.join(os.path.dirname(base_dir), "times")

def data_dir(impl, prog):
    return os.path.join(data_base_dir(), data_folder, impl, prog)

def test_data_dir():
    if alternate_input_data_dir != None:
        return alternate_input_data_dir
    else:
        return os.path.join(os.path.dirname(base_dir), "..", "test", "data")

def plot_save_dir():
    return os.path.join(plot_dir, data_folder)

def go(progs = [], skip = None, default_transformation = "ms", plot_kind = 'barchart',
       inputs_file = "inputs.txt", plots_file = "plots.json", benchmarks_file = "benchmarks.txt"):
    conf, inputs, plotconf = get_benchmark_configuration(benchmarks_file,
                                                         inputs_file,
                                                         plots_file
    )
    if skip != None: # Override whatever is read from plots.txt
        if type(skip) == list:
            skipFun = lambda p, i, n : i in skip
        elif type(skip) == dict:
            skipFun = lambda p, i, n : i in skip[p]
        else:
            raise Exception("If set, skip must be a list or a dictionary!")
    else: # Then we do not override the skip map from plots.txt
        def f(p, i, n): # program name, implementation name, output name
            try: return i in plotconf[p][n]['skip']
            except KeyError: return False
        skipFun = f
    
    def getTransFun(p, n):
        try: return transformations[plotconf[p][n]['transformation']]
        except KeyError:
            return transformations[default_transformation]

    try:
        os.makedirs(plot_save_dir())
    except OSError:
        notice_print("Plot output dir already exists")
        pass

    plot_all(get_data(conf, progs), inputs, plotconf, skipFun, getTransFun, plot_kind)

# returns: {"as" : { "kleenex" : { <version> : { "as_100mb.txt" : [110, 123, 35, ...],
#                                                "as_245mb.txt" : [13, 8, 3, ...],
#                                                 ...} } ,
#                    "perl" : ... },
#           ... }
def get_data(conf, only_progs = []):
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
            # Get timing info from database_dir (times/)
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
def plot_all(benchmarks, inputNames, plotconf, skipFun, getTransformation, plot_kind):
    for prog, benchs in benchmarks.iteritems():
        try: output_names = plotconf[prog].keys()
        except KeyError: output_names = [prog + ".pdf"]
        for output_name in output_names:
            inputglob = ""
            try: inputglob = plotconf[prog][output_name]['indata']
            except KeyError: pass
            try:
                if inputglob == "" or inputglob == "DEFAULT":
                    inputglob = inputNames[prog][0]
            except KeyError:
                print "Skipping %s; no input data specified!" % prog
                continue
            transFun = getTransformation(prog, output_name)
            
            inputfiles = glob.glob(os.path.join(test_data_dir(), inputglob))
            inputnames = map(os.path.basename, inputfiles)

            inputsizes_map = map(lambda f: (f, get_input_file_size(f)), inputfiles)
            inputsizes = map(lambda (f,s): s, inputsizes_map)
            inputfiles_by_size = sorted(inputsizes_map, key=lambda (f,s): s)
            # A list of triples (filename, size, transformationfunc)
            inputnames_by_size = map(lambda (f,s): (os.path.basename(f), s, transFun(s)),
                                     inputfiles_by_size)
            verbose_print("Sizes: %s" % str(sorted(inputsizes)))
            verbose_print("Files: %s" % str(inputfiles_by_size))

            def sf(i, n, version): # Specialise the skip function to this program.
                rendered = format_label(prog, version)
                if plotconf[prog][n]['skip_impl'] != None:
                    return rendered in plotconf[prog][n]['skip_impl']
                try: return skipFun(prog, i, n)
                except KeyError: return False

            try: plot_title = plotconf[prog][output_name]["plot_title"]
            except KeyError: plot_title = None # Defaults in plot_*
            
            if len(inputfiles) > 1:
                plot_collated_benchmark(prog, benchs, inputnames_by_size,
                                        output_name, sf, plot_title = plot_title)
            else:
                try:
                    plot_benchmark(prog, benchs,
                                   inputnames[0], output_name,
                                   sf, transFun(inputsizes[0]),
                                   plot_kind, plot_title = plot_title)
                except IndexError:
                    warning_print("Skipping %s because inputnames is empty." % prog)

def strip_input_file_suffix(s):
    input_suffix = ".runningtime"
    return s[:-len(input_suffix)]
def add_input_file_suffix(s):
    input_suffix = ".runningtime"
    return s + input_suffix
    

def get_benchmark_configuration(conf_file, inputs_file, plots_file):
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
    # Read in plot setup file
    plot_conf = None
    with open(plots_file, 'r') as f: plot_conf = json.load(f)
    for p in plot_conf:
        plots[p["program"]] = {}
        for concreteplot in p["plots"]:
            try:             skip = concreteplot["skip"]
            except KeyError: skip = []
            try:             inputfilename = concreteplot["input"]
            except KeyError: inputfilename = "DEFAULT"
            try:             plot_transform = concreteplot["plot_transform"]
            except KeyError: plot_transform = "Mbit/s"
            try:             collated = concreteplot["collated"]
            except KeyError: collated = False
            try:             plot_title = concreteplot["title"]
            except KeyError: plot_title = None # Defaults to input file and size
            try:             skip_impl = concreteplot[""]
            except KeyError: skip_impl = None
            plots[p["program"]][concreteplot["filename"]] = { 'skip' : skip,
                                                              'indata' : inputfilename,
                                                              'transformation' : plot_transform,
                                                              'plot_title' : plot_title,
                                                              'skip_impl' : skip_impl }
    return (conf, inputs, plots)

def get_input_file_size(inpf):
    f = os.path.join(test_data_dir(), inpf)
    try:
        return os.lstat(f).st_size
    except OSError: # File not found...
        warning_print("Could not get size of \"%s\", file not found!" % f)
        return 0

# Make scatter plots of each implementation and connect with lines.
def plot_collated_benchmark(prog, data, inputnames, output_name, skipThis, plot_title = None):
    yaxis_label = inputnames[0][2]["yaxis_label"]
    if plot_title != None:
        title = plot_title
    else:
        title = "%s" % prog

    labels = []
    plot_data = {}
    for impl, versions in iter(sorted(data.iteritems())):
        plot_data[impl] = {}
        for version, inputfiles in iter(sorted(versions.iteritems())):
            if skipThis(impl, output_name, version):
                continue
            plot_data[impl][version] = {}
            avg_times = []
            stddev_times = []
            ls = []
            for (inputfile, filesize, tf) in inputnames:
                times = []
                trans_fun = tf["trans_fun"]
                try: times = inputfiles[add_input_file_suffix(inputfile)]
                except KeyError:
                    verbose_print("Skipping %s because it is not included in the series." % inputfile)
                    continue
                inputname = inputfile
                if times == []:
                    verbose_print("Skipping %s because there are no time data." % inputfile)
                    continue
                avg_times.append(np.average(trans_fun(times)))
                stddev_times.append(np.std(trans_fun(times)))
                if labels == []: # Should be the same every time, so only set it once...
                    ls.append(filesize)
            if labels == []:
                labels = ls
            if version == default_version_name():
                v = None
            else:
                v = version
            plot_data[impl][version] = { 'avgs' : avg_times,
                                         'stddevs' : stddev_times }

    numElms = len(labels)
    if numElms == 0:
        warning_print("No labels? Skipping %s (collated)..." % prog)
        return False
    
    markers = ['.', ',', 'o', 'v', '^', '<', '>', '1', '2', '3', '4',
               '8', 's', 'p', '*', 'h', 'H', '+', 'x', 'D', 'd', '|', '_']
    markIdx = 0
    # Now plot
    fig, ax = plt.subplots()
    for impl, versions in iter(sorted(plot_data.iteritems())):
        for version, timedata in iter(sorted(versions.iteritems())):
            x = labels
            mark = markers[markIdx]
            line = ax.plot(x, timedata['avgs'], label = format_label(impl, version), marker = mark)
            ax.errorbar(x, timedata['avgs'], yerr=timedata['stddevs'], color=line[0].get_color(), fmt=mark)
            markIdx = (markIdx + 1) % len(markers)

    # Add legend so we have a chance of reading the plot.
    ax.legend(loc='best', fancybox = True, framealpha = 0.4)
    
    # Setup plot niceness
    ax.yaxis.grid(True,
                  linestyle='-',
                  which='major',
                  color='grey',
                  alpha=0.7)
    ax.spines["right"].set_visible(False)
    ax.spines["top"].set_visible(False)
    ax.spines["left"].set_color('lightgrey')
    ax.spines["bottom"].set_color('lightgrey')
    ax.set_xlabel("Input file size")
    ax.set_ylabel(yaxis_label)
    ax.set_title(title)
#    ax.set_xticks(labels)
    ax.tick_params(axis = 'x', length = 0)
    ax.tick_params(axis = 'y', colors = "black")
    ax.set_ylim(bottom=0)
    ax.set_xticklabels(np.arange(labels[-1]))
    ax.get_xaxis().set_major_formatter(ticker.FuncFormatter(lambda x, p: sizeof_fmt(x)))
    locale.setlocale(locale.LC_ALL, 'en_US')
    def locale_formatter(x, p):
        return locale.format("%d", x, grouping=True)
    ax.get_yaxis().set_major_formatter(ticker.FuncFormatter(locale_formatter))
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        plt.tight_layout() # Throws an annoying warning about renderers.
    save_plot(fig, labels, plot_data, plot_save_dir(), output_name, write_csv = False)
    return True
            
def sizeof_fmt(num, suffix='B'):
    for unit in ['','Ki','Mi','Gi','Ti','Pi','Ei','Zi']:
        if abs(num) < 1024.0:
            return "%3.1f%s%s" % (num, unit, suffix)
        num /= 1024.0
    return "%.1f%s%s" % (num, 'Yi', suffix)

# data is of the form
#  data["kleenex"][version] = {inputfilename: [1,2,3,4]}
#  data["gawk"] = {"DEFAULT" : {inputfilename: [1,2,3,4]}}
# the skipThis returns True on an impl name if it should be skipped!
def plot_benchmark(prog, data, inputname, output_name, skipThis, data_trans, plot_kind, plot_title = None):
    trans_fun = data_trans["trans_fun"]
    median_format_string = data_trans["median_format_string"]
    yaxis_label = data_trans["yaxis_label"]
    if plot_title != None:
        title = "%s (%.2f MB)" % (plot_title, data_trans["inputsize_bytes"] / 2.0**20)
    else:
        title = data_trans["title"](prog, inputname)

    lbls = []
    plot_data = []
    # Add the bars
    for impl, versions in iter(sorted(data.iteritems())):
        for version, inputfiles in iter(sorted(versions.iteritems())):
            if skipThis(impl, output_name, version):
                continue
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
    
    fig, ax = plt.subplots()
    if plot_kind == "boxplot":
        make_boxplot(ax, lbls, plot_data, median_format_string)
    elif plot_kind == "barchart":
        make_barplot(ax, lbls, plot_data, median_format_string)
    else:
        raise Error("Illegal plot kind: %s" % plot_kind)

    # Set properties of plot and make it look nice.
    if plot_kind == "boxplot":
        ax.xaxis.grid(True,
                      linestyle='-',
                      which='major',
                      color='lightgrey',
                      alpha=0.5)
    
    ax.yaxis.grid(True,
                  linestyle='-',
                  which='major',
                  color='grey',
                  alpha=0.7)
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
    ax.tick_params(axis = 'y', colors = "black")
    locale.setlocale(locale.LC_ALL, 'en_US')
    def locale_formatter(x, p):
        return locale.format("%d", x, grouping=True)
    ax.get_yaxis().set_major_formatter(ticker.FuncFormatter(locale_formatter))
    ax.set_ylim(bottom=0)
    ax.set_xticklabels(map(lambda (x,y): format_label(x,y), lbls),
                       rotation = 45,
                       horizontalalignment="right",
                       size="medium"
    )
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        plt.tight_layout() # Throws an annoying warning about renderers.
    save_plot(fig, lbls, plot_data, plot_save_dir(), output_name)
    plt.close()
    return True

def make_barplot(ax, labels, plot_data, median_format_string):
    means = map(lambda ys: np.average(ys), plot_data)
    stddevs = map(lambda ys: np.std(ys), plot_data)
    bar_width = 0.85
    n = len(labels)
    def pick((x,y)):
        if x.find("kleenex") >= 0:
            return "steelblue"
        else:
            return "darkseagreen"
    colors = map(pick, labels)
    bars = ax.bar(
        left = map(lambda l:l - bar_width/2, np.arange(1, n+1)),
        height = means,
        width = bar_width,
        yerr = stddevs,
        color = colors,
        ecolor = 'darkslategrey',
        edgecolor = 'lightgrey',
        linewidth = 0,
        capsize=5,
        error_kw={'elinewidth': 2,
                  'capthick' : 1.2 }
    )
    

def make_boxplot(ax, labels, plot_data, median_format_string):
    numBoxes = len(labels)
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
                size='medium', weight="bold", color=boxColors[k])

def save_plot(fig, labels, plot_data, directory, name, write_csv = True):
    make_sure_path_exists(directory)
    fn = os.path.join(directory, name)
    csv_fn = (lambda (n, _) : n + ".csv")(os.path.splitext(fn))
    if not force_override and os.path.exists(fn):
        warning_print("File %s already exists, skipping.  Use -f to override." % fn)
    else:
        fig.savefig(fn)
        if write_csv:
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
                        try:               row.append(plot_data[j][i])
                        except IndexError: row.append(None)
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
    return shorten("%s%s%s" % (name, n, v))

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
    parser.add_argument('-s', nargs='+', help = "Skip implementation")
    parser.add_argument('-b', nargs=1,
                        help = "Alternate base bench/ directory than current dir.")
    parser.add_argument('-l', nargs=1,
                        help = "Label/timestamp of the run to use data from")
    parser.add_argument('-v', action='count', help = "Be more verbose.")
    parser.add_argument('-d', nargs=1,
                        help = "Destination directory for plots.  Default plots/<label>")
    parser.add_argument('-f', action='count', help = "Force overwrite of existing plots.")
    parser.add_argument('-k',
                        help = "Kind of plot to make (barchart, boxplot). Default: barchart.")
    parser.add_argument('-g',
                        help = "Alternate benchmark configuration file (default: benchmarks.txt)")
    parser.add_argument('-c',
                        help = "Alternate plot configuration JSON file (default: plots.json)")
    parser.add_argument('-e',
                        help = "Alternate input configuration file (default: inputs.txt)")
    parser.add_argument('-i',
                        help = "Alternate directory for test input data (default: ../test/data/)")
    args = parser.parse_args()

    # Set all the nice, global variables first...
    if args.v != None:
        is_verbose = True
        print "Entering verbose mode."

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

    if args.l != None:
        data_folder = args.l[0]
    else:
        dirs = sorted(filter(lambda s: s[0] != '.', os.listdir(data_base_dir())), reverse=True,
                      key=lambda d: os.path.getctime(os.path.join(data_base_dir(), d)))
        if len(dirs) < 1:
            warning_print("No folders found in timing directory.")
            exit(2)
        data_folder = dirs[0]
        notice_print("No data directory specified; using %s." % data_folder)

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

    # Use a different input data dir.
    if args.i != None:
        alternate_input_data_dir = args.i

    # Now build the call to go()
    call_dict = {}
    if args.p != None: call_dict['progs'] = args.p

    if args.t == "mbs":   call_dict['default_transformation'] = "Mbit/s"
    elif args.t == "gbs": call_dict['default_transformation'] = "Gbit/s"
    elif args.t == "ms":  call_dict['default_transformation'] = "ms"
    elif args.t == "s":   call_dict['default_transformation'] = "s"
    else:
        if args.t != None:
            warning_print("Unknown transformation: '%s'!." % args.t)
            exit(2)


    if args.k == "barchart":  call_dict['plot_kind'] = 'barchart'
    elif args.k == "boxplot": call_dict['plot_kind'] = 'boxplot'
    else:
        if args.k != None:
            warning_print("Unknown plot kind: %s", args.k)
            exit(2)

    if args.g != None: call_dict["benchmarks_file"] = args.g
    if args.c != None: call_dict["plots_file"] = args.c
    if args.e != None: call_dict["inputs_file"] = args.e

    if args.s != None: call_dict["skip"] = args.s

    go(**call_dict)
