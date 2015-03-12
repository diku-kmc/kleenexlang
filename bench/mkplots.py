#!/usr/bin/env python
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np
import glob
import re
import locale
import os

def trans_Gbit_per_s(inputsize_bytes):
    return { "median_format_string" : "%.3f",
             "trans_fun" : lambda ps : map(lambda p : ((inputsize_bytes * 8) / 1e9) / (p / 1000.0), ps),
             "yaxis_label" : "GBit/s",
             "title" : lambda prog, inputname : "%s (%s %.2f MB)" %
             (prog, inputname, inputsize_bytes / 2.0**20)
         }
def trans_Mbit_per_s(inputsize_bytes):
    return { "median_format_string" : "%.2f",
             "trans_fun" : lambda ps : map(lambda p : ((inputsize_bytes * 8) / 1e6) / (p / 1000.0), ps),
             "yaxis_label" : "MBit/s",
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
    "ms" : trans_ms
}

def default_version_name():
    return "DEFAULT"

def get_plot_full_name(n):
    return os.path.join(os.path.dirname(os.path.realpath("__file__")), "plots", n)

def g(): # For testing purposes.
    go("simple_id", ["cpp11"])

def go(prog = None, skip = None, transform = "ms"):
    skipFun = lambda p, i : False
    try:
        transFun = transformations[transform]
    except KeyError:
        raise Exception("Could not find transformation \"%s\"" % transform)

    if skip != None:
        if type(skip) == list:
            skipFun = lambda p, i : i in skip
        elif type(skip) == dict:
            skipFun = lambda p, i : i in skip[p]
        else:
            raise Exception("If set, skip must be a list or a dictionary!")
    conf, inputs = get_benchmark_configuration()
    plot_all(get_data(conf, prog), inputs, skipFun, transFun)

def get_data(conf, only_prog = None):
    benchmarks = {}
    for prog, impls in conf.iteritems():
        if only_prog != None and only_prog != prog:
            continue
        benchmarks[prog] = {}
        for impl in impls:
            benchmarks[prog][impl] = {}
            # Get timing info from impl/time/prog/*
            timing_dirs = filter(os.path.isdir, glob.glob(data_dir(impl, prog) + "*"))
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
def plot_all(benchmarks, inputNames, skipFun, transFun):
    first = lambda m : m[m.keys()[0]]
    for prog, benchs in benchmarks.iteritems():
        filename = get_plot_full_name(prog + ".pdf")
        try:
            # TODO We just pick the first input file...
            # inputname = (first(first(benchs)).keys()[0])[:-len(input_suffix)]
            inputfile = inputNames[prog][0]
            inputname = os.path.basename(inputfile)
            inputsize = get_input_file_size(inputfile)
            def sf(i): # Specialise the skip function to this program.
                try: return skipFun(prog, i)
                except KeyError: return False
            fig = plot_benchmark(prog, benchs, inputname, filename, sf, transFun(inputsize))
        except KeyError: # No file name
            pass

def strip_input_file_suffix(s):
    input_suffix = ".runningtime"
    return s[:-len(input_suffix)]

def get_benchmark_configuration(conf_file = "benchmarks.txt", inputs_file = "inputs.txt"):
    def read_conf(fp, sep):
        m = {}
        with open(fp, 'r') as f:
            for line in f:
                if line[0] == '#': continue
                ps = filter(lambda x : len(x) > 0, line.split(" "))
                prog = ps[0].strip()
                vector = map(lambda x : x.strip(), ps[1].split(sep))
                m[prog] = vector
        return m
                
    conf = read_conf(conf_file, ',')
    inputs = read_conf(inputs_file, ';')
    return (conf, inputs)

def get_input_file_size(inpf):
    try:
        return os.lstat(os.path.join(test_data_dir(), inpf)).st_size
    except OSError: # File not found...
        return 0

def data_dir(impl, prog):
    return os.path.join(os.path.dirname(
        os.path.realpath("__file__")),
        impl, "times", prog)

def test_data_dir():
    return os.path.join(os.path.dirname(os.path.realpath("__file__")), "..", "test", "data")

def read_benchmark_output(fn):
    times = []
    magic_word = "matching (ms):"   # ouch!
    other_magic_word = "time (ms):" #
    with open(fn, 'r') as f:
        for line in f:
            if line.startswith(magic_word):
                l = int(line[len(magic_word):].strip())
                times.append(l)
            elif line.startswith(other_magic_word):
                l = int(line[len(other_magic_word):].strip())
                times.append(l)
    return times

# data is of the form
#  data["kleenex"][version] = {inputfilename: [1,2,3,4]}
#  data["gawk"] = {"DEFAULT" : {inputfilename: [1,2,3,4]}}
# the skipThis returns True on an impl name if it should be skipped!
def plot_benchmark(prog, data, inputname, outfilename, skipThis, data_trans):
    trans_fun = data_trans["trans_fun"]
    median_format_string = data_trans["median_format_string"]
    yaxis_label = data_trans["yaxis_label"]
    title = data_trans["title"](prog, inputname)
    fig, ax = plt.subplots()
    lbls = []
    plot_data = []
    # Add the bars
    for impl, versions in data.iteritems():
        color_idx = 0
        if skipThis(impl):
            continue
        for version, inputfiles in versions.iteritems():
            for inputfile, times in inputfiles.iteritems():
                if strip_input_file_suffix(inputfile) != inputname:
                    continue
                if times == []:
                    continue
                plot_data.append(trans_fun(times))
                if version == default_version_name():
                    v = None # I.e., there is only one version of the implementation
                else:
                    v = version
                lbls.append((impl, v))
    numBoxes = len(plot_data)
    if numBoxes == 0:
        plt.close()
        return False
    returnCode = False
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
    ax.set_axisbelow(True)
    ax.set_xlim(0, numBoxes + 0.5)
    ax.spines["right"].set_visible(False)
    ax.spines["top"].set_visible(False)
    ax.spines["left"].set_color('grey')
    ax.spines["bottom"].set_color('grey')
    ax.yaxis.set_ticks_position('left')
    ax.set_ylabel(yaxis_label)
    ax.set_title(title)
    ax.set_xticks(np.arange(numBoxes) + 1)
    ax.tick_params(axis = 'x', length = 0)
    ax.tick_params(axis = 'y', colors = "grey")
    # TODO make y tick labels formatted
    locale.setlocale(locale.LC_ALL, 'en_US')
    def locale_formatter(x, p):
        return locale.format("%d", x, grouping=True)
    ax.get_yaxis().set_major_formatter(ticker.FuncFormatter(locale_formatter))
    ax.set_ylim(bottom=0)
    ax.set_xticklabels(map(lambda (x,y):format_label(x,y), lbls), rotation = 90)
    plt.tight_layout()
    fig.savefig(outfilename)
    print "Wrote file %s" % outfilename
    plt.close()
    return True

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
        n = "\n"
    else:
        v = ""
        n = ""
    return "%s%s%s" % (name, n, v)

def format_version(vstring):
    if vstring == None:
        return ""
    ret_string = ""
    
    try:
        m = re.match(".*__([0-9]+)__(.*)", vstring)
        opt_level = m.group(1)
        compiler = m.group(2)
        ret_string = "%s, %s" % (opt_level, compiler)
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
    skipMap = {
        "simple_id" : ["cpp11", "cat"] 
    }
    go(skip = skipMap, transform = "Mbit/s")
