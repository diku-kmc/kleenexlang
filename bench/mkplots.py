#!/usr/bin/env python
#from matplotlib.pylab import *
import matplotlib.pyplot as plt
import glob
import re

# for each program:
#  for each input file:
#     collect all performance data for each implementation and combine
#     plot combined data

def default_version_name():
    return "DEFAULT"

def get_plot_full_name(n):
    return os.path.join(os.path.dirname(os.path.realpath("__file__")), "plots", n)

def g(): # For testing purposes.
    go("simple_id")

def go(prog = None, skip = None):
    skipFun = lambda p, i : False
    if skip != None:
        if type(skip) == list:
            skipFun = lambda p, i : i in skip
        elif type(skip) == dict:
            skipFun = lambda p, i : i in skip[p]
        else:
            raise Exception("If set, skip must be a list or a dictionary!")
    
    plot_all(get_data(prog), skipFun)

def get_data(only_prog = None):
    conf = get_benchmark_configuration()
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
def plot_all(benchmarks, skipFun):
    first = lambda m : m[m.keys()[0]]
    for prog, benchs in benchmarks.iteritems():
        filename = get_plot_full_name(prog + ".pdf")
        try:
            # TODO We just pick the first input file...
            inputname = first(first(benchs)).keys()[0]
            def sf(i): # Specialise the skip function to this program.
                try: return skipFun(prog, i)
                except KeyError: return False
            fig = plot_benchmark(prog, benchs, inputname, filename, sf)
        except IndexError: # No file name
            pass

def get_benchmark_configuration(conf_file = "benchmarks.txt"):
    conf = {}
    with open(conf_file, 'r') as f:
        for line in f:
            if line[0] == '#': continue
            ps = filter(lambda x : len(x) > 0, line.split(" "))
            prog = ps[0]
            impls = map(lambda x : x.strip(), ps[1].split(','))
            conf[prog] = impls
    return conf

def data_dir(impl, prog):
    return os.path.join(os.path.dirname(
        os.path.realpath("__file__")),
        impl, "times", prog)

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
def plot_benchmark(title, data, inputname, outfilename, skipThis):
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
                if inputfile != inputname:
                    continue
                if times == []:
                    continue
                plot_data.append(times)
                if version == default_version_name():
                    v = None # I.e., there is only one version of the implementation
                else:
                    v = version
                lbls.append((impl, v))
    returnCode = False
    # Make the actual boxplot
    bp = ax.boxplot(x=plot_data, sym='+', vert=1)
    plt.setp(bp['boxes'], color='black')
    plt.setp(bp['whiskers'], color='darkgrey')
    plt.setp(bp['fliers'], color='grey', marker='+')
    boxColors = ['steelblue', 'darkseagreen']
    numBoxes = len(plot_data)
    for i in xrange(numBoxes):
        boxCoords = get_box_coords(bp["boxes"][i])
        boxWidth = boxCoords[1][0] - boxCoords[0][0]
        # Alternate between the colors
        k = i % (len(boxColors))
        ax.add_patch(Polygon(boxCoords, facecolor=boxColors[k]))
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
        ax.text(txtX, txtY, "%d" % median, horizontalalignment='center',
                size='small', weight="bold", color=boxColors[k])

    # Set o
    ax.xaxis.grid(True, linestyle='-', which='major', color='lightgrey', alpha=0.5)
    ax.set_axisbelow(True)
    ax.set_xlim(0, numBoxes + 0.5)
    ax.spines["right"].set_visible(False)
    ax.spines["top"].set_visible(False)
    ax.spines["left"].set_color('grey')
    ax.spines["bottom"].set_color('grey')
    ax.yaxis.set_ticks_position('left')
    ax.set_ylabel('Time (ms)')
    ax.set_title("%s (%s)" % (title, inputname))
    ax.set_xticks(arange(numBoxes) + 1)
    ax.tick_params(axis = 'x', length = 0)
    ax.tick_params(axis = 'y', colors = "grey")
#    ax.ticklabel_format(axis = 'y', useLocale = True)
    ax.set_ylim(bottom=0)
    ax.set_xticklabels(map(lambda (x,y):format_label(x,y), lbls), rotation = 90)
    plt.tight_layout()
    
    if numBoxes > 0: # Only save it if we actually drew something.
        fig.savefig(outfilename)
        print "Wrote file %s" % outfilename
        returnCode = True
    
    plt.close()
    return returnCode

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
        m = re.match(".*-(.*)", vstring)
        vname = m.group(1)
        ret_string = "%s" % (vname)
    return ret_string

# Start main program; do everything!
if __name__ == "__main__":
    go()    
