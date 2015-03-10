#!/usr/bin/env python
from matplotlib.pylab import *
import matplotlib.pyplot as plt
import glob
# import os as os

# for each program:
#  for each input file:
#     collect all performance data for each implementation and combine
#     plot combined data

def default_version_name():
    return "DEFAULT"

def get_plot_full_name(n):
    return os.path.join(os.path.dirname(os.path.realpath("__file__")), "plots", n)

def go():
    plot_all(get_data())

def get_data():
    conf = get_benchmark_configuration()
    benchmarks = {}
    for prog, impls in conf.iteritems():
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
#  data["hased"][version] = {inputfilename: [1,2,3,4]}
#  data["gawk"] = {"DEFAULT" : {inputfilename: [1,2,3,4]}}
def plot_all(benchmarks):
    first = lambda m : m[m.keys()[0]]
    for prog, benchs in benchmarks.iteritems():
        filename = get_plot_full_name(prog + ".pdf")
        try:
            # TODO We just pick the first input file...
            inputname = first(first(benchs)).keys()[0]
            fig = plot_benchmark(prog, benchs, inputname, filename)
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
#  data["hased"][version] = {inputfilename: [1,2,3,4]}
#  data["gawk"] = {"DEFAULT" : {inputfilename: [1,2,3,4]}}
def plot_benchmark(title, data, inputname, outfilename):
    fig, ax = plt.subplots()
    colors = "rgby"
    w = 0.35
    pos = 0
    labels = []
    # Add the bars
    for impl, versions in data.iteritems():
        color_idx = 0
        for version, inputfiles in versions.iteritems():
            for inputfile, times in inputfiles.iteritems():
                if inputfile != inputname:
                    continue
                if times == []:
                    continue
                mean_time = mean(times)
                std_dev   = std(times)
                ax.bar(left=pos, height=mean_time, width=w,
                       color=colors[color_idx], yerr=std_dev)
                ax.text(pos + w / 2.0, 20 + mean_time, '%d' % int(mean_time),
                        ha='center', va='bottom')
                if version == default_version_name():
                    name = impl
                else:
                    name = version
                # TODO: Figure out way to place name labels in a good way.
                ax.text(pos + w / 2.0, 100 + mean_time, name, ha="center", va="bottom", rotation=90)
                labels.append(name)
                color_idx = (color_idx + 1) % len(colors)
                pos += 1
    ax.set_ylabel('Mean time (ms)')
    ax.set_title("%s (%s)" % (title, inputname))
    ax.set_xticks(arange(len(labels)))
    ax.tick_params(axis = 'x', length = 0)
    fig.savefig(outfilename)
    plt.close()
    print "Wrote file %s" % outfilename
    return True



# Start main program; do everything!
if __name__ == "__main__":
    go()    
