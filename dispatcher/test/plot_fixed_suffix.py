import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter, FuncFormatter
from math import log
import os
import re

this_path = os.getcwd();

src_dir = os.getcwd()

src_path = os.path.join(this_path, src_dir)

state_count = []

bad_files = [".DS_Store", ".gitignore"]
bad_dirs = [".idea"]

record = re.compile('\d+')

plotdata = {}

size={'csv_project3': 1000000054,
      'DIV7-gen': 1073741824,
      'flip_ab': 1000000001,
      'irc': 1000000211,
      'regex2': 135557924,
      'thousand_sep': 1000000001}


def trans_Mbit_per_s(inputsize_bytes):
    return { "median_format_string" : "%.1f",
             "trans_fun" : lambda ps : map(lambda p : ((inputsize_bytes * 8) / 1e6) / (p / 1000.0), ps),
             "yaxis_label" : "Mbit/s",
             "title" : lambda prog : "%s (%.2f MB)" %
             (prog, inputsize_bytes / 2.0**20),
             "inputsize_bytes" : inputsize_bytes,
             "unit": 'Mbit'
         }


def trans_Gbit_per_s(inputsize_bytes):
    return { "median_format_string" : "%.3f",
             "trans_fun" : lambda ps : map(lambda p : ((inputsize_bytes * 8) / 1e9) / (p / 1000.0), ps),
             "yaxis_label" : "Gbit/s",
             "title" : lambda prog : "%s (%.2f MB)" %
             (prog, inputsize_bytes / 2.0**20),
             "inputsize_bytes" : inputsize_bytes,
             "unit": 'Gbit'
         }


def load_file(fn):
    name = fn.split("_bin")[0]
    chunks = re.search(r'(?<=c)\d{3}', fn).group(0)
    suffix = re.search(r'(?<=l)\d{3}', fn).group(0)
    if name not in plotdata:
        plotdata[name] = {}

    entry = plotdata[name]
    times = []

    with open(fn, 'r') as file:
        for line in file:
            if line.startswith('FST'):
                m = record.search(line)
                times.append(float(m.group(0)))

    if not times:
        return

    if int(suffix) == 0:
        entry[0] = times
        return

    if int(suffix) not in entry:
        entry[int(suffix)] = [[0], [0], [0], [0], [0], [0], [0], [0], [0], [0]]

    entry[int(suffix)][int(log(int(chunks), 2))] = times


def load_data():
    for _, dirs, _ in os.walk(src_path):
        for d in dirs:
            if d in bad_dirs:
                continue
            for root, _, files in os.walk(d):
                for fn in files:
                    if fn in bad_files:
                        continue
                    load_file(os.path.join(root, fn))


def plot_data():
    x = [1, 2, 4, 8, 16, 32, 64, 128, 256, 512]
    for key, val in plotdata.iteritems():
        trans = trans_Mbit_per_s(size[key])
        for key2, entry in val.iteritems():
            if key2 == 0:
                continue
            entry[0] = val[0]

        ys = {}

        for key2 , entry in val.iteritems():
            if key2 == 0:
                continue
            y = [[0], [0], [0], [0], [0], [0], [0], [0], [0], [0]]
            for e, i in zip(entry, range(10)):
                if key == "regex2" and i == 6:
                    y[i] = 0
                else:
                    y[i] = np.average(trans['trans_fun'](e))
            ys[key2] = y

        y1 = ys[1]
        y2 = ys[2]
        y4 = ys[4]
        y8 = ys[8]
        y16 = ys[16]
        y32 = ys[32]
        y64 = ys[64]
        y128 = ys[128]
        y256 = ys[256]
        y512 = ys[512]

        ylabel = trans['yaxis_label']
        xlabel = "Number of chunks"
        labels = ['|suffix| =   1', '|suffix| =   2', '|suffix| =   4', '|suffix| =   8', '|suffix| =  16',
                  '|suffix| =  32', '|suffix| =  64', '|suffix| = 128', '|suffix| = 256', '|suffix| = 512']
        colors = ['#5bc0eb', '#fde74c', '#9bc53d', '#e55934', '#fa7921',
                  '#336c84', '#a09330', '#71912d', '#c429b7', '#29c48b']

        plot(trans['title'](key), key + "_suffix_"+ trans['unit'] +"_scale.pdf", xlabel, ylabel, labels, colors, x, y1, y2, y4, y8, y16, y32, y64, y128, y256, y512)


def plot(title, save_as, x_label, y_label,  graph_labels, colors, x, *ys):
    fig, ax = plt.subplots()
    if title:
        plt.title(title)
    if x_label:
        ax.set_xlabel(x_label)
    if y_label:
        ax.set_ylabel(y_label)

    ax.set_xscale('log', basex=2)

    if colors:
        for y, l, c in zip(ys, graph_labels, colors):
            ax.plot(x, y, '-', label=l, color=c)
    else:
        for y, l in zip(ys, graph_labels):
            ax.plot(x, y, '-', label=l)

    ax.xaxis.set_major_formatter(ScalarFormatter())
    ax.yaxis.set_major_formatter(FuncFormatter(lambda x, p: format(int(x), ',')))
    plt.legend(bbox_to_anchor=(1, 1.02), loc="upper left")
    if save_as:
        fig.savefig(save_as, bbox_inches="tight")


if __name__ == "__main__":
    load_data()
    plot_data()
