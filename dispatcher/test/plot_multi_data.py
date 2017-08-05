import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter
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

sizes = {'flip_ab': {
    '1mb': 1024001,
    '2mb': 2048002,
    '4mb': 4096004,
    '8mb': 8192008,
    '16mb': 16384016,
    '32mb': 32768032,
    '64mb': 65536064,
    '128mb': 131072128,
    '256mb': 262144256,
    '512mb': 524288512,
    '1024mb': 1048577024,
    '2048mb': 2097154048,
    '4096mb': 4194308096,
    '8192mb': 8388616192,
    '16384mb': 16777232384
},
    'csv_project3': {
        '1mb': 1024109,
        '2mb': 2048218,
        '4mb': 4096436,
        '8mb': 8192872,
        '16mb': 16385744,
        '32mb': 32771488,
        '64mb': 65542976,
        '128mb': 131085952,
        '256mb': 262171904,
        '512mb': 524343808,
        '1024mb': 1048687616,
        '2048mb': 2097375232,
        '4096mb': 4194750464,
        '8192mb': 8389500928,
        '16384mb': 16779001856
    },
    'DIV7-gen': {
        '1mb': 1048576,
        '2mb': 2097152,
        '4mb': 4194304,
        '8mb': 8388608,
        '16mb': 16777216,
        '32mb': 33554432,
        '64mb': 67108864,
        '128mb': 134217728,
        '256mb': 268435456,
        '512mb': 536870912,
        '1024mb': 1073741824,
        '2048mb': 2147483648,
        '4096mb': 4294967296,
        '8192mb': 8589934592,
        '16384mb': 17179869184
    },
    'irc': {
        '1mb': 1024027,
        '2mb': 2048054,
        '4mb': 4096108,
        '8mb': 8192216,
        '16mb': 16384432,
        '32mb': 32768864,
        '64mb': 65537728,
        '128mb': 131075456,
        '256mb': 262150912,
        '512mb': 524301824,
        '1024mb': 1048603648,
        '2048mb': 2097207296,
        '4096mb': 4194414592,
        '8192mb': 8388829184,
        '16384mb': 16777658368
    },
    'thousand_sep': {
        '1mb': 1024001,
        '2mb': 2048002,
        '4mb': 4096004,
        '8mb': 8192008,
        '16mb': 16384016,
        '32mb': 32768032,
        '64mb': 65536064,
        '128mb': 131072128,
        '256mb': 262144256,
        '512mb': 524288512,
        '1024mb': 1048577024,
        '2048mb': 2097154048,
        '4096mb': 4194308096,
        '8192mb': 8388616192,
        '16384mb': 16777232384
    },
}


def trans_Mbit_per_s(inputsize_bytes):
    return {"median_format_string": "%.1f",
            "trans_fun": lambda ps: map(lambda p: ((inputsize_bytes * 8) / 1e6) / (p / 1000.0), ps),
            "yaxis_label": "Mbit/s",
            "title": lambda prog: "%s (%.2f MB)" %
                                  (prog, inputsize_bytes / 2.0 ** 20),
            "inputsize_bytes": inputsize_bytes,
            "unit": 'Mbit'
            }


def load_file(fn):
    if 'bin16' in fn or 'bin64' in fn:
        return
    name = fn.split("_data")[0]
    size = fn.split("_bin_")[1][:-4]
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

    if size in entry:
        raise "Multiple data"
    entry[size] = times


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
    x = [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384]
    data = {}

    for key, val in plotdata.iteritems():
        y = [0] * 15
        for key2, entry in val.iteritems():
            trans = trans_Mbit_per_s(sizes[key][key2])
            y[int(log(int(key2[:-2]),2))] = np.average(trans['trans_fun'](entry))

        data[key] = y

    ab = data['flip_ab']
    csv = data['csv_project3']
    div7 = data['DIV7-gen']
    irc = data['irc']
    thou = data['thousand_sep']

    ylabel = trans['yaxis_label']
    xlabel = "Input data in MB"
    labels = ['flip_ab', 'csv_project3', 'DIV7-gen', 'irc', 'thousand_sep']

    colors = ['#5bc0eb', '#9bc53d', '#e55934', '#fa7921', '#336c84',
              '#a09330', '#71912d', '#c429b7', '#29c48b', '#fde74c']

    plot('Throughput', "throughput_" + trans['unit'] + ".pdf", xlabel, ylabel, labels, colors, x, ab, csv, div7, irc, thou)


def plot(title, save_as, x_label, y_label, graph_labels, colors, x, *ys):
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

    ax.yaxis.set_major_formatter(FuncFormatter(lambda x, p: format(int(x), ',')))

    plt.legend(bbox_to_anchor=(1, 1.02), loc="upper left")
    if save_as:
        fig.savefig(save_as, bbox_inches="tight")


if __name__ == "__main__":
    load_data()
    plot_data()
