#!/usr/bin/python

import sys

from matplotlib import pyplot as plt
from scipy import stats
import numpy as np

num_bins = 100

split_data_no_opt_lines = map(lambda x: x.split(','), open('data_no_opt').readlines())
data_no_opt_a = map(lambda x: float(x[1]), filter(lambda x: x[0] == 'a', split_data_no_opt_lines))
data_no_opt_b = map(lambda x: float(x[1]), filter(lambda x: x[0] == 'b', split_data_no_opt_lines))

split_data_with_opt_lines = map(lambda x: x.split(','), open('data_with_opt').readlines())
data_with_opt_a = map(lambda x: float(x[1]), filter(lambda x: x[0] == 'a', split_data_with_opt_lines))
data_with_opt_b = map(lambda x: float(x[1]), filter(lambda x: x[0] == 'b', split_data_with_opt_lines))

plt.subplot(221)
plt.hist(data_no_opt_a, label='no opt', bins=num_bins, histtype='stepfilled', normed=True, color='#AA0000', alpha=0.5)
plt.hist(data_with_opt_a, label='with opt', bins=num_bins, histtype='stepfilled', normed=True, color='#00AA00', alpha=0.5)
plt.title('a')
plt.xlabel('a')
plt.ylabel('frequency')
plt.legend()

plt.subplot(222)
plt.hist(data_no_opt_b, label='no opt',bins=num_bins, histtype='stepfilled', normed=True, color='#AA0000', alpha=0.5)
plt.hist(data_with_opt_b, label='with opt', bins=num_bins, histtype='stepfilled', normed=True, color='#00AA00', alpha=0.5)
plt.title('b')
plt.xlabel('b')
plt.ylabel('frequency')
plt.legend()

plt.subplot(223)
heatmap, xedges, yedges = np.histogram2d(data_no_opt_a, data_no_opt_b, bins=num_bins)
extent = [xedges[0], xedges[-1], yedges[0], yedges[-1]]
plt.imshow(heatmap, extent=extent, aspect='auto')
plt.title('a x b (no opt)')
plt.xlabel('a')
plt.ylabel('b')

plt.subplot(224)
heatmap, xedges, yedges = np.histogram2d(data_with_opt_a, data_with_opt_b, bins=num_bins)
extent = [xedges[0], xedges[-1], yedges[0], yedges[-1]]
plt.imshow(heatmap, extent=extent, aspect='auto')
plt.title('a x b (with opt)')
plt.xlabel('a')
plt.ylabel('b')

plt.show()
