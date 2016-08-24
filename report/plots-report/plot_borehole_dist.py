#!/usr/bin/env python
# title         : plot_borehole_dist.py
# description   : Create a set of plots of histogram for borehole input 
#               : parameters and the output
# author        : WD41, LRS/EPFL/PSI
# date          : August 2016
# usage         : python plot_borehole_dist.py <output png file>
# py_version    : 3.5 Anaconda
import os
import sys
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
sys.path.append(os.path.abspath("../../simulation/"))

from py_test_functions import borehole


def main():
    # Create the axis label
    x_labels = ["$r_w$", "$r$", 
                "$T_u$", 
                "$H_u$", 
                "$T_l$", 
                "$H_l$", 
                "$L$", 
                "$K_w$"]

    # Create the data (function argument)
    np.random.seed(97752)
    xx_norm = np.random.rand(500, 8)
    # Rescale the input parameters
    xx = borehole.rescale_input(xx_norm)
    # Evaluate the function
    y = borehole.eval(xx)
    # Plot the figure
    fig = plt.figure(figsize=(12,4))
    axarr = []
    axarr.append(plt.subplot2grid((2,6), (0,0)))
    axarr.append(plt.subplot2grid((2,6), (0,1)))
    axarr.append(plt.subplot2grid((2,6), (0,2)))
    axarr.append(plt.subplot2grid((2,6), (0,3)))
    axarr.append(plt.subplot2grid((2,6), (1,0)))
    axarr.append(plt.subplot2grid((2,6), (1,1)))
    axarr.append(plt.subplot2grid((2,6), (1,2)))
    axarr.append(plt.subplot2grid((2,6), (1,3)))
    axarr.append(plt.subplot2grid((2,6), (0,4), colspan=2, rowspan=2))

    matplotlib.rc('font', size=10)
    k = 0
    for i in range(2):
        for j in range(4):
            axarr[k].hist(xx[:,k], color="black", alpha=0.75)
            axarr[k].set_yticks(np.array([0, 30, 60]))
            axarr[k].xaxis.set_tick_params(labelsize=9)
            axarr[k].yaxis.set_tick_params(labelsize=9)
            axarr[k].set_xticks(np.array([np.round(np.min(xx[:,k]),2), 
                                          np.round(np.median(xx[:,k]),2),
                                          np.round(np.max(xx[:,k]),2)]))
            axarr[k].set_xlabel(x_labels[k])
            axarr[k].ticklabel_format(style="sci", axis="x", scilimits=(0,0))
            k += 1
    
    axarr[8].hist(y, color="black", alpha=0.75)
    axarr[8].set_xlabel("$y \, \mathrm{[m^3\cdot year^{-1}]}$",
                                  fontsize=16)

    plt.tight_layout()
    plt.savefig(sys.argv[-1], format="PNG", dpi=600)


if __name__ == "__main__":
    main()