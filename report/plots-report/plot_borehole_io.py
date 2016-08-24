#!/usr/bin/env python
# title         : plot_borehole_io.py
# description   : Create a set of plots of borehole function output at sampled
#               : points for each input dimension
# author        : WD41, LRS/EPFL/PSI
# date          : August 2016
# usage         : python plot_borehole_io.py <output png file>
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
    x_labels = ["$r_w \, \mathrm{[m]}$", "$r \, \mathrm{[m]}$", 
                "$T_u \, \mathrm{[m^2\cdot year^{-1}]}$", 
                "$H_u \, \mathrm{[m]}$", 
                "$T_l \, \mathrm{[m^2\cdot year^{-1}]}$", 
                "$H_l \, \mathrm{[m]}$", 
                "$L \, \mathrm{[m]}$", 
                "$K_w \, \mathrm{[m\cdot year^{-1}]}$"]

    # Create the data (function argument)
    np.random.seed(97752)
    xx_norm = np.random.rand(500, 8)
    # Rescale the input parameters
    xx = borehole.rescale_input(xx_norm)
    # Evaluate the function
    y = borehole.eval(xx)
    # Plot the figure
    fig, axarr = plt.subplots(2, 4, figsize=(16,8))
    k = 0
    for i in range(2):
        for j in range(4):
            axarr[i,j].scatter(xx[:,k], y, facecolors="none")
            axarr[i,j].set_ylim([-10, 250])
            axarr[i,j].set_xlabel(x_labels[k], fontsize=16)
            axarr[i,j].set_ylabel("Flow Rate $\mathrm{[m^3\cdot year^{-1}]}$",
                                  fontsize=16)
            axarr[i,j].ticklabel_format(style="sci", axis="x", scilimits=(0,0))
            axarr[i,j].grid()
            k += 1
    
    plt.tight_layout()
    plt.savefig(sys.argv[-1], format="PNG", dpi=600)


if __name__ == "__main__":
    main()
