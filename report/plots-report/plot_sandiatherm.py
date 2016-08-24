#!/usr/bin/env python
# title         : plot_sandiatherm.py
# description   : Create a set of plots of the Sandia Lab. Thermal Challenge
#               : problem for each of the configurations
# author        : WD41, LRS/EPFL/PSI
# date          : August 2016
# usage         : python plot_sandiatherm.py <output png file>
# py_version    : 3.5 Anaconda
import os
import sys
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
sys.path.append(os.path.abspath("../../simulation/"))

from py_test_functions import sandiatherm


def get_ij(ind: int, max_j: int):
    """Get double index from a single index"""
    import math
    # get the first Index 
    i = math.floor(ind/max_j)
    j = ind%max_j

    return (i,j)


def main():
    # Create the plot title
    plt_titles = ["Configuration 1", "Configuration 2", "Configuration 3", 
                  "Configuration 4", "Configuration 5", "Configuration 6"]
    t = np.linspace(0., 1000., 11)  # Create the time grid
    x = 0.0                         # The relevant lateral location
    temp_init = 25 + 273.15         # initial temperature in [K]
    # The nominal values for q and L (normalized)
    ql_nominal = [[0., 0.], [0., 1.], [0.4, 0.], 
                  [0.4, 1.], [0.8, 0.5], [1.0, 0.5]]
    # Generate samples for k and rho_cp (normalized)
    np.random.seed(97752)
    n_s = 50
    k_rhocp = np.random.rand(n_s, 2)

    # Loop over configurations and make the plot
    fig, axarr = plt.subplots(2, 3, figsize=(12,8))
    for ind in range(6):
        # The nominal L
        xx_norm = np.column_stack((
            np.array([np.repeat(ql_nominal[ind][1], n_s)]).T,
            k_rhocp))
        # The nominal q
        xx_norm = np.column_stack((
            np.array([np.repeat(ql_nominal[ind][0], n_s)]).T,
            xx_norm))
        # Rescale the input parameters
        xx = sandiatherm.rescale_input(xx_norm)
        # Evaluate the function
        yy = sandiatherm.eval(xx, t, x, temp_init)
        # Plot the figure
        i, j = get_ij(ind, max_j=3)
        axarr[i,j].set_title(plt_titles[ind])
        axarr[i,j].grid()
        axarr[i,j].set_xlim([0, 1050])
        axarr[i,j].set_ylim([250, 1200])
        axarr[i,j].set_xlabel("Time $\mathrm{[s]}$")
        axarr[i,j].set_ylabel("Surface Temperature $\mathrm{[K]}$")
        axarr[i,j].axhline(y=1173.15, xmin=0, xmax=1050, ls="--", color="black")
        for k in range(n_s):
            axarr[i,j].plot(t, yy[k,:], lw=0.25, color="black")
            

    plt.tight_layout()
    plt.savefig(sys.argv[-1], format="PNG", dpi=600)


if __name__ == "__main__":
    main()
