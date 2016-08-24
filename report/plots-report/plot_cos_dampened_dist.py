#!/usr/bin/env python
# title         : plot_cos_dampened_dist.py
# description   : Create a plot of distribution of output for the dampened
#               : cosine test function
# author        : WD41, LRS/EPFL/PSI
# date          : August 2016
# usage         : python plot_cos_dampened_dist.py <output png file>
# py_version    : 3.5 Anaconda
import os
import sys
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
sys.path.append(os.path.abspath("../../simulation/"))

from py_test_functions import cos_dampened


def main():
    # Create the data (function argument)
    np.random.seed(97752)
    x = np.random.rand(500, 1)
    # Evaluate the function
    y = cos_dampened.eval(x)
    # Plot the figure
    matplotlib.rc('font', size=12)
    fig = plt.figure(figsize=(4,4))
    ax = fig.add_subplot(111)
    ax.hist(y, color="black", alpha=0.75)
    ax.set_xlim(-0.8, 1.1)
    ax.set_xlabel("$y$ [-]")
    ax.set_ylabel("Frequency [-]")
    plt.grid()
    plt.tight_layout()
    plt.savefig(sys.argv[-1], format="PNG")


if __name__ == "__main__":
    main()
