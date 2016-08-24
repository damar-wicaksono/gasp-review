#!/usr/bin/env python
# title         : plot_cos_dampened.py
# description   : Create a plot of 1D dampened cosine test function
# author        : WD41, LRS/EPFL/PSI
# date          : August 2016
# usage         : python plot_cos_dampened.py <output png file>
# py_version    : 3.5 Anaconda
import os
import sys
import numpy as np
import matplotlib.pyplot as plt
sys.path.append(os.path.abspath("../../simulation/"))

from py_test_functions import cos_dampened


def main():
    # Create the data (function argument)
    x = np.linspace(0, 1, num = 100)
    # Evaluate the function
    y = cos_dampened.eval(x)
    # Plot the figure
    fig = plt.figure(figsize=(4,4))
    ax = fig.add_subplot(111)
    ax.plot(x, y, color="black")
    ax.set_xlabel("$x$ [-]")
    ax.set_ylabel("$y$ [-]")
    plt.grid()
    plt.tight_layout()
    plt.savefig(sys.argv[-1], format="PNG")


if __name__ == "__main__":
    main()
