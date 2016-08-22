# -*- coding: utf-8 -*-
"""A Python3 implementation of the Sandia Lab. Thermal Problem"""
import numpy as np


__author__ = "Damar Wicaksono"


def rescale_input(xx: np.ndarray) -> np.ndarray:
    r"""Rescale the normalized input according to actual input ranges
    
    The thermal problem formulation contains 4 input parameters, 2 of which are 
    the control variables (q and L) and the other two are "model" variables 
    (k and rho_cp). The range of parameters is as follows:

     1. q, heat flux at the slab surface, [W.m^-2], [1'000, 3'500]
     2. L, length of the slab, [m], [1.27E-2, 2.54E-2]
     3. k, thermal conductivity, [W.m^-1.K^-1], [0.0455, 0.0811]
     4. rho_cp, volumetric heat capacity, [J.m^-3.K^-1], [3.38E5, 4.69E5]

    **Reference:**
    (1) Kevin J. Dowding, Martin Pilch, and Richard G. Hills, "Formulation of 
        the Thermal Problem," Computer Methods in Applied Mechanics and 
        Engineering, vol. 197, 2008, pp. 2385 - 2389

    :param xx: an n-by-4 normalized input parameters matrix, where n is 
        independent realization
    :return: an n-by-4 rescaled input parameters matrix
    """
    yy = np.empty(xx.shape)
    yy[:,0] = 1000. + (3500. - 1000.) * xx[:,0]         # q 
    yy[:,1] = 1.27E-2 + (2.54E-2 - 1.27E-2) * xx[:,1]   # L
    yy[:,2] = 0.0455 + (0.0811 - 0.0455) * xx[:,2]      # k
    yy[:,3] = 3.38E5 + (4.69E5 - 3.38E5) * xx[:,3]      # rho_cp

    return yy


def eval(xx: np.ndarray, t: np.ndarray, x: float, temp_init: float) -> np.ndarray:
    r"""Evaluate the time-dependent temperature at selected time and position
    
    The Sandia Lab. thermal problem is based on a truncated infinite series 
    solution of heat conduction problem. It contains in total 7 free parameters 
    described below. The first 4 are considered input parameters.

     **Reference:**
    (1) Kevin J. Dowding, Martin Pilch, and Richard G. Hills, "Formulation of 
        the Thermal Problem," Computer Methods in Applied Mechanics and 
        Engineering, vol. 197, 2008, pp. 2385 - 2389

    :param xx: an n-by-4 input parameters matrix containing in sequential 
        column: q, L, k, and rho_cp. n is the number of independent realization
    :param t: time-points array (t) in [s] where the temperature is evaluated
    :param x: lateral location (x) in [m] where the temperature is evaluated
    :param temp_init: the initial temperature (temp_init) in [K]
    :return: an n-by-t.shape[0] of temperature in [K] at each select 
        time points for each input parameters xx
    """
    # Assign the input parameters into local variables
    q = xx[:,0]
    L = xx[:,1]
    k = xx[:,2]
    rho_cp = xx[:,3]

    # Create an array of output
    n_s = xx.shape[0]   # number of samples
    n_t = t.shape[0]    # number of time points 
    yy = np.empty([n_s, n_t])
    
    # Loop over time-points and evaluate the temperature
    for i, time in enumerate(t):
        if time < 0.:
            raise ValueError("Not valid for negative time")
        elif time < 1E-6:
            yy[:,i] = temp_init
        else:
            # Compute the sum of the series
            series_sum = 0
            for j in range(1,7):
                series_sum += 1/j**2 * \
                    np.exp(-1 * j**2 * np.pi**2 * k / rho_cp * time / L**2) * \
                    np.cos(j * np.pi * x / L)
            series_sum *= 2 / np.pi**2
            
            # Compute the temperature
            yy[:,i] = np.repeat(temp_init, n_s) + q * L / k * \
                (k / rho_cp * time / L**2 + 1.0/3.0 - x / L + \
                0.5 * (x/L)**2 - series_sum)
        
    return yy
