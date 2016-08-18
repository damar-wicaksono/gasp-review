# -*- coding: utf-8 -*-
"""A Python3 implementation of the Borehole function"""
import numpy as np


__author__ = "Damar Wicaksono"


def rescale_input(xx: np.ndarray) -> np.ndarray:
    r"""Rescale the normalized input according to the borehole input range
    
    The range of input parameter of parameter 8 are taken from [1] where it 
    was modified from the original range in [2] to induce more non-linearities
    and non-additivity to the output. The range of the parameters is as follow:

     1. rw, radius of the borehole, [m], [0.05, 0.15]
     2. r, radius of influence, [m], [100, 50'000]
     3. Tu, transmissivity of upper aquifer, [m^2.year^-1], [63'070, 115'600]
     4. Hu, potentiometric head of upper aquifer, [m], [990, 1'110]
     5. Tl, transmissivity of lower aquifer, [m^2.year^-1], [63.1, 116]
     6. Hl, potentiometric head of lower aquifer, [m], [700, 820]
     7. L, length of the borehole, [m], [1'120, 1'680]
     8. Kw, hydraulic conductivity of the borehole, [m.year^-1],[1'500, 15'000]

    **References:**
    (1) Max D. Morris, Toby J. Mitchell, and Donald Ylvisaker, "Bayesian Design
        and Analysis of Computer Experiments: Use of Derivatives in Surface 
        Prediction," Technometrics, vol. 35, no. 3, 1993, pp. 243-255
    (2) Brian A. Worley, "Deterministic Uncertainty Analysis," in Proc. of 
        the American Nuclear Society Winter Meeting, Los Angeles, USA, 1987

    :param xx: an n-by-8 normalized input parameters matrix, where n is 
        independent realization
    :return: an n-by-8 rescaled input parameters matrix
    """
    yy = np.empty(xx.shape)
    yy[:,0] = 0.05 + (0.15 - 0.05) * xx[:,0]        # rw
    yy[:,1] = 100. + (50000. - 100.) * xx[:,1]      # r
    yy[:,2] = 63070. + (115600. - 63070.) * xx[:,2] # Tu
    yy[:,3] = 990. + (1110. - 990.) * xx[:,3]       # Hu
    yy[:,4] = 63.1 + (116. - 63.1) * xx[:,4]        # Tl
    yy[:,5] = 700. + (820. - 700.) * xx[:,5]        # Hl
    yy[:,6] = 1120. + (1680. - 1120.) * xx[:,6]     # L
    yy[:,7] = 1500. + (15000. - 1500.) * xx[:,7]    # Kw

    return yy


def eval(xx: np.ndarray) -> np.ndarray:
    r"""Evaluate the flow rate through the borehole using the Borehole function
    
    :math:`y = \frac{2 \, \pi \, T_u (H_u - H_l)}{\ln(r/r_w) 
        [1 + \frac{2\,L\,T_u}{\ln(r/r_w)\,r_{w}^2\,K_w} + T_u/T_l]}`

    The borehole function is 8-dimensional scalar function. The input is the 
    following and listed as the column of the input matrix:
     1. rw, radius of the borehole, [m]
     2. r, radius of influence, [m]
     3. Tu, transmissivity of upper aquifer, [m^2.year^-1]
     4. Hu, potentiometric head of upper aquifer, [m]
     5. Tl, transmissivity of lower aquifer, [m^2.year^-1]
     6. Hl, potentiometric head of lower aquifer, [m]
     7. L, length of the borehole, [m]
     8. Kw, hydraulic conductivity of the borehole, [m.year^-1]

    The output of the function is the flow rate through the borehole in 
    [m^3.year^-1]

    **Reference:**
    (1) Max D. Morris, Toby J. Mitchell, and Donald Ylvisaker, "Bayesian Design
        and Analysis of Computer Experiments: Use of Derivatives in Surface 
        Prediction," Technometrics, vol. 35, no. 3, 1993, pp. 243-255

    :param xx: a n-by-8 input parameters matrix, where n is independent 
        realization
    :return: a vector of output, flow rate through the borehole [m^3.year^-1]
    """
    # Assign the input parameters to local variables
    rw = xx[:,0]
    r = xx[:,1]
    Tu = xx[:,2]
    Hu = xx[:,3]
    Tl = xx[:,4]
    Hl = xx[:,5]
    L = xx[:,6]
    Kw = xx[:,7]

    # Calculate the flow rate
    y = (2 * np.pi * Tu * (Hu - Hl)) / np.log(r/rw) / \
        (1 + 2 * L * Tu / np.log(r/rw) / rw**2 / Kw + Tu / Tl)

    return y
