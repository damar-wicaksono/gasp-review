import numpy as np

def eval(x: np.ndarray) -> np.ndarray:
    r"""Evaluate the 1D dampened cosine test function
    
    :math:`y(x) = e^{-1.4 \cdot x} \cdot cos(3.5 \cdot \pi \cdot x)`
    
    **Reference:**
    (1) T. Santner, B. Williams, and W. Notz, "The Design and Analysis of 
        Computer Experiments," pp. 56, Springer-Verlag, New York, 2003.

    :param x: a vector of input [0,1]
    :returns: a vector of output
    """
    return np.exp(-1.4 * x) * np.cos(3.5 * np.pi * x)
