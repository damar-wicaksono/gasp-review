import unittest
import numpy as np
import sandiatherm

class SandiaThermalTestCase(unittest.TestCase):
    """Tests for `sandia_thermal.py`"""

    def test_is_solution_verified(self):
        """Check the verification data"""
        # Reference values are taken from the reference paper
        yy_ref = np.array([25.0, 264.365410, 363.582289, 440.597591, 
                          507.977177, 570.904767, 631.761990, 691.655773, 
                          751.101191, 810.337947, 869.477597])
        t = np.linspace(0, 1000., 11)
        xx_ver = np.array([[3000., 0.0127, 0.05, 0.4E6]])
        yy_ver = sandiatherm.eval(xx_ver, t, 0.0, 25.)
        for ver, ref in zip(yy_ver[0,:], yy_ref):
            self.assertAlmostEqual(ver, ref, places=6)


if __name__ == "__main__":
    unittest.main()