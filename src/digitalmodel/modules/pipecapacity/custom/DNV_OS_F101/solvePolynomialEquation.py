# -*- coding: utf-8 -*-
"""
Created on Thu Jul 17 2018

@author: achantv
"""

#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Author: Vamsee Achanta
Date Updated: 2018-17-07
Objective: To solve a polynomial equation
'''
import numpy as np


# ACTION : Convert to class function
def solvePolynomialEquation(coefficients):
    # coefficients = [3.2, 2, 1]
    solution =  np.roots(coefficients )
    return solution

