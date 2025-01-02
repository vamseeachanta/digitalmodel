# -*- coding: utf-8 -*-
"""
Created on Thu May 17 10:05:12 2018

@author: achantv
"""

#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Author: Vamsee Achanta
Date Updated: 2017-11-25
Objective: To set logging level. logLevels can be 'DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL'
Outputs: Settings for logging
'''
from sympy import *

x = symbols("x")
solution =  solve(x**2-2,x)
print(solution)

print(solution[0])
print(float(solution[0]))

