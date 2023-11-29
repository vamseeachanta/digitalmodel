'''
Author: Vamsee Achanta
Date Updated: 2018-04-24
Objective: VM stress calculation utilizing API RP 2RD code
Outputs: VM Stress operating envelope
Running Program:
python VMCalculation.py --log DEBUG
'''

import math
import matplotlib.pyplot as pyplot
import munch

from data_manager.argumentParseFunction import *
from data_manager.setLogging import *
from data_manager.parameters import *
from calculations.VMStress import *
from calculations.pipeProperties import *

# Get configuration data
configData = argumentParseFunction()
# Set logging settings
setLogging(configData.log[0])

# DATA PROVISION Convert data into accessible class attributes
pipeData = munch.munchify(pipeInput)
codeParameters = munch.munchify(codeParameterInput)
loading = munch.munchify(loadingData)

# CALCULATION of pipe properties
# Add calculated structural properties of pipe as attributes to the same object
pipeData = pipeProperties(pipeData)

# perform VM Stress calculation
VMStress(pipeData, loading, codeParameters)
