# -*- coding: utf-8 -*-
"""
Created on September 20 2018
"""
'''
Author: Vamsee Achanta
Date Updated: 2018-09-20
Objective: To generate catenary riser shape and evaluate static configuration
Run instructions with:
 Default yml file : python APISTD2RD.py
 Update default yml with parameters in 12.yml: python APISTD2RD.py 12.yml
 Outputs: JSON file and ASCII DataFrame with outputs

 UPDATES:
 Input YML: Relocate Spacing to PlotSettings
 Rename Summary "Hangoff" to "HangOffToSag"


'''
import logging
import os
import subprocess
import time
from math import *

from common.saveData import saveDataYaml
from common.setLogging import setLogging
# from catenarycalculation import *
from common.ymlInput import ymlInput

# Data preparation
SleepTimeBetweenLiveRuns = 10
defaultYml = "dataManager\\Settings.yml"

# Get updateYML file
try:
    if (sys.argv[1] != None):
        updateYml = "dataManager\\" + sys.argv[1]
        logging.critical("Updating default values with contents in file {0}" .format(updateYml) )
except:
    updateYml = None
    logging.critical("No update values file is provided. Running program default values")

# Get updated configuration file for Analysis
cfg = ymlInput(defaultYml, updateYml)
try:
    cfg['FileName'] = updateYml.split('\\')[1].split('.')[0]
except:
    cfg['FileName'] = defaultYml.split('\\')[1].split('.')[0]

# Set logging
setLogging(cfg['Default']['LogLevel'])
logging.info(cfg)

while True:
    LiveRuns = ymlInput(cfg['LiveRun'], None)
    print(LiveRuns)
    # saveDataYaml({'TrigToRun': [None, None]}, cfg['LiveRun'].split('.')[0])
    for TrigToRunIndex in range(0, len(LiveRuns['TrigToRun'])):
        if LiveRuns['TrigToRun'][TrigToRunIndex] is not None:
            # try:
                # subprocess.call([LiveRuns['TrigToRun'][TrigToRunIndex]])
                # subprocess.Popen([LiveRuns['TrigToRun'][TrigToRunIndex]], shell=True)
                # ActivateEnvironment = cfg['ActivateEnvironment']
                FileName = LiveRuns['TrigToRun'][TrigToRunIndex]
                CurrentWorkingDirectory = os.path.dirname(FileName)
                print(CurrentWorkingDirectory)
                # Process1 = subprocess.Popen([ActivateEnvironment, FileName], cwd=CurrentWorkingDirectory)
                Process1 = subprocess.Popen([FileName], cwd=CurrentWorkingDirectory)
                print("Successful run : {}" .format(LiveRuns['TrigToRun'][TrigToRunIndex]))
                Process1.kill()
            # except:
                print("Failed run : {}" .format(LiveRuns['TrigToRun'][TrigToRunIndex]))
                try:
                    Process1.kill()
                except:
                    print("No process to kill")
        else:
            print("No batch files to run")

    print("Sleeping for {0} s" .format(SleepTimeBetweenLiveRuns))
    time.sleep(SleepTimeBetweenLiveRuns)
