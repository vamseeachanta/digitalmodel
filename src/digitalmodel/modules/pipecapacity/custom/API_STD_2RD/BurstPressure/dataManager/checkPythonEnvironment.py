#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Author: Vamsee Achanta
Date Updated: 2018-06-01
Objective: To check python environment
Outputs: Settings for logging
'''
import logging
import os


def checkPythonEnvironment(pythonVirtualEnvironment):

    osPath = os.__file__
    osPathList = osPath.split('\\')
    
    if(pythonVirtualEnvironment in osPathList):
        logging.critical("Running in project python virtual environment : " + pythonVirtualEnvironment)
    else:
        logging.critical("Warning: NOT running in project python environment. program may NOT find all required dependencies.")
