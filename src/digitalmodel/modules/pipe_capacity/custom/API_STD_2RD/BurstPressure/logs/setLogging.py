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
import datetime
import logging
import os


def setLogging(logLevel):
    logNumericLevel = getattr(logging, logLevel.upper())

    if not isinstance(logNumericLevel, int):
        raise ValueError('Invalid log level: %s' % loglevel)

    # Create log directory if not existing
    logDirectory = os.getcwd()+'\logs'
    if not os.path.exists(logDirectory):
        os.makedirs(logDirectory)

    programStartTime = datetime.datetime.now()

    # Basic configuration for logging
    logging.basicConfig(level=logNumericLevel, 
                        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s', 
                        datefmt='%m/%d/%Y %I:%M:%S %p',
                        filename='logs/' + programStartTime.strftime('%Y%m%d_%Hh%Mm_') + logLevel + '.log', filemode='w')
    