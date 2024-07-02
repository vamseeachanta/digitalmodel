#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Author: Vamsee Achanta
Date Updated: 2017-11-25
Objective: To set logging 
Outputs: Settings for logging
'''
import logging
import os
def setLogging(logLevel):
    logNumericLevel = getattr(logging, logLevel.upper())

    # Create log directory if not existing
    logDirectory = os.getcwd()+'\logger'
    if not os.path.exists(logDirectory):
        os.makedirs(logDirectory)

    if not isinstance(logNumericLevel, int):
        raise ValueError('Invalid log level: %s' % loglevel)

    # Basic configuration for logging
    logging.basicConfig(level=logNumericLevel, 
                        format='%(asctime)s %(message)s', datefmt='%m/%d/%Y %I:%M:%S %p',
                        filename='logger/' + logLevel + '.log', filemode='w')

