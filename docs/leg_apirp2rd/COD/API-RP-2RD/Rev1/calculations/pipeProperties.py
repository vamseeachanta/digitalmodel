# -*- coding: utf-8 -*-
"""
Created on Tue Apr 24 16:07:23 2018

@author: vamsee.achanta
"""
import logging
import math
def pipeProperties(pipe):

    ## calculate these properties as a new class and add to pipe properties

    pipe.NominalID = pipe.NominalOD - 2*pipe.NominalWT  # inside diameter
    logging.info( "pipeNominalInsideDiameter : "  +  str(round(pipe.NominalID,5)) )

    
    pipe.MinimumID = pipe.NominalOD -(2*pipe.MinimumWT)
    logging.info( 'pipeMinimumID_m : ' + str(round(pipe.MinimumID,5)) )
    
    pipe.A = (math.pi/4)*(pipe.NominalOD**2 - pipe.MinimumID**2)  # Area
    logging.info( "pipeArea : " + str(round(pipe.A,5)) )

    pipe.Ai = (math.pi/4)*(pipe.MinimumID**2)
    logging.info( "pipeAi : "  + str(round(pipe.Ai,5)) )

    pipe.Ao = (math.pi/4)*(pipe.NominalOD**2)
    logging.info( "pipeAo : " + str(round(pipe.Ao,5)) )

    pipe.I = (math.pi/64)*(pipe.NominalOD**4 - pipe.MinimumID**4)  # Moment of Intertia
    logging.info( "pipeIntertia : " + str(round(pipe.I,5)) )

    return pipe