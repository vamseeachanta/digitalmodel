# -*- coding: utf-8 -*-
"""
Created on Tue Apr 24 16:07:23 2018

@author: vamsee.achanta
"""
import math
import logging

def VMStress(pipe, loading, codeParameters):

    ## calculate these properties as a new class and add to pipe properties

    SigmaA = codeParameters.AllowableStressFac*pipe.pipeYieldStrength  # Basic
    logging.info( "SigmaA : " + str(round(SigmaA,5)) )
    
    SigmaACf = codeParameters.DesignCaseFac*SigmaA  # R.H.S of Equation
    logging.info("Stress(R.H.S) in kPa : " + str(round(SigmaACf*1E-3,5)) )
    logging.info("Stress(R.H.S) in ksi: " + str(round(SigmaACf*1.45038E-007,5)))
    
    sigmaRadial_1 = -((loading.ExternalPressure*pipe.NominalOD)+(loading.InternalPressure*pipe.NominalID))/(pipe.NominalOD+pipe.NominalID)
    logging.info("sigmaRadial_1 : " + str(round(sigmaRadial_1,5)) ) 

    sigmaCircuferential_1 = (((loading.InternalPressure-loading.ExternalPressure)*pipe.NominalOD)/(2*pipe.MinimumWT))-loading.InternalPressure
    logging.info("sigmaCircuferential_1 : " + str(round(sigmaCircuferential_1,5)))
    
    logging.info("BendingMoment_pipe : " + str(round(loading.Moment,5)))
    
    Tension_LHS = 2*(((loading.Moment/(2*pipe.I))*(pipe.NominalOD- pipe.MinimumWT))**2)
    logging.info("Tension_LHS : " + str(round(Tension_LHS,5))) 

    Tension_RHS = (2*SigmaACf**2)-(sigmaRadial_1 -sigmaCircuferential_1)**2-Tension_LHS
    logging.info("Tension_RHS : " + str(round(Tension_RHS,5)))

    a = 2*((1/pipe.A)**2)
    b = (4*((loading.Moment/(2*pipe.I))*(pipe.NominalOD- pipe.MinimumWT)))*(1/pipe.A)
    c = -Tension_RHS
    Tension_positive1 = (-b+math.sqrt(b**2-(4*a*c)))/(2*a)
    Tension_negative1 = (-b-math.sqrt(b**2-(4*a*c)))/(2*a)

