########################################################################################################
#
#  This code provides users of Ansys Aqwa with examples of how to use the interface allowing them to 
#  apply forces onto Aqwa structures at runtime. Please do not use if you are not a registered user of Aqwa.
#
########################################################################################################
from AqwaServerMgr import *

########################################################################################################
#
# The user would typically define one or more functions. As an example, we define three of them below :
#
#      UF1, UF2, and UF3
#
#      After that, at the bottom of the file, we instanciate the Server
#
########################################################################################################
# Simple example
# Elastic force maintaining the structure around (0.0)
# AddMass is initialized and kept with a funny pattern in it's values (easily spottable in a output listing)
def UF1(Analysis,Mode,Stage,Time,TimeStep,Pos,Vel):
    # Check that input file is the one expected without .DAT extension

    Error   = 0

    ExpectedFileName = "AD_PYTHONUSERFORCE"
    ActualFileName = Analysis.InputFileName.split("\\")[-1] # We strip the path part of the filename for easy comparison
    if (ActualFileName!=ExpectedFileName):
        print("Error. Incorrect input file !")
        print("Expected : "+ ExpectedFileName)
        print("Actual : "+ActualFileName)
        Error = 1    # Will cause Aqwa to stop

    # If this passed, we create an empty container for AddMass and Force
    AddMass = BlankAddedMass(Analysis.NOfStruct)
    Force   = BlankForce(Analysis.NOfStruct)

    # User defined code here
    # Here additional inertial force for structure s : F[s][dof] = AddMass[s][dof2][dof] * Acc[s][dof2] 
    # (Einstein notation, and Python Row-Major order of array indices)
    for s in range(Analysis.NOfStruct):
        for dof in range(6):
            Force[s][dof] = -10000.0*Pos[s][dof] # Elastic Force
            for dof2 in range(6):
                AddMass[s][dof2][dof]=1e-3*(dof2*6+dof) # AddMass goes [0.000, 0.001, 0.002, ..., 0.036]

    # Now return the results

    return Force,AddMass,Error

########################################################################################################
# Example applying a vertical Force away from the centre of gravity
# 
def UF2(Analysis,Mode,Stage,Time,TimeStep,Pos,Vel):
    AddMass = BlankAddedMass(Analysis.NOfStruct)
    Force   = BlankForce(Analysis.NOfStruct)
    Error   = 0

    ExpectedFileName = "AD_PYTHONUSERFORCE"
    ActualFileName = Analysis.InputFileName.split("\\")[-1] # We strip the path part of the filename for easy comparison
    if (ActualFileName!=ExpectedFileName):
        print("Error. Incorrect input file !")
        print("Expected : "+ ExpectedFileName)
        print("Actual : "+ActualFileName)
        Error = 1    # Will cause Aqwa to stop


    # Note, we use the R_Control values specified in the DAT file if they are not zero.
    if (Analysis.R_Control[5]==0.0):
        coef = -10000.0
    else:
        coef = Analysis.R_Control[5] # Should be set to 30000 in the Aqwa input file.

    VertForce   = ( 0, 0, coef)

    # Application point coordinates in Deck 1's system of coordinates (using Node 3 of AD_PYTHONUSERFORCE.DAT)

    DefPos  = ( 0, 17.53, 0)

    CurPos = Analysis.GetNodeCurrentPosition(Struct=0,
                                             DefAxesX=DefPos[0],
                                             DefAxesY=DefPos[1],
                                             DefAxesZ=DefPos[2])

    ExtraForce = Analysis.ApplyForceOnStructureAtPoint(Struct=0,
                                                       FX=VertForce[0],
                                                       FY=VertForce[1],
                                                       FZ=VertForce[2],
                                                       AppPtX=CurPos[0],
                                                       AppPtY=CurPos[1],
                                                       AppPtZ=CurPos[2])

    # Add the extra force generated to initially empty force 
    # (BlankForce and BlankAddedMass classes support algebraic operations (+, -, and scalar multiplication)

    Force += ExtraForce

    # Note : Looking at the .LIS file, the user should be able to verify that we have just set 
    #        this force so that it exactly negates the mooring force associated to the FORC card in deck 14.
    # Now return the results

    return Force,AddMass,Error

########################################################################################################
# A third simple example
# AddMass is kept zeroed
# Force is acting only on X,Y.
# It is still proportional to X,Y pos
# But rotated. The structure should go around in circles
def UF3(Analysis,Mode,Stage,Time,TimeStep,Pos,Vel):
    AddMass = BlankAddedMass(Analysis.NOfStruct)
    Force   = BlankForce(Analysis.NOfStruct)
    Error   = 0

    # User defined code here
    for s in range(Analysis.NOfStruct):
        Force[s][0] = 10000.0*Pos[s][1]
        Force[s][1] = -10000.0*Pos[s][0]

    # Now return the results

    return Force,AddMass,Error


########################################################################################################
# A Custom Function for Fender

def UFFender(Analysis,Mode,Stage,Time,TimeStep,Pos,Vel):
    AddMass = BlankAddedMass(Analysis.NOfStruct)
    Force   = BlankForce(Analysis.NOfStruct)
    Error   = 0

    # Fender Definition
    delta_L_cfg = {'Struct': 0, 'DefPos': ( 149.11, 18.02, 0.)}
    ForceApply_cfg = {0: ( 149.11, 18.02, 0.), 1: (150, 25, 0)}

    Struct = delta_L_cfg['Struct']
    DefPos = delta_L_cfg['DefPos']
    CurPos = Analysis.GetNodeCurrentPosition(Struct=Struct ,
                                             DefAxesX=DefPos[0],
                                             DefAxesY=DefPos[1],
                                             DefAxesZ=DefPos[2])

#TODO Should we give DefPos or CurPos to GetNodeCurrentVelocity?
    CurVel = Analysis.GetNodeCurrentVelocity(Struct=Struct ,    
                                             DefAxesX=CurPos[0],
                                             DefAxesY=CurPos[1],
                                             DefAxesZ=CurPos[2])
    
    fender_node_change = CurPos[1] - DefPos[1]
    L0 = 6.984
    delta_L = fender_node_change 
    if delta_L  > 0:
        stiffener_force = 50000 * delta_L+ 3000*delta_L**2
    else:
        stiffener_force = 0
    dampener_force = 0.2 * CurVel[1]
    force_fender_y = stiffener_force - dampener_force
    force_fender = [0, -force_fender_y, 0]

    ExtraForce = Analysis.ApplyForceOnStructureAtPoint(Struct=0,
                                                       FX=force_fender[0],
                                                       FY=force_fender[1],
                                                       FZ=force_fender[2],
                                                       AppPtX=CurPos[0],
                                                       AppPtY=CurPos[1],
                                                       AppPtZ=CurPos[2])

    # Add the extra force generated to initially empty force 
    # (BlankForce and BlankAddedMass classes support algebraic operations (+, -, and scalar multiplication)
    Force += ExtraForce

    result_array = [TimeStep, CurPos[1], delta_L, stiffener_force, CurVel[1], dampener_force, force_fender[1]] 
    Analysis.result_df.loc[len(Analysis.result_df)] = result_array 

    # Now return the results

    return Force,AddMass,Error


########################################################################################################
# 
#  Finally instanciating the server and asking it to run each of our functions.
#  This is going to start the server which is going to wait for an incoming connection
#  from the Aqwa program. It will then use the first function UF1 for that particular Aqwa run.
#  The process will then repeat for two more Aqwa runs, using UF2 and UF3.
#  

Server = AqwaUserForceServer()

for UF in [UFFender] * 1:
    try:
        print("Now running user function {0}".format(UF.__name__))
        Server.Run(UF)
    except Exception as E: # If an error occurred, we print it but continue
        print("Caught error : ",E)
        print("Skipping to next case")
