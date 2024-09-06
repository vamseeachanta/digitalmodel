# Standard library imports
import logging
import os

# Third party imports
import pandas as pd
from assetutilities.common.data_exploration import DataExploration
from assetutilities.common.file_management import FileManagement

# Reader imports
from digitalmodel.custom.aqwa.aqwa_utilities import AqwaUtilities
from digitalmodel.custom.aqwa.ef_server.AqwaServerMgr import *

fm = FileManagement()
au = AqwaUtilities()
de = DataExploration()

class AqwaEFServer: 

    def __init__(self) -> None:
        pass

    def ef_server_router(self, cfg: dict) -> None:

        ########################################################################################################
        # 
        #  Finally instanciating the server and asking it to run each of our functions.
        #  This is going to start the server which is going to wait for an incoming connection
        #  from the Aqwa program. It will then use the first function UF1 for that particular Aqwa run.
        #  The process will then repeat for two more Aqwa runs, using UF2 and UF3.
        #
        cfg = fm.router(cfg)
        self.cfg = cfg

        number_of_client_runs = cfg['analysis_settings']['ef_input']['number_of_runs']
        if number_of_client_runs is None:
            number_of_client_runs = len(self.cfg['file_management']['input_files']['DAT'])

        if number_of_client_runs == 0:
            logging.info("No AQWA client runs specified. Exiting...")
            raise ValueError("No AQWA client runs specified. Exiting...")

        aqwa_client_directory = cfg['Analysis']['file_management_input_directory']
        Server = AqwaUserForceServer(port=0, dir_path=aqwa_client_directory)

        run_function_name = cfg['analysis_settings']['ef_input']['run_function']
        run_function = getattr(self, run_function_name)

        closing_function = cfg['analysis_settings']['ef_input']['closing_function']
        if closing_function is not None:
            closing_function = getattr(self,cfg['analysis_settings']['ef_input']['closing_function'])

        for run_idx in range(0, number_of_client_runs):
            try:
                print("Now running user function {0}".format(run_function_name))
                InputFileName = self.cfg['file_management']['input_files']['DAT'][run_idx]
                au.run_aqwa_analysis_as_subprocess(cfg, InputFileName, process='detached')
                self.define_result_df(cfg)
                Server.Run(run_function, closing_function)
            except Exception as E: # If an error occurred, we print it but continue
                print("Caught error : ",E)
                print("Skipping to next case")

            print("Now saving result for: {0}".format(InputFileName))
            self.save_result_df(cfg, InputFileName)

    def define_result_df(self, cfg):
        columns = cfg['analysis_settings']['ef_input']['columns']
        
        def_pos_cfg = cfg['analysis_settings']['ef_input']['def_pos']
        self.result_df_array = []
        for idx in range(0, len(def_pos_cfg)):
            self.result_df_array.append(pd.DataFrame(columns=columns))

    def save_result_df(self, cfg, InputFileName):
        basename = os.path.basename(InputFileName)
        output_file_basename = basename.split('.')[0]
        def_pos_cfg = cfg['analysis_settings']['ef_input']['def_pos']
        for def_pos_idx in range(0, len(def_pos_cfg)):
            df = self.result_df_array[def_pos_idx]
            
            aqwa_client_directory = cfg['Analysis']['result_folder']
            filename = output_file_basename + '_' +str(def_pos_idx) + '_py_inputs.csv'
            filename_path = os.path.join(aqwa_client_directory , filename)
            df.to_csv(filename_path , index=False)

            # filename = output_file_basename + '_' +str(def_pos_idx) + '_py_inputs_statistics.csv'
            # filename_path = os.path.join(aqwa_client_directory , filename)
            # self.save_statistics(df, filename_path)

    def save_statistics(self, df, statistics_filename):
        df_statistics = de.get_df_statistics(df)
        df_statistics.to_csv(statistics_filename, index=True, header=True)

    def UF1(self, Analysis,Mode,Stage,Time,TimeStep,Pos,Vel):
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

    def UF2(self, Analysis,Mode,Stage,Time,TimeStep,Pos,Vel):
        ########################################################################################################
        # Example applying a vertical Force away from the centre of gravity
        # 
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

    def UF3(self, Analysis,Mode,Stage,Time,TimeStep,Pos,Vel):
        ########################################################################################################
        # A third simple example
        # AddMass is kept zeroed
        # Force is acting only on X,Y.
        # It is still proportional to X,Y pos
        # But rotated. The structure should go around in circles
        AddMass = BlankAddedMass(Analysis.NOfStruct)
        Force   = BlankForce(Analysis.NOfStruct)
        Error   = 0

        # User defined code here
        for s in range(Analysis.NOfStruct):
            Force[s][0] = 10000.0*Pos[s][1]
            Force[s][1] = -10000.0*Pos[s][0]

        # Now return the results

        return Force,AddMass,Error


    def UFFender(self, Analysis,Mode,Stage,Time,TimeStep,Pos,Vel):
        ########################################################################################################
        # A Custom Function for Fender
        AddMass = BlankAddedMass(Analysis.NOfStruct)
        Force   = BlankForce(Analysis.NOfStruct)
        Error   = 0

        # Fender Definition
        def_pos_cfg = self.cfg['analysis_settings']['ef_input']['def_pos']
        for def_pos_idx in range(0, len(def_pos_cfg)):
            def_pos = def_pos_cfg[def_pos_idx]
            Struct = def_pos['Struct']
            DefPos = def_pos['DefPos']
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

            delta_L = fender_node_change 
            if delta_L  > 0:
                stiffener_force = - (50000 * delta_L+ 3000*delta_L**2)
                dampener_force = -0.2 * CurVel[1]

                ramp_cfg = self.cfg['analysis_settings']['ef_input']['load_ramp']
                if ramp_cfg['flag'] and Time < ramp_cfg['time']:
                    force_dict= {'stiffener_force': stiffener_force, 'dampener_force': dampener_force}
                    ramp_force_dict= self.get_ramp_force(ramp_cfg, Time, force_dict)

                    stiffener_force = ramp_force_dict['stiffener_force']
                    dampener_force = ramp_force_dict['dampener_force']

                force_fender_y = stiffener_force + dampener_force
                force_fender = [0, force_fender_y, 0]

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
            else:
                force_fender = [0, 0, 0]
                dampener_force = 0
                stiffener_force = 0

            result_array = [TimeStep, CurPos[1], delta_L, -stiffener_force, CurVel[1], -dampener_force, -force_fender[1]] 
            self.result_df_array[def_pos_idx].loc[len(self.result_df_array[def_pos_idx])] = result_array 

        return Force,AddMass,Error

    def get_ramp_force(self, ramp_cfg, Time, force_dict):

        ramp_factor = self.get_ramp_factor(ramp_cfg, Time)
        ramp_force_dict = force_dict.copy()
        for key in force_dict:
            ramp_force_dict[key] = force_dict[key] * ramp_factor

        return ramp_force_dict

    def get_ramp_factor(self, ramp_cfg, Time):
        t_0 = 0
        f_0 = ramp_cfg['initial_factor']
        t_f = ramp_cfg['time']
        f_f= 1
        
        ramp_factor = (Time - t_0) * (f_f - f_0) / (t_f - t_0) + f_0

        return ramp_factor

    def uf_wlng_fst(self, Analysis,Mode,Stage,Time,TimeStep,Pos,Vel):
        AddMass = BlankAddedMass(Analysis.NOfStruct)
        Force   = BlankForce(Analysis.NOfStruct)
        Error   = 0

        self.print_heartbeat(Time)

        def_pos_cfg = self.cfg['analysis_settings']['ef_input']['def_pos']
        for def_pos_idx in range(0, len(def_pos_cfg)):
            def_pos = def_pos_cfg[def_pos_idx]
            result_dict = self.wsp_dampener_by_def_pos(Analysis, Time, def_pos)

            Force = Force  + result_dict['Force']
            AddMass = AddMass + result_dict['AddMass']
            Error = result_dict['Error']
            self.assign_results_to_df(def_pos_idx, Time, result_dict)

        return Force, AddMass, Error

    def assign_results_to_df(self, def_pos_idx, Time, result_dict):
        # columns = ['Time', 'dl_x', 'dl_y', 'dl_z','velocity_x', 'velocity_y', 'stiffener_force_x', 'dampener_force_x', 'total_force_x', 'stiffener_force_y', 'dampener_force_y', 'total_force_y']
        dl = result_dict['dl']
        CurNodeVel = result_dict['CurNodeVel']
        force_x = result_dict['force_x']
        force_y = result_dict['force_y']
        result_array = [Time, dl[0], dl[1], dl[2], CurNodeVel[0], CurNodeVel[1], CurNodeVel[2], force_x['stiffness'], force_x['dampener'] , force_x['total'], force_y['stiffness'], force_y['dampener'], force_y['total']]
        result_array = [round(item, 4) for item in result_array]

        result_array_idx = len(self.result_df_array[def_pos_idx])

        add_to_df_flag = True
        if result_array_idx > 0:
            last_time_in_df = self.result_df_array[def_pos_idx].iloc[-1]['time']
            if round(float(Time), 3) == round(float(last_time_in_df), 3):
                add_to_df_flag = False

        if add_to_df_flag:
            self.result_df_array[def_pos_idx].loc[result_array_idx] = result_array

    def wsp_dampener_by_def_pos(self, Analysis, Time, def_pos):
        AddMass = BlankAddedMass(Analysis.NOfStruct)
        Force   = BlankForce(Analysis.NOfStruct)
        Error   = 0

        Struct = def_pos['Struct']
        DefPos = def_pos['DefPos']
        dl_correction = def_pos['dl_correction']
        CurPos = Analysis.GetNodeCurrentPosition(Struct=Struct ,
                                                DefAxesX=DefPos[0],
                                                DefAxesY=DefPos[1],
                                                DefAxesZ=DefPos[2])

        #TODO Should we give DefPos or CurPos to GetNodeCurrentVelocity?
        CurNodeVel = Analysis.GetNodeCurrentVelocity(Struct=Struct ,    
                                                DefAxesX=CurPos[0],
                                                DefAxesY=CurPos[1],
                                                DefAxesZ=CurPos[2])

        dl = [CurPos[0] - DefPos[0] -dl_correction[0], CurPos[1] - DefPos[1]-dl_correction[1], CurPos[2] - DefPos[2]-dl_correction[2]]

        force_x = self.get_wsp_dampener_force(Time, dl[0], CurNodeVel[0], dir='X')
        force_y = self.get_wsp_dampener_force(Time, dl[1], CurNodeVel[1], dir='Y')

        force_vector = [force_x['total'], force_y['total'], 0]

        ExtraForce = Analysis.ApplyForceOnStructureAtPoint(Struct=Struct,
                                                        FX=force_vector[0],
                                                        FY=force_vector[1],
                                                        FZ=force_vector[2],
                                                        AppPtX=CurPos[0],
                                                        AppPtY=CurPos[1],
                                                        AppPtZ=CurPos[2])

        # Add the extra force generated to initially empty force 
        # (BlankForce and BlankAddedMass classes support algebraic operations (+, -, and scalar multiplication)
        Force += ExtraForce

        result_dict = {'Force': Force, 'AddMass': AddMass, 'Error': Error, 'dl': dl, 'CurNodeVel': CurNodeVel, 'force_x': force_x, 'force_y': force_y}
        # Now return the results
        return result_dict

    def get_wsp_dampener_force(self, Time, dof_pos_delta, dof_vel, dir='X'):
        stiffness = {'X': {'dl': [0.15, 0.70, 10], 'k': [1, 7.03e6, 2.8e7]}, 
                    'Y': {'dl': [0.15, 0.70, 10], 'k': [1, 14.06e6, 5.6e7]}}
        c = {'X': 3.75E+06, 'Y': 7.51E+06}

        stiffness_force = 0
        dof_pos_delta_abs = abs(dof_pos_delta)
        if dof_pos_delta_abs < stiffness[dir]['dl'][0]:
            stiffness_force = stiffness[dir]['k'][0] * dof_pos_delta_abs
        elif dof_pos_delta_abs < stiffness[dir]['dl'][1]:
            stiffness_force = stiffness[dir]['k'][0] * stiffness[dir]['dl'][0]
            stiffness_force = stiffness_force + stiffness[dir]['k'][1] * (dof_pos_delta_abs - stiffness[dir]['dl'][0])
        else:
            stiffness_force = stiffness[dir]['k'][0] * stiffness[dir]['dl'][0]
            stiffness_force = stiffness_force + stiffness[dir]['k'][1] * (stiffness[dir]['dl'][1] - stiffness[dir]['dl'][0])
            stiffness_force = stiffness_force + stiffness[dir]['k'][2] * (dof_pos_delta_abs - stiffness[dir]['dl'][1])

        if dof_pos_delta > 0:
            stiffness_force = -stiffness_force

        dampener_force = c[dir] * (abs(dof_vel)**0.6)
        if dof_vel > 0:
            dampener_force = -dampener_force

        ramp_cfg = self.cfg['analysis_settings']['ef_input']['load_ramp']
        if ramp_cfg['flag'] and Time < ramp_cfg['time']:
            force_dict= {'stiffness_force': stiffness_force, 'dampener_force': dampener_force}
            ramp_force_dict= self.get_ramp_force(ramp_cfg, Time, force_dict)

            stiffness_force = ramp_force_dict['stiffness_force']
            dampener_force = ramp_force_dict['dampener_force']

        total_force = stiffness_force + dampener_force
        force = {'stiffness': stiffness_force, 'dampener': dampener_force, 'total': total_force}

        return force


    def print_heartbeat(self, Time):
        if Time % 50 == 0:
            # if self.heartbeat_count == 1:
            logging.info(f"analysis Time: {Time} s")
            #     # self.heartbeat_count = 0
            # else:
            #     self.heartbeat_count += 1