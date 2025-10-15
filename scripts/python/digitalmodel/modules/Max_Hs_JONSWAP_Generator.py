# -*- coding: utf-8 -*-

import OrcFxAPI
import math
import numpy
import os.path
import glob
import pandas
from xlsxwriter.utility import xl_col_to_name
import xlsxwriter
import time
from pathlib import Path
# =============================================================================
# do not change below source for the function codes
# =============================================================================
import sys
pythonfilepath='M:/Projects/Active/1607508 - Balder Future Project - Detail Design\Analysis\FlexibleFlowlines/Tie-in/python files'
sys.path.append(pythonfilepath)
import geocoord
import position_alongline_locator
sys.path.append('N:/MM/python files')
import density
start_time=time.process_time()
x=0
y=0
def multiple(multiplicator,start,end):
    a=[]
    for i in range(start,end+1):
        a.append(i*multiplicator)
    return a
# =============================================================================
# mainpath of the project folder containing the base model
# =============================================================================
mainpath='M:/Projects/Active/1601173 - AKOFS Seafarer Nitrogen Transfer/Analysis/Orcaflex'
# =============================================================================
# basemodel filename
# =============================================================================
model_filename='Base_*.dat'
folder=[
        # '22v/75/195',
        # '22v/75/135',
        # '23v/test'
        # '23v/75/135',
        # '23v/75/150',
        # '23v/75/165',
        # '23v/75/180',
        # '24v/75/150',
        # '24v/75/165',
        # '24v/75/180',
        # '23v/test'
        ]
base_files=glob.glob(os.path.join(mainpath,folder[0],model_filename))


# =============================================================================
# =============================================================================
# =============================================================================
# # Hs and Tp list
# =============================================================================
# Hs=[1.5]###list(numpy.arange(1.5,5,0.5))
# Tp=[5]###[5, 7, 9, 10, 11, 12, 13, 14, 15, 17, 19, 20]
# W_dir=list(range(270-30,270+31,15))##[270-30,270,270+30]#list(range(0,181,30))
W_dir=list(range(255-30,255+31,30))
# W_dir=[0]
wave_matrix=[
    # [2,[10]],
    # [4.0,[6]],
#     Hs=[3.0,3.5]
# # Hs=[4.0,4.5]
# Tp=[6,16,17,18]
                  # [3.0,[6,16,17,18]],
                  # [3.5,[6,16,17,18]],
                  # [4.0,[6,16,17,18]],
                  # [4.5,[6,16,17,18]],
                # [1.5,list(range(4,18,1))],
                # [2.0,list(range(4,19,1))],
                # [2.5,list(range(5,14,1))],
                [3.0,list(range(7,16,1))],
                [3.5,list(range(7,16,1))],
                # [3.5,[7,8,13]],
                # [4,[8]]
                # [3.5,[8]]
                # [4,[7,8,9,10,11]]
                # [3.5,[8]],
                [4.0,list(range(7,16,1))],
                # # [4.5,list(range(7,19,1))],
                # [3.5,[14]],
                # [4.5,[10,14]],
                # 
            ]
current_speed=0.25
number_of_files=0
created_files=[]
for i2,base_file in enumerate(base_files):
    model=OrcFxAPI.Model()
    model.LoadData(base_file)
    hose=model['HOSE']
    seim_pride=model['Siem Pride']
    seim_pride_heading=seim_pride.InitialHeading
    basefile_name=os.path.splitext(base_file.replace('Base_',''))[0]
    old_comment=model.objects[0].Comments
    old_comment=old_comment.replace('\n','    ')
    old_comment=old_comment.replace('     ','    ')
    for i5,wdir in enumerate(W_dir):
        # model.environment.WaveHeight=hs
        # model.environment.WaveType='Stokes\' 5th'
        # if wdir>=seim_pride_heading+180-30 and wdir<=seim_pride_heading+180+30:
        if seim_pride_heading>=45 and seim_pride_heading<=90:
        # if (wdir==240 and seim_pride_heading==105) or (wdir==285 and seim_pride_heading==60) or (wdir==300 and seim_pride_heading==60) or (wdir==300 and seim_pride_heading==75):
            for i3,[hs,Tp] in enumerate(wave_matrix):
                # model.environment.WavePeriod=tp
                for i4, tp in enumerate(Tp):
                    # model.environment.WaveDirection=wdir
                    filnavn=os.path.join(basefile_name+'_Hs{hs:s}_Tp{tp:s}_Wdir{wdir:s}.yml'.format(hs=str(hs),tp=str(tp),wdir=str(wdir)))          
                    with open(filnavn, 'w') as f:
                        f.write('BaseFile: {:s}\n'.format(base_file))
                        # # f.write('RestartingFrom: %s\n' % BaseFile)
                        f.write('General: \n')
                        f.write('  Comments: !\n    {old_comment:s}\r    Wave height {hs:.2f}m\r    Wave direction {wdir:.2f}deg\r    Wave timeperiod {tp:.2f}s\r    Current speed{current_speed:.2f}m/s\n'.format(current_speed=current_speed,old_comment=old_comment,hs=hs,tp=tp,wdir=wdir))
                        # f.write('  StageCount: 1\n')
                        # f.write('  StageDuration[1]: 200\n')
                        f.write('Environment:\n')
                        f.write('  WaveTrains:\n')
                        f.write('    Wave1:\n')
                        f.write('      WaveType: Dean stream\n')
                        f.write('      WaveDirection: {:d}\n'.format(wdir))
                        f.write('      WaveHeight: {:.2f}\n'.format(hs*1.86))
                        f.write('      WavePeriod: {:.2f}\n'.format(tp*0.9))
                        f.write('      WaveStreamFunctionOrder: 5\n')
                        f.write('  MultipleCurrentDataCanBeDefined: No\n')
                        f.write('  CurrentRamped: Yes\n')
                        f.write('  CurrentMethod: Interpolated\n')
                        f.write('  RefCurrentSpeed: {:.2f}\n'.format(current_speed))
                        f.write('  RefCurrentDirection: {:d}\n'.format(wdir))
                        # f.write('Shapes:\n')
                        # f.write('  Siem Pride Chute -bottom curved plate:\n')
                        # f.write('    OriginX: -1.5\n')
                        # f.write('    ProfileDistanceAlongAxis[2]: 3\n')
                        # f.write('  Siem Pride Chute flat bottom:\n')
                        # f.write('    OriginX: -1.5\n')
                        # f.write('    SizeX: 3\n')
                        # f.write('  AKOFS Seafarer Chute-bottom flate plate:\n')
                        # f.write('    OriginX: 1.25\n')
                        # f.write('    SizeX: 2.5\n')
                        # f.write('  AKOFS Seafarer Chute-right side edge wing plate:\n')
                        # f.write('    OriginY: -0.624\n')
                        # f.write('    ProfileDistanceAlongAxis[2]: 2.5\n')
                        # f.write('  AKOFS Seafarer Chute-left side edge wing plate:\n')               
                        # f.write('    OriginY: -0.624\n')
                        # f.write('    ProfileDistanceAlongAxis[2]: 2.5\n')
                        f.close()
                    print(filnavn)
                    created_files.append(filnavn)
                    # model.SaveData(filnavn)   
                    number_of_files+=1
    # # base_file=base_files[0]
    # # basefile_name=os.path.splitext(base_file.replace('Base_withoutsupp_',''))[0]
    # for i3,[hs,Tp] in enumerate(wave_matrix):
    #     # model.environment.WaveHeight=hs
    #     # model.environment.WaveType='Stokes\' 5th'
    #     for i4, tp in enumerate(Tp):
    #         # model.environment.WavePeriod=tp
    #         for i5,wdir in enumerate(W_dir):
    #             # model.environment.WaveDirection=wdir
    #             filnavn=os.path.join(mainpath,folder[0],'Hs{hs:s}_Tp{tp:s}_Wdir{wdir:s}.txt'.format(hs=str(hs),tp=str(tp),wdir=str(wdir)))          
    #             with open(filnavn, 'w') as f:
    #                 # f.write('BaseFile: {:s}\n'.format(base_file))
    #                 # # f.write('RestartingFrom: %s\n' % BaseFile)
    #                 # f.write('General: \n')
    #                 # f.write('  Comments: ! "{old_comment:s}\r Wave height{hs:.2f}\r Wave direction{wdir:.2f}\r Wave timeperiod{tp:.2f}\r"'.format(old_comment=old_comment,hs=hs,tp=tp,wdir=wdir))
    #                 # f.write('  StageCount: 1\n')
    #                 # f.write('  StageDuration[1]: 200\n')
    #                 # f.write('Environment:\n')
    #                 # f.write('  WaveTrains:\n')
    #                 # f.write('    Wave1:\n')
    #                 # f.write('      WaveType: Stokes\' 5th\n')
    #                 # f.write('      WaveDirection: {:d}\n'.format(wdir))
    #                 # f.write('      WaveHeight: {:.2f}\n'.format(hs))
    #                 # f.write('      WavePeriod: {:.2f}f\n'.format(tp))
    #                 f.write('test')
    #                 f.close()
    #             print(filnavn)
                  
    #             number_of_files+=1

end_time=time.process_time()
print('Time taken :{:.0f} minutes and {:.0f} seconds'.format((-start_time+end_time)//60,(-start_time+end_time)%60))

with open(os.path.join(mainpath,folder[0],'runlist_newlycreated.lst'),mode='w') as f:
    for i in created_files:
        if 'Hs3.5_Tp7' in i :
            print(i)
        elif 'Hs4.0_Tp7' in i:
            print(i)
        elif 'Wdir285' in i and 'svh45.0' not in i:
            print(i)
        else:
            f.write(i+'\n')
    f.close()
