Usage:

If you wish to disable current and wind from part of the simulation then you could achieve this by using Restarts to split the model into several simulation files, and have current and wind acting for the parts you require. 


https://www.orcina.com/webhelp/OrcaFlex/Default_Left.htm#StartTopic=html/Restartanalyses.htm|SkinName=Web%20Help

Detailed Method:

- Parent file should have a time recorded for a time.
- Child file can therefore use that time
- If not time recorded for parent file. then the restart file with start from end of parent file.


Scratch

Winches:
  Line08:
      StageMode, StageValue:
        - [Specified payout, 0]
        - [Specified tension, 25]
        - [Specified payout, 0]
  
  Line10:
      StageMode, StageValue:
        - [Specified payout, 0]
        - [Specified tension, 30]
        - [Specified payout, 0]
  # includefile: ../../source/winches/iteration_01.yml




[Window Title]
OrcaFlex

[Main Instruction]
Error

[Content]

• 'fst1_f_fst2_f_lngc_ec125km3_l_pb_hwl_iteration_02.yml' (Error reading K:\github\digitalmodel\docs\pkg_orcaflex\notes\pre_restarts\mooring\fsts_lngc\01_qa\fst1_f_fst2_f_lngc_ec125km3_l_pb_hwl_iteration_02.yml: Failed to set StageDataStageCount=1 (Change not allowed). Last parsed name: 'StageMode, StageValue' near line 5 Last parsed name: 'includefile', value: includefile_fst1_f_fst2_f_lngc_ec125km3_l_pb_hwl_iteration_02.yml near line 8)


[OK]


[Window Title]
OrcaFlex

[Main Instruction]
Error

[Content]
Cannot perform restart because the parent input data file's last modified date is later than the parent simulation file's last modified date.

[OK]


[Window Title]
OrcaFlex

[Main Instruction]
Error

[Content]
Error reading K:\github\digitalmodel\docs\pkg_orcaflex\notes\pre_restarts\mooring\fsts_lngc\01_qa\fst1_f_fst2_f_lngc_ec125km3_l_pb_hwl_iteration_01.yml: Failed to set StageMode[1]=Specified payout ('Specified payout' not found. Possible values are: Specified length and Specified tension).

Last parsed name: 'StageMode, StageValue' near line 16

[OK]