CALL ..\SET_Directories

REM ================
SET program=visualization
REM CALL ACTIVATE %program%

CD %git_root%
REM CALL PYTHON %program%.py

REM External files

REM SET file_name=visualization_0190_BM_timetrace_2019_24_05_2200H.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=visualization_0190_BM_timetrace_2019_24_10_0300H.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=visualization_0190_well_head_BOP_acc.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=visualization_0190_subplot_traces.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM 0190 Count Plots
REM SET file_name=visualization_0190_count_environment.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=visualization_0190_count_vessel.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=visualization_0190_count_BM.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=visualization_0190_count_BM.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM 0190 Extrema Plots
REM SET file_name=visualization_0190_wh_bm_stats.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=visualization_0190_riser_tensions_stats.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=visualization_0190_mud_weight_stats.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=visualization_0190_environment_stats.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=visualization_0190_riser_stroke_stats.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=visualization_0190_bop_heading_stats.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=visualization_0190_bop_acc.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=visualization_0190_pitch_stats.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=visualization_0190_roll_stats.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=visualization_0190_vm_position_stats.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=visualization_0190_vm_position_east_stats.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=visualization_0190_vm_position_north_stats.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=visualization_0190_wellhead_position.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=visualization_0190_wellhead_position_mean.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM Timetraces
REM SET file_name=visualization_0190_timetrace_2019_06_06_17H.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=visualization_0190_timetrace_2019_08_24_02H.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=visualization_0190_timetrace_2019_08_26_03H.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM Timetrace Data
REM SET file_name=visualization_0190_timetrace_2019_06_06_17H_data.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=visualization_0190_timetrace_2019_08_24_02H_data.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=visualization_0190_timetrace_2019_08_26_03H_data.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=visualization_0190_timetrace_2019_06_06_17H_acc.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=visualization_0190_bop_acc_stats.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=visualization_0190_lrj_ang_stats.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=visualization_0190_lmrp_ang_stats.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=visualization_0190_ang_stats_mean.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=visualization_0190_vm_stats.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=visualization_0190_v_pos_stats.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=visualization_bsee_stones_footprint.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=visualization_bsee_julia_footprint.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=visualization_bsee_cascadechinook_footprint.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

CD %working_directory%
SET working_directory%=
