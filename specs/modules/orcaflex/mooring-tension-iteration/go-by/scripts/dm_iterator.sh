# Iteration 1
# Tension calculation
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml
# batch run all yml files and save sim files to same directory
/d/github/digitalmodel/.venv/Scripts/python run_models_to_sim.py dat=true input_directory="../.dat/" output_directory="../.sim/"
# /d/github/digitalmodel/.venv/Scripts/python -m digitalmodel.modules.orcaflex.universal pattern="fsts*180km3*pb_*.yml" input_directory="." output_directory="." validate=false

# postprocess to get results
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel dm_ofx_post_fsts_lngc.yml --workers 30
/d/github/assetutilities/.venv/Scripts/python -m assetutilities au_collate.yml
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel viz.    yml "{'meta': {'label': 'viz_fsts_180km3_sb'}, 'file_management': {'input_directory': '../.sim/', 'output_directory': '../output/visual', 'filename': {'pattern': 'fsts_*_vessel_statics_6dof'}}}" --workers 30

