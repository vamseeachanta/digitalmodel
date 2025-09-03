#!/bin/bash
# Tension calculation - Parallel Processing

# Method 1: Using GNU parallel (if available)
# Uncomment the following lines if GNU parallel is installed
# parallel -j 8 /d/github/digitalmodel/.venv/Scripts/python -m digitalmodel {} ::: \
#   config/dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml \
#   config/dm_ofx_anal_mooring_fsts_l015_125km3_sb.yml \
#   config/dm_ofx_anal_mooring_fsts_l015_180km3_pb.yml \
#   config/dm_ofx_anal_mooring_fsts_l015_180km3_sb.yml \
#   config/dm_ofx_anal_mooring_fsts_l095_125km3_pb.yml \
#   config/dm_ofx_anal_mooring_fsts_l095_125km3_sb.yml \
#   config/dm_ofx_anal_mooring_fsts_l095_180km3_pb.yml \
#   config/dm_ofx_anal_mooring_fsts_l095_180km3_sb.yml

# Method 2: Using background processes with wait
echo "Starting parallel tension calculations..."
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel config/dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml &
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel config/dm_ofx_anal_mooring_fsts_l015_125km3_sb.yml &
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel config/dm_ofx_anal_mooring_fsts_l015_180km3_pb.yml &
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel config/dm_ofx_anal_mooring_fsts_l015_180km3_sb.yml &
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel config/dm_ofx_anal_mooring_fsts_l095_125km3_pb.yml &
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel config/dm_ofx_anal_mooring_fsts_l095_125km3_sb.yml &
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel config/dm_ofx_anal_mooring_fsts_l095_180km3_pb.yml &
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel config/dm_ofx_anal_mooring_fsts_l095_180km3_sb.yml &

# Wait for all background processes to complete
wait
echo "All tension calculations completed."
