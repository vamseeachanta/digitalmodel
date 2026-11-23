#! /bin/bash

# shell script to run python test scripts from
repo_name=$(basename $(git rev-parse --show-toplevel))
repo_root=$(git rev-parse --show-toplevel)
tests_home="tests/domains"
python_script=${tests_home}"/orcaflex/orcaflex_post_process/test_orcaflex.py"

# yaml_file=${tests_home}"/orcaflex/orcaflex_post_process/test_orcaflex_license.py"

cat << COM
Starting test. key details 
  - Repository name: $repo_name
  - Repository root: $repo_root
  - python script : $python_script
Executing scripts now 
COM

# get to repo root
cd "$repo_root"

# run module using  specific test yaml file
python ${python_script}