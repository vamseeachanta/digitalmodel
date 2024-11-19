#! /bin/bash

repo_name=$(basename $(git rev-parse --show-toplevel))
repo_root=$(git rev-parse --show-toplevel)
tests_home="tests/modules"
yaml_file=${tests_home}"/orcaflex/orcaflex_post_process/test_orcaflex_license.py"

cat << COM
Starting test. key details 
  - Repository name: $repo_name
  - Repository root: $repo_root
  - yaml file : $yaml_file
Executing scripts now 
COM

# get to repo root
cd "$repo_root"

# run module using  specific test yaml file
python -m ${repo_name} ${yaml_file}