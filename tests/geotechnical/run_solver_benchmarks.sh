#!/usr/bin/env bash
# Run the solver-enabled collection separately from the ordinary specialized gate.
# Each xslope circular search runs in one process for about a minute, so the
# tests are spread over the runner's cores (-n auto) instead of run one by one.
# The default "load" schedule hands each worker a long run of consecutive tests,
# which can put every slow test in test_slope_stability.py on one worker;
# worksteal lets idle workers take queued tests from a busy one.
PYTHONPATH=src exec uv run --no-sources --with-editable '.[test]' \
  --with 'xslope==1.0.3' --with 'gmsh>=4.15' \
  python -m pytest tests/geotechnical -n auto --dist worksteal -q -rA \
  -p no:cacheprovider "$@"
