#!/usr/bin/env bash
# Run the solver-enabled collection separately from the ordinary specialized gate.
PYTHONPATH=src exec uv run --no-sources --with-editable '.[test]' \
  --with 'xslope==1.0.3' --with 'gmsh>=4.15' \
  python -m pytest tests/geotechnical -q -rA -p no:cacheprovider "$@"
