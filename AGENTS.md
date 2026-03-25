---
purpose: Engineering digital twin — asset integrity, cathodic protection, GIS, hydrodynamics, field development
entry_points: [src/digitalmodel/engine.py, src/digitalmodel/asset_integrity/, src/digitalmodel/cathodic_protection/]
test_command: PYTHONPATH=src uv run python -m pytest
depends_on: [assetutilities]
maturity: beta
---
# digitalmodel

Contract: ../AGENTS.md | Source: src/digitalmodel/
Key modules: engine.py, asset_integrity/, cathodic_protection/, gis/, hydrodynamics/, field_development/
