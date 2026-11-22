---
purpose: Engineering digital twin — asset integrity, cathodic protection, GIS, hydrodynamics, field development
entry_points: [src/digitalmodel/engine.py, src/digitalmodel/asset_integrity/, src/digitalmodel/cathodic_protection/]
test_command: uv run --no-project --python 3.11 --no-sources --with-editable '.[test]' python -m pytest
depends_on: [assetutilities]
maturity: beta
---
# digitalmodel

Contract: ../workspace-hub/AGENTS.md | Source: src/digitalmodel/
Key modules: engine.py, asset_integrity/, cathodic_protection/, gis/, hydrodynamics/, field_development/
Routing: docs/README.md | docs/maps/digitalmodel-operator-map.md | docs/registry/module-routing.yaml
