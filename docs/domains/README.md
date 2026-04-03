# Domain Documentation Layout

This repository stores domain documentation under `docs/domains/`.

## Current high-level layout

```
digitalmodel/
├── docs/domains/                  # Domain and solver documentation
│   ├── aqwa/                      # Solver: AQWA hydrodynamics
│   ├── orcaflex/                  # Solver: OrcaFlex marine dynamics
│   ├── orcawave/                  # Solver: OrcaWave diffraction
│   ├── mooring/                   # Mooring systems
│   ├── pipelines/                 # Pipeline engineering
│   ├── risers/                    # Riser systems
│   ├── structural/                # Structural analysis, FFS, materials
│   └── ...
├── src/digitalmodel/              # Actual Python source tree
│   ├── orcaflex/                  # Public OrcaFlex package
│   ├── orcawave/                  # Public OrcaWave package
│   ├── solvers/orcaflex/          # Deeper OrcaFlex solver implementation
│   ├── hydrodynamics/diffraction/ # DiffractionSpec and OrcaWave pipeline
│   └── ...
├── tests/                         # Actual test tree
│   ├── orcaflex/
│   ├── orcawave/
│   ├── solvers/orcaflex/
│   ├── hydrodynamics/diffraction/
│   └── ...
└── ../docs/maps/                  # Workspace-level operator maps and audits
```

## Important note

Some older documentation still refers to `src/modules/` and `tests/domains/`.
That is not the current layout for active OrcaWave/OrcaFlex work.
Use the actual source tree under `src/digitalmodel/` and tests under `tests/`.

## Start here for OrcaWave / OrcaFlex work

Use the canonical workspace-level operator map:
- `../docs/maps/digitalmodel-orcawave-orcaflex-operator-map.md`

That document links the main code surfaces, tests, issue clusters, queue scripts,
and machine boundaries for future work.
