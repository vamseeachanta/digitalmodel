# OpenFOAM cavity v2312 smoke manifest

## Overview

This manifest provides the reproducible instruction-only surface for the canonical `cavity`
smoke validation used by issue #2269.

## Prerequisites

- OpenFOAM ESI/OpenFOAM.com v2312 available on the execution host
- Source one of the supported bashrc paths:
  - `/usr/lib/openfoam/openfoam2312/etc/bashrc`
  - `/opt/openfoam2312/etc/bashrc`
- A writable `~/foam/run` directory on the execution host

## Commands

```bash
source /usr/lib/openfoam/openfoam2312/etc/bashrc || source /opt/openfoam2312/etc/bashrc
cp -r "$WM_PROJECT_DIR/tutorials/incompressible/icoFoam/cavity/cavity" ~/foam/run/cavity
cd ~/foam/run/cavity
blockMesh
icoFoam
```

## Expected Outputs

- `~/foam/run/cavity/log.blockMesh`
- `~/foam/run/cavity/log.icoFoam`
- time directories produced by the transient run
- normalized wrapper verdict at `logs/engineering/openfoam-baseline/latest-verdict.yaml` when invoked through `scripts/openfoam/verify-openfoam-baseline.sh`

## Failure Modes

- missing supported bashrc path should fail with explicit probe-order output
- version mismatch should fail if the runtime is not v2312 or `WM_PROJECT_DIR` does not resolve to `openfoam2312`
- copied case data must remain machine-local; this manifest must stay instruction-only and must not commit tutorial case trees into git
