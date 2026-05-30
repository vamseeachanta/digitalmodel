# Blender cube-render smoke manifest

> Issue: #2270  
> Validator: `scripts/blender/verify-blender-baseline.sh`

## Overview

`cube-render` is the minimum reproducible Blender headless validation case for workspace-hub engineering portability. It creates a default cube, area light, and camera, then renders a small PNG in headless mode.

## Prerequisites

- Blender available on `PATH` or through `BLENDER_BIN`.
- Canonical execution host for live evidence: dev-secondary.

## Commands

```bash
scripts/blender/verify-blender-baseline.sh \
  --verdict logs/engineering/blender-baseline/latest-verdict.yaml
```

Direct smoke invocation:

```bash
blender -b --python scripts/blender/smoke-render.py -- --output /tmp/blender-baseline-smoke/smoke-render.png
```

## Expected Outputs

- A YAML verdict at `logs/engineering/blender-baseline/latest-verdict.yaml`.
- A non-empty PNG at the runtime output path named by `render_output`.

## Failure Modes

- Missing Blender binary: rerun on dev-secondary or set `BLENDER_BIN`.
- Render process failure: inspect stderr for headless/GPU/runtime failures.
- Empty or missing PNG: treat as a failed baseline even if Blender exits zero.
