# Mooring-chain degradation analysis example

This synthetic example runs the existing dimensional screen and retains its
`ESCALATE` verdict. A local `WITHIN_LIMITS` result does not establish chain
strength, fatigue life or continued-service fitness. The registered version 1
workflow is experimental and is excluded from latest-stable resolution.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-11-16 | Add a runnable synthetic inspection with explicit output destinations. |

## Run

From the repository root with the package dependencies available:

```sh
uv run python -m digitalmodel.asset_integrity.mooring_chain_cli examples/workflows/mooring-chain-degradation-screen/input.yml --output examples/workflows/mooring-chain-degradation-screen/results/screen.json
```

The command writes UTF-8 `screen.json` and adjacent `screen.html`. JSON retains
the inspection, calculated result and the same HTML; storage paths are excluded.
The output argument is required and both destinations must be new. Input/output
collisions and existing files are refused. Choose another output name to repeat
the run. Writes are not transactional; an I/O failure may leave partial outputs.

## Input basis and interpretation

Diameters and absolute measurement uncertainty are in mm. Diameter-loss limits
and minimum circular area ratios are dimensionless fractions. Operator limits,
their provenance reference and uncertainty are required without defaults. The
supplied limits are synthetic arithmetic inputs with no design authority; real
use requires applicable operator criteria and their documented provenance.

The example matches the YAML in the existing
[method document](../../../docs/domains/asset-integrity/mooring-chain-degradation-screen.md).
For its synthetic readings the local result is `WITHIN_LIMITS`. Overall FFS
remains `ESCALATE`; remaining MBL and fatigue life are JSON null. Pitting,
interlink wear, ovalization and capacity/fatigue qualification remain explicit
evidence gaps. No qualified degraded-link capacity or published break-test
benchmark is added. Existing method citations are references, not newly checked
standards text. No solver is required. Generated results are runtime outputs.
