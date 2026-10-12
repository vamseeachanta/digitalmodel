# Riser-joint FFS demonstration

Run from the repository root in an environment with the project dependencies:

```bash
PYTHONPATH=src uv run python -c 'from digitalmodel.engine import engine; engine(inputfile="examples/workflows/riser-joint-ffs/input.yml")'
```

The registered offline workflow reads four existing anonymized C-scan grids in
place, plus the GML register and its provenance README. It returns start/end
acceptance envelopes, measured-minimum-wall collapse limits, four scan placement
assessments and a life-based roll-up of 26 unique Main joints. The two RJ-101
grids do not add an inventory joint. No licensed tables or private originals
are required at runtime.

Outputs are `results/riser-joint-ffs.html` and `results/riser-joint-ffs.json`.
The HTML is rendered through the FFSReport shell and includes source SHA-256
digests, calculation basis and limitations. The committed [report.html](report.html)
is a generated example readback; regenerate it by copying the output HTML after
the run. JSON paths describe the current run's output location.

[Validation record](../../../docs/domains/asset-integrity/riser-joint-ffs-validation-2026-10-10.md)
defines composition criteria and links the supporting engine anchors. This
demonstration does not establish independent qualification for fleet operation.
Nominal-wall envelopes do not assess C-scan pit morphology; register life is
not a newly calculated fatigue life. Unmeasured stations remain a coverage limit.
