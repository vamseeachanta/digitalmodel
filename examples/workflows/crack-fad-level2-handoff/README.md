# Plate crack assessment and specialist handoff

The workflow computes a plate surface-flaw point against **BS 7910:2013
Option 1**, a bounded depth search at fixed half-length, and the existing
Level 3 specialist handoff. Every case is escalated; an inside-envelope point
alone does not qualify continued service.

Run from the repository root with the project's runtime dependencies installed:

```sh
PYTHONPATH=src uv run python -m digitalmodel examples/workflows/crack-fad-level2-handoff/input.yml
```

The example writes `results/crack-fad-report.html` and
`results/level3-handoff.json` beside its input. The engine may also write its
normal configuration echo. Set `crack_assessment.output_dir` to direct report
and handoff outputs elsewhere; relative output directories resolve beside the
input file. No external data or solver is needed.

The synthetic inputs are a 4.0 mm deep, 10.0 mm total-length surface flaw in a
20.0 mm plate, with a 50.0 mm **half-width**. The membrane stress is 100.0 MPa;
bending stress is zero. Yield, tensile strength, elastic modulus and toughness
are explicit numerical inputs, with no material grade implied. Their values
are chosen for the hand calculation in
[`test_crack_fad_workflow.py`](../../../tests/asset_integrity/test_crack_fad_workflow.py).
The depth cap and tolerance are numerical search settings, not standard limits.

The point gives Lr = 0.260 and Kr = 0.085, against f(Lr) = 0.983 and
Lr_max = 1.250. The point is inside the stated envelope. The fixed-length depth
search extends above the assessed 4.0 mm depth to its geometry cap near 5.0 mm;
this is **not a failure boundary**.
The disposition remains `ESCALATE` and the handoff includes the input basis,
computed point, search result, triggers, recommended analyses and open questions.

The [validation record](../../../docs/domains/asset-integrity/crack-fad-handoff-validation.html)
documents the method and independent arithmetic. Published Annex-example
qualification is `NOT_EVALUATED`. Residual stress, secondary stress, intermediate
crack-front maxima, fatigue growth, temperature-dependent toughness, API 579
safety factors and service acceptance are not evaluated. Cylinder and embedded
flaws are rejected; measured toughness is required and no Charpy fallback is used.
