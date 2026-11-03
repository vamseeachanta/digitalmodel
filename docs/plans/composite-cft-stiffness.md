# Composite CFT stiffness plan

## Plan-lite

- **Scope:** concrete-filled tubular (CFT) Case A in
  `scripts/integrations/sectionproperties_composite_poc.py` and its regressions.
  Expose the supplied geometry, materials, mesh settings and output directory;
  keep imports inert and distinguish EI from steel-transformed I. Cases B/C
  retain their existing analysis statements.
- **Independent basis:** integrate concentric circles for each material's area
  and second moment, then sum EA and EI by elastic modulus. Divide EI by the
  steel modulus for steel-transformed I. Integrate regular polygon triangles
  independently to distinguish discretization error from numerical roundoff.
- **Acceptance:** use two synthetic geometry/material sets, refine polygon
  sides from 32 to 64 and triangle areas from 16 to 4 square millimetres, check
  both bending axes and decreasing circle-integral error, and enforce relative
  deficit bounds of theta squared divided by 6 for EA and by 3 for transformed
  I, where theta = 2*pi/n. Check import inactivity and JSON output confinement.

Run the combined CFT and existing sectionproperties regressions from the
repository root, with temporary output directed to an existing scratch folder:

```sh
PYTHONPATH=src uv run --no-project --with pytest --with 'sectionproperties==3.10.2' python -m pytest tests/structural/test_composite_cft_stiffness.py tests/test_sectionproperties_integration.py -q -rA -p no:cacheprovider --basetemp="$TMPDIR/bt-cft"
```

The expected collection is 18 tests: 3 CFT regressions and 15 existing
integration tests. The comparison assumes perfect bond and linear elasticity.
Cases B/C, CFT warping, plastic analysis and stress-distribution validation are
outside this acceptance command's CFT coverage.
