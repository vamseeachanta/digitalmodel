# HIPPS closure-kinematics worked example

This builder covers a bounded synthetic closure example for issue #3340. The
parent module-report issue remains open. No full-module qualification is claimed.
Draft for maintainer review.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-25 | Added the bounded example and its rebuild command so the arithmetic can be inspected. |
| 2 | 2026-10-25 | Corrected the input provenance to identify author-selected assumptions and added the kinematic method source. |

## Rebuild

From the repository root, with `TMPDIR` set to a writable temporary directory:

```sh
PYTHONPATH=src uv run --no-project --with pytest python docs/reports/modules/hipps/build_worked_example.py --output-dir "$TMPDIR/worked-example"
```

The selected directory receives only `closure_kinematics.html`. The page has
embedded styles and needs no external input data, licensed software, network
access or large computation. Python and the source checkout are sufficient;
`uv` may need network access to prepare its Python environment on the first run.
Importing the builder produces no report. Existing report symlinks are rejected.
Generated HTML is a rebuild artifact and is not committed or added to an index.

## Arithmetic basis and units

The travel, speed and sample times are author-selected illustrative assumptions,
not supplied by issue #3340 and not measured equipment data. Maintainer acceptance
of this basis is required before merge. Full actuator travel is 0.6 m and constant
loaded speed is 0.2 m/s. Constant-speed kinematics gives
`T = L / v = (0.6 m) / (0.2 m/s) = 3 s`.

Method source: OpenStax, *University Physics Volume 1*, 2016 edition,
section 3.4, [Motion with Constant Acceleration](https://openstax.org/books/university-physics-volume-1/pages/3-4-motion-with-constant-acceleration),
Equation 3.10 with constant velocity. This is not a source of the selected
numeric inputs or the separately assumed remaining-conductance law.

Assume a linear remaining-conductance law
`g(t) = 1 - clip(t / T, 0, 1)`. The bounds zero and one define normalization.
At elapsed times `(0, 1.5, 3, 4) s`, the completed travel fractions are
`(0, 1/2, 1, 1)`, so the remaining idealized conductance fractions are
`(1, 1/2, 0, 0)`, dimensionless. Time starts with actuator motion.
The builder calls `stroke_time` and linear `closure_fraction` in
`src/digitalmodel/subsea/hipps/valve_sizing.py`.

Actuator travel is mechanical displacement. Idealized conductance is a separately
assumed normalized characteristic. Neither establishes actual flow or leakage.
Detection delay, SIL, pressure transients, capacity, equipment suitability,
standards compliance and actual flow remain `NOT_EVALUATED`. No licensed numeric
criteria are introduced. Further work requires qualified equipment data,
reliability evidence, detection and logic timing, transient analysis and
applicable acceptance criteria.

## Checks

```sh
PYTHONPATH=src uv run --no-project --with pytest python -m pytest tests/subsea/hipps -q -rA -p no:cacheprovider
```

The subprocess tests read rendered values and units against independent hand
calculations, require the limits and revision history, check import safety and
compare rebuild bytes both in place and across output directories. Full reports,
shared indexes and broader engineering qualification remain follow-up work.
