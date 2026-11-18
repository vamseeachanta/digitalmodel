# CPT and CPTU interpretation analysis

## Summary

The CPT interpreter calculates area-corrected resistance, stress-normalised
soil behaviour bands and conditional strength or sand-state estimates. It
exports point observations to the existing geotechnical parameter database,
retaining input values, method citations, calibration inputs and unavailable
results. It does not infer layer boundaries or assign design properties.

The synthetic profile gives clay behaviour through 10 m and sand behaviour
below it. Drainage flags and quantitative drainage corrections remain
`NOT_EVALUATED` without readable, applicable calibration evidence. The way
forward is to qualify the stress profile, material assumptions, cone factor,
drainage bands and backbone fit before design use.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 0 | 2026-10-22 | Added CPT interpretation, provenance export and a synthetic example to expose methods and missing calibration. |

## Interface and units

Implementation: [cpt.py](../../../src/digitalmodel/geotechnical/cpt.py).
Evidence: [CPT tests](../../../tests/geotechnical/test_cpt.py).
Runnable example: [example.py](../../../examples/workflows/cpt-interpretation/example.py).

```python
from digitalmodel.geotechnical.cpt import CPTData, correct_cpt, normalise_cpt

data = CPTData(
    depth_m=[1, 2, 3], qc=[1000, 2000, 3000], fs=[10, 20, 30],
    u2=[100, 200, 300], area_ratio=0.8, pressure_unit="kPa",
)
corrected = correct_cpt(
    data, total_stress_pa=[20000, 40000, 60000],
    equilibrium_pore_pressure_pa=[10000, 20000, 30000],
)
normalised = normalise_cpt(corrected)
```

Depth is in metres. `pressure_unit` explicitly selects Pa, kPa or MPa for
all three measured pressures; stored pressures and supplied stresses are Pa.
Area ratio is a required calibration input. Measured u2 and equilibrium u0
must use the same pressure datum, with total stress on a consistent basis.
Unit labels cannot detect numbers
entered with the wrong label. Arrays are copied into immutable tuples.

Depth must be finite, nonnegative and strictly increasing. Pressures must be
finite and arrays must have matching lengths. Cone resistance and sleeve
friction are nonnegative; measured and equilibrium pore pressures may be
negative. The net cone area ratio lies between zero and one by its area
definition. A missing `u2` may be specified for an individual row or the whole
sounding. Corrected resistance, classification and the CPTU drainage check
then carry `NOT_EVALUATED`; `qc` is never silently substituted for `qt`.

`NOT_EVALUATED` is accompanied by a reason and `None` for unavailable numeric
results. Measured values and calculable intermediate quantities remain
available. For example, a nonpositive net resistance is retained, while its
`Bq` and logarithmic interpretation are unavailable.

## Equations and source qualification

G22 is the *Guide to Cone Penetration Testing*, seventh edition (2022).
The public PDF is hosted under a 2024 filename, but its title and page headers
identify the 2022 edition. R09 is *Interpretation of cone penetration tests —
a unified approach*, Canadian Geotechnical Journal 46 (2009), 1337–1355.
Both documents were opened on 2026-10-22. Equations are implemented without
digitising experimental curves or copying source tables.

| Quantity | Equation or rule | Source locator |
| --- | --- | --- |
| Corrected cone resistance | `qt = qc + u2 (1-a)` | G22 p. 18 |
| Net cone resistance | `qn = qt - sigma_v` | G22 p. 32 |
| Friction ratio, percent | `Rf = 100 fs/qt` | G22 terminology and p. 26 |
| Effective vertical stress | `sigma_eff = sigma_v - u0` | R09 stress definitions; G22 p. 32 |
| Pore-pressure ratio | `Bq = (u2-u0)/qn` | G22 p. 31 |
| Normalised friction, percent | `Fr = 100 fs/qn` | G22 p. 32; R09 eq. 2 |
| Normalised cone resistance | `Qtn = (qn/pa) (pa/sigma_eff)^n` | R09 eq. 6 |
| Behaviour index | `Ic = sqrt((3.47-log10(Qtn))^2 + (log10(Fr)+1.22)^2)` | R09 eq. 5, with Qtn per eqs. 6–7 |
| Stress exponent | `n = min(1, 0.381 Ic + 0.05 sigma_eff/pa - 0.15)` | R09 eq. 7 |
| Reference pressure | `pa = 100 kPa` unless explicitly supplied | G22 fig. 22 p. 29 |
| Undrained strength | `Su = qn/Nkt`; Nkt required | G22 p. 41 |
| Relative density, fraction | `Dr = sqrt(Qtn/350)` | G22 p. 49 |
| Peak friction angle, degrees | `phi = 17.6 + 11 log10(Qtn)` | G22 p. 53 |

Normalisation begins at `n=1` and iterates equations 5–7. The numerical
controls are an absolute exponent residual of `1e-10` and a budget of 200
iterations; these are software controls, not engineering acceptance limits.
They may be supplied explicitly. The returned index and resistance use the
returned exponent. Exhaustion returns `NOT_EVALUATED` rather than a last
iterate presented as a classification. `method="1990"` uses fixed `n=1`.
Positive effective stress, net resistance and sleeve friction are required
for logarithmic interpretation. Zero sleeve friction is not replaced by a
small invented value.

The Ic boundaries 1.31, 2.05, 2.60, 2.95 and 3.60 were read from R09 fig. 2,
p. 1342. Zone descriptions follow G22 fig. 23. The software assigns equality
to the finer band as a stated convention, rather than implying a precise
geological transition. Ic alone cannot identify sensitive soils or the
cemented/heavily overconsolidated zones 1, 8 and 9. Behaviour labels do not
establish grain-size classification, organic content or design suitability.

## Drainage screening and the unresolved correction

The requested velocity scaling is `V = v d/c`, with penetration rate `v`
in m/s, diameter `d` in m and consolidation coefficient `c` in m²/s. The caller
must identify whether the coefficient is vertical `cv` or horizontal `ch`.
No isotropy or conversion between them is assumed. A zero rate is a limiting
mathematical case; it does not represent an advancing CPT measurement.

`DrainageBands` requires both boundaries, consolidation basis, citation and
source locator. There are no numerical default bands. Its lower boundary is
included in `DRAINED`, its upper boundary in `UNDRAINED`, and the interior in
`PARTIALLY_DRAINED`. This inclusion policy is explicit software behaviour.
The input basis must match the calibration basis. Citation strings require
independent review; their presence does not establish applicability.

With rate, diameter and consolidation coefficient present, V is retained
even when bands are absent. Missing pore pressure returns `NOT_EVALUATED`
for the CPTU check even if the other inputs are present.

The citation for *Influence of Partial Consolidation during Cone Penetration
on Estimated Soil Behavior Type and Pore Pressure Dissipation Measurements*,
Journal of Geotechnical and Geoenvironmental Engineering 138 (2012), 777–788,
was confirmed on a public institutional publication list. Its publisher page
returned HTTP 403; the institutional PDF page requires a password. Repository
requests returned HTTP 403 or 404. No login or protected document was used.

The 1994 penetration-rate bands and the requested 2008 backbone calibration
were not obtained from readable open papers. Consequently, neither numerical
bands nor a fitted quantitative correction are embedded. The dimensional
velocity identity is implemented; the source-specific regime and correction
are not inferred from it. `correct_partial_drainage` always retains the
measured resistance and returns `NOT_EVALUATED` with no corrected value.
Published worked-point, monotonic-backbone and asymptote tests remain deferred
with the unimplemented fit. Toy bands in tests exercise the interface only;
they are explicitly synthetic and are never library defaults.

## Conditional strength and sand state

`undrained_strength` requires `nkt` and an explicit Boolean `undrained`
assertion. False returns an unavailable estimate. G22 p. 41 quotes a typical
Nkt range of 10–18, but the module uses that range as an informational message
only. It neither supplies a default factor nor rejects a calibrated factor
outside that range. An undrained assertion does not apply a drainage correction.

`SandConditions` requires explicit assertions of clean, quartz, uncemented,
unaged, rounded, drained material and medium compressibility. Unknown or false
conditions do not produce estimates. These combined applicability
conditions combine the qualifications stated on G22 pp. 48–49 and 53.
The simple relative-density equation additionally requires `Ic < 1.6`.
Density outside the physical fraction interval `(0,1]` and friction angle
outside `(0,90)` degrees are unavailable rather than clipped. The opened
source gives no universal numeric stress validity interval for the friction
angle equation; the result remains a conditional correlation, not certified
extrapolation. Sensitivity, ageing, crushing, mineralogy and cementation need
separate assessment. Material and drainage assertions are not inferred from Ic.

## Database output and provenance

`to_database_rows` returns the existing immutable `ParameterRow` records.
`ProfileProvenance` requires observation source, locator, soil unit, region,
row prefix, licence code and confidence grade. It validates the existing
schema codes and requires a read measurement source; no source permissions
are assigned by the adapter. Calibration citations remain distinct from the
measurement source.

Each point retains measured quantities, area ratio, total/effective stress,
equilibrium pore pressure, corrected and normalised quantities, the selected
method and reference pressure, iteration count, zone, drainage inputs, basis,
bands, flag and correction availability. Optional strength and sand estimates
retain their method and applicability assertions. Numeric strings retain
round-trip precision; rounding is confined to the printed example.

`depth_range` explicitly says **point observation**. The adapter neither
averages observations into layers nor invents layer boundaries. Drainage
results must belong to the same corrected observation; mismatched results or
array lengths are rejected. Optional correlations must be supplied in the
same observation order. Missing quantities are stored as `NOT_EVALUATED`,
which the existing loader preserves as nonnumeric text with a reason.

The database loader already converts Pa, percent and dimensionless values.
Its existing unit-conversion allowlist does not cover every exported unit,
including m and m²/s; those rows retain printed numeric values and units but
have unavailable SI conversion. This module does not extend that allowlist.
The round-trip tests exercise schema validation, YAML persistence, numeric
access and missing-data controls without changing database code.

## Worked synthetic 20 m profile

Run from the repository root:

```sh
PYTHONPATH=src uv run --no-project python examples/workflows/cpt-interpretation/example.py
```

All geological values below are synthetic illustration inputs. They are not
published field measurements or recommended defaults. For depth `z` in m,
total stress is `18 z` kPa and equilibrium pore pressure is `10 z` kPa.
Area ratio is 0.8. Rate is 20.0 mm/s and cone diameter is 40.0 mm.

| Assumed interval | qc, kPa | fs, kPa | u2, kPa | Vertical consolidation coefficient, m²/s |
| --- | --- | --- | --- | --- |
| Clay, observations at 1–10 m | `68 z` | `3 z` | `50 z` | `1.0e-7` |
| Sand, observations at 11–20 m | `8000 + 300 z` | 20.0 | `10 z` | `1.0e-2` |

At 1 m, direct substitution gives `qt=68+50(1-0.8)=78.0 kPa` and
`qn=78-18=60.0 kPa`. Effective stress is `18-10=8.0 kPa`, `Fr=5.0%`
and `Bq=(50-10)/60`. The capped exponent is 1.0, so `Qtn=60/8=7.5`
and the unrounded index lies in clay zone 3. With a separately specified,
illustrative `Nkt=15`, the conditional strength is `60/15=4.0 kPa`.
At 20 m, `qt=14000+200(1-0.8)=14040.0 kPa` and
`qn=14040-360=13680.0 kPa`. The iteration gives sand zone 6.

| Depth, m | qt, kPa | qn, kPa | Qtn | Ic | Zone | Conditional Su, kPa | Conditional Dr, % | Conditional phi, degrees | Drainage flag |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 1.0 | 78.0 | 60.0 | 7.5 | 3.2 | 3 | 4.0 | NOT_EVALUATED | NOT_EVALUATED | NOT_EVALUATED |
| 10.0 | 780.0 | 600.0 | 7.5 | 3.2 | 3 | 40.0 | NOT_EVALUATED | NOT_EVALUATED | NOT_EVALUATED |
| 20.0 | 14040.0 | 13680.0 | 108.3 | 1.5 | 6 | NOT_EVALUATED | 55.6 | 40.0 | NOT_EVALUATED |

Classification uses unrounded values; displayed outputs have one decimal
place. The example prints all 20 inputs and answer rows. It assumes undrained
clay and eligible drained sand solely for the conditional correlations;
these assumptions are unverified independently of the velocity flag.
The displayed velocities are 8000.0 in clay and 0.1 in sand, but no regime is assigned
without calibration. The output includes 600 database rows with grade C
synthetic provenance, including explicit unavailable fields.

The example demonstrates the arithmetic and missing-evidence behaviour.
It is not a site-specific interpretation or a design acceptance result.
Required follow-up is source qualification of drainage bands and backbone
constants, site calibration of Nkt and sand correlations, and independent
engineering review before using derived properties in foundation design.

## Sources

- [G22: public seventh-edition CPT Guide](https://cpt-robertson.com/wp-content/uploads/2024/09/cpt-guide-7th-2024.pdf), 2022 edition, locators above; opened successfully.
- [R09: public copy of the 2009 normalisation paper](https://cpt-robertson.com/PublicationsPDF/Robertson%202009%20CGJ.pdf), equations 2, 5–7 and fig. 2; opened successfully.
- [Public institutional publication list](https://faculty.engineering.ucdavis.edu/dejong/publications/), confirming the 2012 partial-consolidation citation only; opened successfully.
- [2012 partial-consolidation paper DOI](https://doi.org/10.1061/(ASCE)GT.1943-5606.0000646), publisher access failed; no fit constants or regime limits were extracted.

Source responses, retrieval addresses, HTTP status and UTC retrieval times
are retained outside the repository. No source PDF is redistributed here.
