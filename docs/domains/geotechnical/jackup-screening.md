# Generic jack-up screening demonstrator

This example calculates a spudcan resistance curve and illustrative three-leg
static checks using selected public facts and explicit assumptions. It is a
**generic screening demonstrator, not a verified site-specific assessment**.
Published-case disagreement and missing environmental inputs prevent an
installation acceptance conclusion. The way forward is to qualify those inputs
and improve the penetration method before using it for a particular installation.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-27 | Add public research calculations, benchmark limitations and an offline example to make the screening boundaries reproducible. |

## Run the example

Use an environment containing the repository dependencies. Direct all outputs and
plotting caches outside the checkout.

```sh
PYTHONPATH=src python examples/domains/geotechnical/jackup_screening.py "$OUTPUT_DIRECTORY"
```

The example verifies the frozen input digest before calculating. It writes
`results.json` to the supplied directory. It performs no network request and
uses no licensed solver. The [implementation plan](../../plans/jackup-screening-demonstrator.md)
records scope, rights and verification.

## Inputs and reuse

The numerical snapshot is [inputs.json](../../../examples/data/jackup_screening/inputs.json).
Its [manifest](../../../examples/data/jackup_screening/manifest.json) hashes the
exact committed bytes and records digests of external source collections,
research manuscripts and the observation archive. Digests prove byte identity,
not engineering applicability or publication rights.

| Input | Basis | Use and limitation |
| --- | --- | --- |
| Generic particulars | Anonymous aggregates of the supplied public specification collection; 100 entries explicitly state three legs | Median hull length 72.1 m, breadth 66.4 m, depth 7.9 m and leg length 153.4 m. Missing fields are excluded independently; source configurations differ. No identified unit is represented. |
| Design context | Anonymous aggregates of 41 public design records | Context only. Different unit types, concepts and loading conditions are not combined into a unit rating. |
| Layered soil | [Hossain et al. (2019), Table 1, Site 3 and equation 1](https://api.research-repository.uwa.edu.au/ws/portalfiles/portal/45701479/hossaintxt.pdf) | Selected numerical facts for a published anonymized Gulf profile: sand to 4.9 m, clay below it. Strength is defined at the interface and increases below it. It is not colocated with the buoy. |
| Observations | [NOAA NDBC station 42019, 2024 archive](https://www.ndbc.noaa.gov/data/historical/stdmet/42019h2024.txt.gz) | Replayed through the existing NOAA client parser. 52,698 timestamped records; 16,693 usable wave-height and 50,597 usable wind-speed values. Sample maxima are 6.8 m and 24.3 m/s. Missing values are counted, not replaced with zero. |
| ERA5 | Existing ERA5 client interface | Unavailable: authenticated retrieval is outside the permitted access. No credential was inspected or used. No ERA5 value is substituted. |
| Bathymetry | Existing GEBCO client endpoint and [current public service description](https://www.gebco.net/data-products/gebco-web-services/web-map-service) | The existing numerical endpoint returned HTTP 404. The current WMS description supplies maps; no numerical water depth was recovered. A map colour is not a depth measurement. |
| Settlement context | [Li et al. (2018)](https://pmc.ncbi.nlm.nih.gov/articles/PMC6218051/) | Public collection identifies a post-preload settlement study. No settlement equation, numerical input or copied source content is used here. Settlement is not equated with punch-through. |
| Shelf-strength context | [BSEE research report 552](https://www.bsee.gov/sites/bsee.gov/files/research-reports/552ab.pdf) | Collection notes identify public strength curves and proprietary extensions. Neither is digitized or used here. The selected layered profile comes from the cited research case instead. |

Only selected facts, anonymous aggregates and original calculations are included;
source tables, text, documents and figures are excluded. The Hossain accepted
manuscript is identified as CC BY-NC-ND in the supplied source register. Numerical
facts are cited; no adapted source figure is supplied. NOAA observation facts are
United States government data. Public specification access does not establish an unrestricted
reuse licence, so the original collections remain external. ERA5 and GEBCO reuse
qualification remains open because neither supplies a calculation input here.
Li and BSEE are bibliographic context from the supplied register, not claims that
their full documents were re-fetched or numerically qualified in this work.

## Illustrative assumptions

| Quantity | Value | Basis |
| --- | --- | --- |
| Longitudinal and transverse spacing | 40.0 m and 44.0 m | Illustrative assumption near the three-leg sample medians of 39.3 m and 43.1 m; isosceles triangle centred at the origin. |
| Lightship | 8,000.0 t | Illustrative assumption; the supplied collection has no lightship data. Chosen to exercise three-support equilibrium, not inferred from displacement. |
| Variable load | 3,470.0 t | Illustrative assumption using the rounded three-leg sample median; published loading conditions differ. |
| Preload per leg | 45.0 MN | Illustrative assumption within the selected public Gulf preload examples; not an equipment rating or structural capacity. |
| Spudcan area | 160.0 m² | Illustrative assumption within the public design collection range of 144.0–290.0 m²; equivalent circular diameter 14.3 m. |
| Base cone angle | 13.0 degrees | Illustrative assumption satisfying the research model's conical-base domain. Actual geometry is unqualified. |
| Critical-state friction and log crushing strength | 31.0 degrees and 10.0 | Illustrative assumptions for the research model's silica-sand parameters; mineralogy is unqualified. Crushing strength uses kPa inside the logarithm. |
| Curve connections | Straight segments | Illustrative assumptions connecting zero resistance, the analytical peak and the clay branch. No published post-peak solution is claimed. |
| Structural load sensitivity | F = C Hs²; C = 1,000.0 kN/m²; application height 50.0 m | Illustrative assumptions exercising equilibrium. The height does not assert a water depth. No metocean transfer function is established. |
| Sliding resistance | 10.0 MN | Illustrative assumption supplied independently to exercise the inequality; not derived from the selected soil profile. |

Every assumption is also labelled in the frozen input snapshot. Water depth,
ERA5 values, current extremes and environmental return periods remain absent.
No claim of previous operational experience is used to fill these gaps.

## Geotechnical methods

[spudcan.py](../../../src/digitalmodel/geotechnical/spudcan.py) implements
[Hu et al. (2015), equations 4–12](https://jack-up.citystgeorges.ac.uk/wp-content/uploads/2025/09/Jackup-2015-11.pdf)
for peak pressure using an implicit, stress-dependent sand strength. Bisection
stays inside the stated dilatancy interval. Equations 13–14 provide the underlying
clay branch, including a sand-plug contribution. Conical-base calculations require
sand thickness divided by equivalent diameter between 0.16 and 1.0, and a base
angle greater than 7.0 degrees. Inputs outside these bounds are rejected.

Clay strength is evaluated as interface strength plus gradient times depth below
the interface, following equation 1's definition in Hossain. The collection warns
that depth origins must be reconciled rather than inferred from column labels.
This interface interpretation reproduces the selected published peak comparison;
it does not independently qualify the original boring interpretation.

Pressure in kPa multiplied by area in m² gives load in kN. Depth follows the
lowest level of the widest spudcan cross-section, as defined in the research
methods, rather than the cone tip. The curve includes the exact peak and layer
interface, even when the requested sampling grid misses them.

A decreasing resistance branch flags potential capacity loss. A separate flag
states whether the supplied preload reaches the peak. A preload below the peak
is **not an installation acceptance criterion**, particularly given the field
comparison below. Recovery depth solves static force equilibrium; it does not
predict rapid penetration, inertia, buoyancy arrest or leg interaction. The
caller must restrict the calculation to the known extent of its soil profile.

Uniform-clay reference pressure follows equation 5 at zero sand thickness:
q = 6.34 su + 0.56 k D. At zero gradient this reduces to q = 6.34 su.
The separate uniform-sand reference reuses the existing Hansen (1970) mudmat
expression for an equal-area square footing. Its independent closed-form test
checks that surrogate only. It does **not** establish a uniform-sand limit of the
calibrated circular layered-spudcan model. That requested validation remains open.
No industry standard document was accessed and no standard-derived criterion was
extracted for the new calculation.

## Structural methods and operating envelope

[jackup_screening.py](../../../src/digitalmodel/marine_ops/jackup_screening.py)
solves the three signed vertical reactions from vertical force and two moment
balances. External moments and support coordinates share an origin. The checks
report positive contact, maximum reaction relative to supplied preload, and
horizontal force relative to separately supplied sliding resistance.
These are illustrative inequalities without load factors or acceptance rules.

An optional drag calculator supplies a separate sensitivity model using assumed
effective areas, co-directional current and wave velocity, and linear-wave
kinematics. It uses H = Hs as an explicit regular-wave proxy, not an extreme-wave
conversion. Inertia, shielding, actual leg members and dynamic amplification are
absent. It is tested as a calculator and is not used with invented bathymetry.

The new `jackup_operating_envelope` entry point in
[operation_envelope.py](../../../src/digitalmodel/marine_ops/operation_envelope.py)
evaluates these static checks. It does not apply floating-vessel RAOs to an
elevated unit. It reports the last admissible sampled height and the first failed
sample, or an explicit grid cap. A failed sample followed by a passing sample
raises an error. No interpolation or forecast contingency is inferred.

The example's synthetic force is independent of period, so equal bounds across
periods are expected. This is a load-sensitivity illustration, not an environmental
operating envelope for a Gulf site. Only after qualified inputs are supplied can
the client adapter inform a separate environmental analysis. The adapter rejects
missing ERA5 data, empty observation records and zero or unparseable offshore depth.

## Worked result and verification

The generic footing has a calculated peak of 60.9 MN and interface resistance of
55.7 MN. Its illustrative preload is 45.0 MN, below both. The zero-environment
reaction is 37.5 MN per leg. The synthetic height grid remains admissible through
2.25 m and first fails the preload comparison at 2.5 m. These are
illustrative numerical results, not site limits.

| Check | Evidence | Interpretation |
| --- | --- | --- |
| Uniform clay | Independent q = 6.34 su test; gradient term tested separately | Reference half-space arithmetic, without deep installation corrections. |
| Uniform sand | Independent Hansen square-footing closed form | Approximation only; circular-spudcan validation is still missing. |
| Gulf Site 3 | Research peak 46.6 MN; calculated 46.5 MN, a 0.3% difference. Published preload 35.6 MN does not reach the calculated peak. | Numerical comparison agrees within the 1.0% test tolerance; no installation acceptance is inferred. |
| Gulf Site 2 | Published punch-through and final depth 15.8–18.5 m at 53.4 MN preload; calculated peak 60.0 MN and static peak-recovery depth 11.1 m | **Disagreement:** the earlier formulation does not trigger at the published preload and underestimates recovery depth. This is a documented false negative, not validation success. |
| Equilibrium | Independent vertical force and biaxial moment sums | Checks sign conventions, unequal reactions and the centroid load case. |
| Boundaries | Invalid numbers, geometry, layer ratios, preload, missing input, digest mismatch and grid re-entry | Rejects incomplete or out-of-domain evaluation. |
| Existing operation envelope | Existing motion-envelope regression suite | Retains the existing floating-vessel calculation while adding a separate static entry point. |

The Site 2 test preserves the disagreement rather than calibrating the method to
an observation. Hossain's later paper evaluates subsequent formulations and also
records method limitations. Agreement at one case and arithmetic tests cannot
qualify a penetration model across layered Gulf soils.

## Remaining work

Supply qualified ERA5 snapshots and numerical GEBCO depth under suitable reuse
terms, plus a defensible extremes and joint-loading analysis. Audit observation
coverage, station changes, units and time conventions; the annual sample maxima
alone are insufficient. Provide a colocated geotechnical investigation, actual
unit geometry, mass distribution, preload procedure, hydrodynamic members and
combined foundation resistance. Add a directly qualified circular uniform-sand
method and more field comparisons, and replace the illustrative post-peak curve
where engineering use is intended. Independent engineering review remains required.
