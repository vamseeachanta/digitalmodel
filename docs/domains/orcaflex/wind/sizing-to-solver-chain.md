# Floating wind sizing-to-solver chain

One `ChainInput` drives the existing floater screening code, hull-only
OrcaWave diffraction inputs and generic OrcaFlex coupled-template inputs.
This is an offline preparation workflow. Every output manifest records
`solver_ready: false`; native loading and response are `NOT_EVALUATED`.
No licensed solver binding is imported, queried or started.

The [worked example](../../../../examples/workflows/floating-wind-chain/README.md)
shows the inputs, independent arithmetic and generated report.

## Input contract

Import `ChainInput` and `prepare_chain` from
`digitalmodel.floating_wind.solver_chain`. Supply:

- Archetype parameters, including every screening coefficient; topside mass
  and geometry; all screening criteria; named sea states and steady loads.
- A basis identifying the source of the values, or explicitly stating
  "assumed, based on previous experience".
- Water depth, explicit water density in kg/m³, ballast CG above keel in metres,
  frequency/heading grids and hull-only principal radii of
  gyration about the hull-only CG. The screening pitch-radius approximation
  is not copied into all axes.
- A portable GDF mesh basename. The mesh must be supplied and its consistency
  with the sized geometry reviewed; no placeholder mesh is invented.
- A repository `ProjectInputSpec` generic template and the names of its
  target floater and turbine. The target vessel type cannot be shared by
  another vessel. Remove vessel length scaling from the target.

The inherited screening code supports only its existing water density and
ballast-position convention. Both are required explicit inputs; incompatible
values are rejected instead of screening a different physical basis. The
template density must match the explicit density after conversion from
kilograms to tonnes per cubic metre; a mismatch is rejected before writing.
The assumed density is consistent with [NOAA, World Ocean Atlas 2013 FAQ,
question 8](https://www.nodc.noaa.gov/OC5/wod-woa-faqs.html), which uses a
constant seawater density assumption for a conversion. This is not a measured
site density. Standard gravity follows [BIPM, 3rd CGPM (1901), Declaration 2,
conventional value of standard gravity](https://www.bipm.org/en/committees/cg/cgpm/3-1901/resolution-2);
the source is the defining declaration rather than a site-specific value.

Each sea state replaces the template's single spectral wave train with
significant height and peak period. Only JONSWAP and Pierson-Moskowitz are
supported. Regular waves are rejected because no cited design-wave conversion
is provided. Wind,
current, heading, simulation controls and turbine properties are preserved
from the template. Nonzero steady loads are rejected because the screening
proxy is not a native applied force. No IEC design-load-case compliance is
inferred from case names.

The supported topology is a rigid connection chain from turbine through
optional lumped buoys to the floater, with aligned axes and centreline CGs.
Flexible tower lines and constraints are not translated by this slice. The
reviewed arrangement from #2423 must be integrated before a physical coupled
model is established. No vendor turbine, controller or blade data is bundled.

## Mapping and equations

The floater properties contain the *combined* hull, ballast, RNA and tower.
The coupled template must model the RNA and tower separately. Therefore:

```text
m_hull = m_total - m_topside
z_total = KG - draft
z_hull = (m_steel*z_steel,keel + m_ballast*z_ballast,keel) / m_hull - draft
I_hull,axis = m_hull * k_hull,axis²
m_diffraction,kg = 1000 * m_hull,t
```

The first-moment subtraction follows [OpenStax, University Physics volume 1
(2016), section 9.6](https://openstax.org/books/university-physics-volume-1/pages/9-6-center-of-mass).
Inertia from radius follows the mass-distance definition in [section
10.5](https://openstax.org/books/university-physics-volume-1/pages/10-5-calculating-moments-of-inertia).
The exact tonne conversion is in [BIPM, SI Brochure, ninth edition (2019),
Table 8](https://www.bipm.org/documents/20126/41483022/SI-Brochure-9.pdf)
(open copy: version 4.01, June 2026). These public sources were opened for
this implementation; response records are retained with the build evidence.
No empirical coefficient or design acceptance limit is introduced.

CG coordinates use still water as zero, positive upward. The native
VesselTypes/Draughts mass, CG and inertia keys follow the repository's L02
example syntax. Their format is reused; no vendor numeric values are copied.
The target draught is replaced completely so hydrodynamics for an older hull
cannot survive the mapping. Other objects and controller properties pass
through the generic schema.

The diffraction body carries hull-only inertia. Its radiation coefficients,
added mass, damping and load RAOs must be imported into the coupled model
for the new geometry. Bare-hull displacement RAOs are not the response of
the coupled turbine assembly. This import is currently `NOT_EVALUATED`.
Hull-only mass is less than the displaced mass by the separately modelled
topside mass. The manifest records both masses, the missing topside mass and
the criterion that body mass must balance displacement before static or RAO
evaluation. `hull_only_equilibrium` remains `NOT_EVALUATED`; this preparation
does not establish a freely floating bare-hull equilibrium.

## Validation and evidence

Each coupled specification is validated with `ProjectInputSpec`, generated by
`ModularModelGenerator` and checked by the repository's mandatory
post-generation validator. Errors are not bypassed. Schema and reference
validation establish input structure only. They cannot establish a complete
turbine definition or native solver loading.
The adapter writes UTF-8 text with LF line endings. Generation and validation
complete in a temporary sibling directory before the directory is renamed to
the requested output. A failure preserves existing data and leaves no partial
requested output. Links and directory junctions are rejected during generated
file enumeration; manifest paths use forward slashes.

Diffraction schema validation is explicitly deferred: its package initializer
imports solver components. Importing that package would violate this workflow's
offline contract. No alternate loader or weakened validator is used.

The manifest records the canonical shared-input SHA-256, screening verdict,
validation findings, sources, generated-file digests and all unevaluated checks.
Only a feasible mass balance is mapped. A failed screening verdict remains a
failure in the manifest and report; generation never makes it a passing design.

Stored `ResultRecord` JSON objects carry the shared-input digest, case, solver,
version, evidence kind, source, metric, value and unit. Supported comparisons
are coupled-assembly OrcaFlex heave and pitch natural periods in seconds;
bare-hull OrcaWave periods are rejected. A mismatched digest, unknown
case, duplicate metric or mixed version of the same solver is rejected.
Synthetic records remain labelled `SYNTHETIC`; recorded values remain caller
supplied evidence, not an attestation of the raw solver files.

Differences use `(evidence - screen) / screen * 100`, expressed to one decimal
place in the report. The percent convention is SI Brochure section 5.4.7.
An unstable/infinite screening period produces `NOT_EVALUATED`. The dimensional
motion amplitude is never compared directly with the dimensionless DAF proxy.
No acceptance threshold is assigned and causes of differences remain to be
established from the underlying results.

## Report and remaining work

`build_chain_report` returns the repository's standard `ReportSpec`, including
summary, revision history, scope, references, design basis, method, results and
case coverage, comparisons, limitations and way forward. The shared-input
digest is the report provenance. Without records every comparison is explicitly
`NOT_EVALUATED`.

The three offline slices cover mapping/generation, stored-evidence comparison,
and the report/example. Remaining work is the reviewed turbine/controller and
mass inventory, flexible connection support, wetted mesh checks, diffraction
schema validation and coefficient import, native loading, static equilibrium,
decays and operating/parked results on recorded solver versions. Those are
preconditions for claiming the complete acceptance criteria of #2435.
