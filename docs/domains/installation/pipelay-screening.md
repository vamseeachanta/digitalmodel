# S-lay and J-lay installation screening

`digitalmodel.installation.pipelay.screening` provides four composable static
checks. It reuses the installation bending/tension helpers and the existing
`code_checks.dnv_os_f101` pressure functions. Existing workflow routing and
J-lay/reel-lay results are unchanged. All new arguments use m, N, Pa and strain
fractions (0.001 means 0.1%). Convert quantities before calling; no implicit unit
conversion is performed. Steel diameter excludes coating; wet unit weight
includes coatings, contents and displacement.

## Method and required basis

`lay_geometry` takes horizontal touchdown tension H, uniform wet weight w and
suspended height h. The natural catenary gives a=H/w, upper tension T=H+wh,
length L=sqrt(h*(h+2a)), layback x=a*asinh(L/a) and minimum radius a. Sagbend
and stinger outer-fibre strains are D/(2R). The departure angle is measured from
horizontal. In S-lay the upper point is the stinger tip; the reported tension
does not include the supported stinger/deck pipe. Add the surveyed tip offset
to locate touchdown relative to the vessel. Use the pipe centreline stinger
radius. In J-lay, overbend is `None` (not evaluated).

`pressure_screen` reports characteristic elastic, plastic, collapse and
propagation pressures, and separate utilizations (pe-pmin)*gamma_m*gamma_sc/P.
The criterion for each is utilization <=1. The inherited collapse cubic selects
its smallest positive root. The inherited propagation expression is
35*fy*alpha_fab*(t/D)^2.5. The common screening domain is restricted to
15 < D/t < 45, as stated by the public propagation source. Explicitly supply
fabrication, material and safety-class factors. The 0.005 ovality floor is
reported as `applied_ovality`. This is a plain steel pipe screen; no lined or
clad pipe capacity is established. Pressure uses a common datum; minimum
internal pressure must be sustainable throughout the load case.

The pressure formulas document the public DNVGL-ST-F101 (2017) method now
associated with DNV-ST-F101; the public BSEE guidance also documents the
collapse cubic. Compliance with the governing current edition is not
established. Select the characteristic wall (including applicable tolerances and
corrosion), derated strengths and factors independently. Where collapse and
propagation require different characteristic walls or factors, make separate
calls and retain the applicable result from each.

`displacement_controlled_check` implements the externally pressurized strain
interaction U=(epsilon_Sd*gamma_e/epsilon_c)^q+
(pe-pmin)*gamma_m*gamma_sc/Pc, with criterion U<=1. The public legacy DNV
formulation uses q=0.8. The exponent, characteristic strain capacity and all
factors are required inputs. Paid-standard capacity equations, weld factors,
material factors and tables are not supplied or inferred. Qualification of
those inputs against the governing DNV-ST-F101 edition and its applicability
limits remains a design prerequisite. Supply factored **net compressive**
longitudinal strain including bending, axial and environmental effects.
Effective catenary tension is not wall force; converting it directly to N/EA
would misstate the strain.

Displacement control requires a section-specific assessment and explicit
`displacement_controlled=True`. A supported overbend may meet that condition;
a free sagbend is not automatically displacement controlled. Apply the
appropriate load-controlled checks where required. No overall installation
pass is returned. Negative net external pressure is rejected because internal
overpressure and tensile fracture need other checks.

`ar_cable_tension` takes the pipe force components at the A&R head, actual
suspended cable length, wet cable weight and separate wet head weight. Vertical
equilibrium gives Vtop=Vend+w_c*Lc+Whead, and Ttop=hypot(Hend,Vtop).
The supplied dynamic factor multiplies Ttop; it is an assumed envelope, not a
dynamic analysis. Each A&R stage requires compatible endpoints, cable length
and pipe forces. Original lay forces do not automatically describe that stage.
Compare demand with independently qualified cable, winch and rigging capacities.

## Citation sidecars and prerequisites

`pressure_screen` and `displacement_controlled_check` return an additional
`citations` tuple of `Citation` records beside the unchanged numeric fields and
public `sources` locators. Each calculation validates its records through the
existing wiki resolver. Set `LLM_WIKI_PATH` to an authorized wiki checkout whose
source records carry the following metadata. Paths are relative to that checkout.

Citation targets may be standards, sources or concepts pages carrying the cited
clause, as permitted by the calculation citation contract, rule 7. The BSEE
record uses a source page for its External Overpressure section. Page category
does not replace edition, section, sibling or project metadata validation.

| Wiki path | code_id | publisher | revision | section |
| --- | --- | --- | --- | --- |
| `wikis/engineering/wiki/standards/dnvgl-st-f101-2017.md` | DNVGL-ST-F101 | DNV | 2017 | Sections 5.4.4-5.4.5 |
| `wikis/engineering/wiki/sources/bsee-tap-434-2003.md` | BSEE-TAP-434 | BSEE | 2003 | External Overpressure |
| `wikis/engineering/wiki/standards/dnv-os-f101-2013.md` | DNV-OS-F101 | DNV | 2013 | Sec. 5 D |

Table 1. Required edition-specific citation records; each also requires
`source_sibling: generic`, with `source_project` absent or YAML `null`.

The first two records govern the pressure checks and ovality floor; the third
governs the displacement interaction. Missing pages, mismatched publication,
edition, section or sibling metadata, unexpected project scope, and resolved
paths outside the configured wiki raise `CitationResolutionError` before a
numeric result is returned. A current-edition page cannot substitute for a
legacy-edition record. Provisioning and qualifying these wiki records remains
the responsibility of their source repository; synthetic test metadata verifies
the validation behavior without establishing engineering source qualification.

## Worked example

All inputs below are synthetic illustration values, not measured engineering
data or standard default factors. Configure the citation records above, then
run the example from this checkout:

```powershell
$env:PYTHONPATH = 'src'
uv run python examples/workflows/pipelay-screening/example.py
```

The S-lay case uses D=0.5 m, h=100 m, w=1000 N/m, H=200 kN and
stinger radius 250 m. The J-lay case uses h=1000 m and H=50 kN with the same
D and w. Pressure inputs are t=0.020 m, fy=400 MPa, E=200 GPa, nu=0.3,
fabrication factor 1, ovality 0.005, pe=2 MPa, pmin=0, gamma_m=1.15 and
gamma_sc=1.1. The illustrative strain interaction is for a separate section with
established displacement control: epsilon_Sd=0.0015, epsilon_c=0.01,
gamma_e=2 and q=0.8. It does not classify the example's free sagbend.

| Quantity | S-lay | J-lay |
| --- | ---: | ---: |
| Upper free-span tension (kN) | 300.000 | 1050.000 |
| Suspended length (m) | 223.607 | 1048.809 |
| Touchdown offset from departure (m) | 192.485 | 186.855 |
| Sagbend bending strain (%) | 0.125 | 0.500 |
| Overbend bending strain (%) | 0.100 | Not evaluated |

Table 2. Natural-catenary results; rigid-pipe touchdown bending is approximate.

The pressure calculation gives Pc=22.9 MPa and Ppr=4.48 MPa.
The collapse utilization is 0.110 and propagation utilization is 0.565,
both below the stated criterion 1. The conditional strain interaction gives
U=0.492, also below 1 for the supplied illustrative basis. This does not
establish installation acceptance. A separate A&R state with H_end=30 kN,
V_end=40 kN, cable length 100 m, wet cable weight 100 N/m and zero head weight
gives static top tension 58.3 kN and demand 70.0 kN at factor 1.2.
No cable or winch capacity is specified; that acceptance check is not established.
Independent arithmetic and the dimensionless collapse-cubic residual are
recorded in `tests/installation/test_pipelay_screening.py`.

## Ovality carry-over and limitations

Retain the as-laid ovality survey, measurement condition, Dmax/Dmin, locations,
wall measurements, dent dimensions and pressure history as operation inputs.
Initial ovality used here does not predict bending-induced ovality or permanent
deformation. A measured increase reduces computed collapse capacity; it does
not quantify fatigue life, dent strain concentration or remaining service life.
Those assessments require the retained measurements and qualified methods.

The natural catenary neglects bending stiffness and can underestimate touchdown
bending, particularly in shallow water. Stinger-tip contact, local roller loads,
residual overbend strain, field joints, seabed support/friction, current, waves,
vessel motions, free spans, fatigue, plastic cycling and fracture are excluded.
Meeting an individual screening inequality is not installation clearance.

## Native comparison follow-up

Status: **NOT_EVALUATED**. No native model or result is supplied with this change.
Use one case in the separately scheduled licensed OrcaFlex run. Retain model,
native result, software version, case identifier, units, input/output SHA-256
digests and exported tension, suspended length, touchdown position and curvature.

First reproduce Table 2's S-lay free-span boundary, without stinger/contact,
current or dynamics, in a flexible-line reference model with negligible EI and
EA sufficiently high that extension is negligible. Proposed comparison criterion:
within 1% for free-span upper tension, length and offset after mesh convergence.
Record actual EI, EA and residual extension; these are reference assumptions,
not steel pipe properties. Then restore the steel section stiffness and stinger
geometry. Refine segmentation until peak curvature changes by less than 1%;
report the difference from natural-catenary strain rather than forcing a 1%
physical agreement. The rigid-pipe result and classification of displacement
control require independent engineering review. Native code-check factors must
match the governing edition; do not use vendor internals as an implementation
source. Explorer/website integration belongs to its owning repository as follow-up.

## Public sources

- [Surrogate models for the stiffened catenaries: applications in subsea pipe laying (2023), section 3, equations (19)-(25)](https://doi.org/10.1007/s40868-023-00131-8):
  natural-catenary equilibrium, geometry and bending; section 5 discusses limits.
- [Pipeline Wall Thickness Assessment of Various Material Grades and Water Depths Using American and Norwegian Standards (2022), equations (14)-(20)](https://doi.org/10.17576/jkukm-2022-34(6)-14):
  published DNVGL-ST-F101 (2017) collapse, propagation and applicability.
- [BSEE TAP 434 (2003), Guidance Document on Strain Based Design, External Overpressure](https://www.bsee.gov/sites/bsee.gov/files/tap-technical-assessment-program/434ab.pdf):
  independent public presentation of the collapse cubic and ovality floor.
- [Anchor Damage Assessment of Subsea Pipelines (2015), p.57, equation (41)](https://hdl.handle.net/11250/1248742):
  public legacy displacement-control interaction; no project data from that work
  is used. The persistent identifier replaces the obsolete document URL. Governing
  current-edition capacity selection remains a required external input.
