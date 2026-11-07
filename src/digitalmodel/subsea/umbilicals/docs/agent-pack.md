# Umbilicals module knowledge pack

| Header | Value |
| --- | --- |
| Module | subsea/umbilicals |
| Repository | digitalmodel |
| Package | `src/digitalmodel/subsea/umbilicals` |
| Pack revision | 1 |
| Code revision | `aebd7e4bc5c3dda5f7433e20e021dd58ac17d0b3` |
| Analysis family | Engineering calculations (EC); AQG r0, section 10, Python calculations |
| Quality role | Engineering calculations family quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The package exports chemical-delivery hydraulics, cross-section sizing, supplied-limit pressure screening and static lay screening; control architecture and API 17F work remain deferred. [src/digitalmodel/subsea/umbilicals/__init__.py:L4-L17]

Chemical flow assumes steady incompressible single-phase Newtonian flow; lay assumes a uniform, inextensible, perfectly flexible line with positive net weight and excludes current, dynamics, soil contact, bending stiffness and axial stretch. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L11-L13] [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L20-L30]

Sizing assumes straight functional elements, concentric layers and a sealed outer envelope; area occupancy does not establish manufacturing fit, and supplied pressure ratings do not establish standard compliance. [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L4-L32]

The routing registry marks the enclosing subsea domain `stable`; this is a domain designation, not an independently established module qualification. [docs/registry/module-routing.yaml:L562-L567]

## 2. Inputs and outputs

Inputs are Python objects rather than file parsers; dimensions and dataclass fields below are defined by the public functions and their input classes. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L76-L118] [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L105-L179] [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L49-L62]

| Input and caller source | Unit | Allowed range and default | Evidence |
| --- | --- | --- | --- |
| `ChemicalFluid.density`, `viscosity` | kg/m³, Pa·s | Required; comparisons reject ≤0 but do not check finiteness | [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L76-L95] |
| `UmbilicalTube.inner_diameter`, `length` | m | Required; >0 comparisons only; length is developed route length | [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L98-L118] |
| `check_delivery.flow_rate`, `available_pressure` | m³/s, Pa | Required; flow comparison permits zero but friction subsequently rejects zero Reynolds number; available pressure >0 | [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L126-L149] [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L233-L242] |
| `injection_pressure`, `vertical_drop` | Pa, m | Defaults 0.0, 0.0; no direct range or finiteness guard | [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L201-L242] |
| `FunctionalElement` name, kind, count | text, text, count | Nonblank unique names; steel tube or thermoplastic hose; integer count ≥1; all required | [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L114-L135] [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L157-L159] |
| Element outside/inside diameter; wall/contents density | m; kg/m³ | Finite positive diameters and wall density; ID < OD; finite contents density ≥0; required | [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L107-L135] |
| `QualifiedLimit(value, basis)`; burst/collapse limits | Pa for pressure, m for radius, N for tension | Finite positive value and nonblank basis; optional limits default `None`; basis is not authenticated | [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L51-L64] [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L110-L122] [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L58-L59] |
| Core diameter, filler density, armour, sheath | m, kg/m³, ordered layers | Required; finite positive core diameter; nonnegative filler density; solid sheath fraction =1; element OD ≤ core diameter | [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L162-L179] |
| `RadialBuildLayer` thickness, density, material fraction | m, kg/m³, 1 | All required; finite positive; fraction ≤1 | [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L138-L154] |
| Sizing/pressure `water_density`, `gravity`; pressure `water_depth`, internal pressures | kg/m³, m/s²; m, Pa | Required; finite positive density/gravity; finite nonnegative depth/pressures; exact element-name coverage | [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L196-L201] [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L274-L298] |
| Lay configuration, submerged weight, horizontal tension, drop, available length | text, N/m, N, m, m | Required; parallel/bundled/suspended; finite positive weight/tension/capacity; nonnegative drop, positive for touchdown | [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L49-L62] [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L86-L108] |
| Companion weight, horizontal span, lower-support clearance | N/m, m, m | Default `None`; bundled requires finite companion weight and positive combined weight; suspended requires finite positive span and nonnegative clearance; other configurations reject these inputs | [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L93-L107] [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L117-L125] |

| Output | Unit and interpretation | Evidence |
| --- | --- | --- |
| `DeliveryResult` | Flow m³/s, velocity m/s, Reynolds/factor/utilisation dimensionless, pressure terms Pa; feasible when utilisation ≤1 | [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L172-L190] |
| `SizingResult` | OD m, filler m², mass kg/m, signed air/water weight N/m, element count and bounded checks | [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L182-L193] |
| `PressureResult` | External pressure Pa, per-element burst/collapse checks; aggregate FAIL for any failed check, otherwise NOT_EVALUATED while qualification is missing | [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L247-L271] |
| `LayResult` | Geometry/radius/clearance m, weight N/m, tension N, curvature 1/m, six checks; touchdown quantities absent for a suspended span | [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L65-L83] [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L130-L143] |
| `CheckResult` | PASS/FAIL/NOT_EVALUATED, demand and allowable in the compared quantity's unit, dimensionless utilisation or `None`, basis string | [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L42-L95] |

Hydraulic drop is positive downward and assists delivery; sizing pressure uses a common surface gauge datum at the assessed depth, while lay clearance measures height above a horizontal seabed and horizontal span joins the supports. No phase convention is consumed by these static scalar interfaces. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L163-L169] [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L24-L28] [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L12-L30]

## 3. Method and governing standards

| Method and implementing definition | Equation or decision | Basis and limits |
| --- | --- | --- |
| `bore_area`, `mean_velocity`, `reynolds_number` | A=πd²/4; v=Q/A; Re=ρvd/μ | Equations and SI inputs are explicit. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L120-L136] |
| `friction_factor`, `friction_pressure_drop` | f=64/Re through the coded transition band; above it, smooth-tube Blasius; Δp=f(L/d)ρv²/2 | Across 2300 < Re ≤ 4000, held 64/Re is below the Blasius comparison: 39.1% below at Re 2301 and 59.8% below at Re 4000. Consequently `friction_drop`, `required_pressure` and `utilisation` are understated relative to that comparison, so `is_feasible` can be falsely True. The code's “conservative for pressure drop” claim is incorrect; the only flag is `flow_regime == "transitional"`. C09 detects the branch discontinuity; no detection check exists for false feasibility against this comparison; issue pending (2), also manifolds S30. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L30-L39] [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L139-L160] [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L233-L255] [tests/subsea/umbilicals/test_chemical_flow.py:L60-L71] [src/digitalmodel/subsea/manifolds/docs/agent-pack.md:L186-L186] |
| `static_pressure`, `check_delivery` | Δp_static=ρgH; p_required=p_injection+Δp_friction−Δp_static; U=p_required/p_available | Gravity is fixed in the chemical core; no lower clamp on required pressure or utilisation. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L71-L73] [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L163-L169] [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L233-L255] |
| `size_umbilical` | Sum wall, contents, filler and occupied annular layer masses; grow OD by twice each thickness; subtract displaced-water mass before multiplying by supplied gravity | OpenStax, University Physics Volume 1 (2016), §14.1 density and §14.4 buoyancy; unoccupied armour is sealed massless void. [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L13-L32] [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L196-L244] |
| `check_pressure`, `_check` | External p=ρgd; burst=max(pi−pe,0), collapse=max(pe−pi,0); compare independently to caller-qualified limits | No calculated material pressure rating or governing pressure envelope; zero opposite-direction demand is intentional. [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L82-L102] [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L274-L306] |
| `analyze_lay`, touchdown | a=H/w; s=√(z(z+2a)); x=a asinh(s/a); Ttop=H+wz; Rmin=a; touchdown tension H | Deakin, Catenary Curve (2019), equations (3), (4), (9), (10), (32), (34), with datum shifted to the vertex; payout is a capacity, not a fixed-length condition. [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L10-L30] [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L144-L160] |
| `analyze_lay`, suspended | u_mid=asinh(z/(2a sinh(X/(2a)))); endpoints u_mid±X/(2a); L=a(sinh(u2)−sinh(u1)); nearest point to u=0 sets minimum radius/clearance | Same public catenary identities; support height is the lower-support datum; seabed intersection fails the assumed free span. [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L117-L143] |

Public sources opened for the closed-form checks: [OpenStax §14.1](https://openstax.org/books/university-physics-volume-1/pages/14-1-fluids-density-and-pressure), [OpenStax §14.4](https://openstax.org/books/university-physics-volume-1/pages/14-4-archimedes-principle-and-buoyancy), and [Deakin, Catenary Curve (15 August 2019)](https://www.mygeodesy.id.au/documents/Catenary%20Curve.pdf), equations (3), (4), (9), (10), (32), (34).

UNVERIFIED: White, *Fluid Mechanics*, is cited by the code for Darcy-Weisbach and Blasius without an edition or section. The textbook has not been opened for this pack; attribution of the 0.3164 coefficient and 2300/4000 regime thresholds to it is not verified. The Blasius comparison above demonstrates the direction of the coded difference, not a qualified transitional-flow correlation. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L30-L39] [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L60-L74]

API 17E / ISO 13628-5 are scope references in chemical flow, sizing and lay; API 17F occurs as deferred control-system scope, not an implemented criterion. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L60-L64] [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L4-L11] [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L4-L8] [src/digitalmodel/subsea/umbilicals/__init__.py:L16-L17]

UNVERIFIED: API 17E, ISO 13628-5 and API 17F edition, formal title and clause mapping are not established by these references; clause text not checked against the licensed copy; no edition conflict can be resolved from unspecified editions (#1994, #1995). [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L60-L64] [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L4-L11] [src/digitalmodel/subsea/umbilicals/__init__.py:L16-L17]

The returned result schemas contain numeric/status fields and basis strings rather than a `Citation` sidecar; sizing and lay explicitly return standard-qualification gaps (issue pending (4)). [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L172-L185] [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L68-L102] [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L172-L194]

## 4. Validated worked examples and regression examples

Four independent validated worksheets cover geometry/mass balance (V1), hydrostatics (V3) and catenaries (V4/V5); their properties and limits are synthetic, not product qualifications. V2 is an input-acceptance parametrization of V1: `kind` is validated but affects no computed quantity in `size_umbilical`. R1/R2 are regression examples. [tests/subsea/umbilicals/test_umbilical_sizing.py:L4-L7] [tests/subsea/umbilicals/test_umbilical_sizing.py:L51-L104] [tests/subsea/umbilicals/test_lay_analysis.py:L4-L6] [tests/subsea/umbilicals/test_lay_analysis.py:L31-L50] [tests/subsea/umbilicals/test_lay_analysis.py:L76-L102] [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L114-L128] [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L196-L244]

The displayed decimals are rounded; each comparison uses the unrounded expression in the cited test. For worksheet cases using `pytest.approx` without an override, the additional audit criterion is relative error ≤1e-6 or absolute error ≤1e-12 in the stated SI unit; this is a numerical comparison criterion, not an engineering design tolerance. [tests/subsea/umbilicals/test_umbilical_sizing.py:L61-L70] [tests/subsea/umbilicals/test_lay_analysis.py:L38-L50] [tests/subsea/umbilicals/test_lay_analysis.py:L91-L102]

| Case and classification | Independent source and input | Expected value and tolerance | Existing test node ID | Offline result at code pin |
| --- | --- | --- | --- | --- |
| V1 steel sizing; validated | OpenStax (2016), §§14.1, 14.4; inline `section()` and `element()` fixtures, no external input file | OD 60.0 mm (compare in m); mass 21π/8 kg/m (8.2); wet weight 69π/4 N/m (54.2); relative 1e-6 or absolute 1e-12 SI units | `tests/subsea/umbilicals/test_umbilical_sizing.py::test_cross_section_hand_calculation[steel_tube]` | PASS reproduced on 2026-11-07; original before/after logs unavailable; [tests/subsea/umbilicals/test_umbilical_sizing.py:L25-L70] |
| V2 hose kind; input acceptance of V1 | Same closed form and input fixture, `kind=thermoplastic_hose` | Identical arithmetic and expectations to V1; `kind` affects no computed quantity; no hose constitutive law | `tests/subsea/umbilicals/test_umbilical_sizing.py::test_cross_section_hand_calculation[thermoplastic_hose]` | PASS reproduced on 2026-11-07; original before/after logs unavailable; [tests/subsea/umbilicals/test_umbilical_sizing.py:L51-L70] |
| V3 differential pressure; validated | OpenStax (2016), §14.1 hydrostatics; inline two-element fixture | External 1.0 MPa; steel burst utilisation 1.0, hose collapse 2.0; exact pressure/status comparisons, error 0 Pa and 0 dimensionless | `tests/subsea/umbilicals/test_umbilical_sizing.py::test_pressure_differentials_and_limits` | PASS reproduced on 2026-11-07; original before/after logs unavailable; [tests/subsea/umbilicals/test_umbilical_sizing.py:L73-L104] |
| V4 touchdown; validated | Deakin (2019), equations (3), (4), (9), (10), (32), (34); inline `case()` | Length 100√3 m (173.2); span 100 ln(2+√3) m (131.7); tension 2000.0 N; radius 100.0 m; same numerical criterion as V1 | `tests/subsea/umbilicals/test_lay_analysis.py::test_touchdown_geometry_and_checks` | PASS reproduced on 2026-11-07; original before/after logs unavailable; [tests/subsea/umbilicals/test_lay_analysis.py:L17-L50] |
| V5 suspended; validated | Same source, symmetric two-support closed form; inline `case()` with supplied span and support clearance | Length 200√3 m (346.4); clearance 50.0 m; support tension 2000.0 N; same numerical criterion as V1 | `tests/subsea/umbilicals/test_lay_analysis.py::test_equal_height_suspended_span` | PASS reproduced on 2026-11-07; original before/after logs unavailable; [tests/subsea/umbilicals/test_lay_analysis.py:L76-L102] |
| R1 transition boundary; regression | Coded branch expectations, not an independently qualified transition correlation | Existing `pytest.approx` relative criteria 1e-9 and 1e-4; positive jump characterizes existing behaviour, not a pressure bound; the held factor understates the Blasius comparison (S11) | `tests/subsea/umbilicals/test_chemical_flow.py::test_friction_factor_boundary_4000_vs_4001` | PASS reproduced on 2026-11-07; original before/after logs unavailable; [tests/subsea/umbilicals/test_chemical_flow.py:L60-L71] |
| R2 printed worksheet; regression | Printed synthetic example; mass, weight, geometry and tension arithmetic is documented in the test docstring; printed pressure and check statuses remain regression expectations | 13 exact output lines; 0 differing lines (count); output quantities use their printed units | `tests/subsea/umbilicals/test_lay_analysis.py::test_worked_example` | PASS reproduced on 2026-11-07; original before/after logs unavailable; [tests/subsea/umbilicals/test_lay_analysis.py:L187-L216] |

Historical command (initial pack): `PYTHONPATH=src uv run --no-sources python -m pytest tests/subsea/umbilicals tests/docs/test_digitalmodel_routing_contract.py -q -rA -p no:cacheprovider --basetemp=<scratch>` uses the existing `pytest.ini` options, without an addopts override. The initial pack reported before/after totals of 87 collected, 87 passed, 0 failed, 0 skipped and NOT_RUN 0 at the header code revision; the original before results are not reproducible from the repository because their logs were not shipped; the original after logs were also not shipped. Package cases comprise 16 chemical, 24 lay and 33 sizing cases; the remaining 14 are routing-contract cases. [tests/subsea/umbilicals/test_chemical_flow.py:L1-L183] [tests/subsea/umbilicals/test_lay_analysis.py:L1-L216] [tests/subsea/umbilicals/test_umbilical_sizing.py:L1-L201] [tests/docs/test_digitalmodel_routing_contract.py:L67-L254]

Additional downstream command, with the same command form and code pin: `tests/subsea/manifolds/test_chemical_injection.py`; the initial pack reported before/after totals of 61 collected, 61 passed, 0 failed, 0 skipped and NOT_RUN 0. Those historical logs were not shipped; current reproduction is separate evidence. These tests exercise the shared delivery consumer and its additional boundary guards. [tests/subsea/manifolds/test_chemical_injection.py:L25-L32] [tests/subsea/manifolds/test_chemical_injection.py:L43-L298]

Current reproduction on 2026-11-07 uses documentation revision 1; its fix-up base is `4d5679c972dabe9233d498f097ac2b508715b623`. Python source and tests are unchanged between that base and the header code pin. Combined command: `PYTHONPATH=src uv run --no-project --with pytest==8.4.2 --with pytest-asyncio==0.21.1 --with pyyaml python -m pytest tests/subsea/umbilicals tests/docs/test_digitalmodel_routing_contract.py tests/subsea/manifolds/test_chemical_injection.py -q -rA -p no:cacheprovider --basetemp=<scratch>`, with `PYTHONUTF8` unset and existing pytest options. At the fix-up base and after the pack edits, each run collected 148, passed 148, failed 0 and skipped 0; the 2 legacy discoveries below remain NOT_RUN. Subtotals are 87 package/routing cases and 61 downstream cases. The current after-run status digest is SHA-256 `344ea23f879c79a5a1d0950934147ac734abc464196e6220ac0847e67ae879a1`, over `PASSED tests/...` lines sorted by Python string order (Unicode code points) encoded as UTF-8 with one LF per line. That digest, the totals and their scope are recorded in the #3899 fix-up comment; full logs remain local and are not shipped. This attestation does not reconstruct the initial pack records or clear C10.

Discovery-only legacy tests `tests/specialized/umbilical_analysis/test_umbilical_analysis_line_properties.py::test_umbilical_analysis` and `tests/specialized/umbilical_analysis/test_umbilical_analysis_installation.py::test_umbilical_analysis` are NOT_RUN (2): they import the engine, whose solver import chain queries interface availability at import time; collection was not attempted because solver-interface queries are excluded. These are mocked engine workflows, not tests of this package. [tests/specialized/umbilical_analysis/test_umbilical_analysis_line_properties.py:L8-L17] [tests/specialized/umbilical_analysis/test_umbilical_analysis_installation.py:L8-L17] [src/digitalmodel/engine.py:L17-L38] [src/digitalmodel/solvers/orcaflex/orcaflex.py:L5-L10] [src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py:L9-L13]

Historical supplementary contract-domain claim: `PYTHONPATH=src uv run --no-sources python -m pytest tests/contracts -q -rA -p no:cacheprovider --basetemp=<scratch>` reportedly passed 576 cases, failed 0 and skipped 0 at the same code revision. Its log was not shipped; this is not reproducible execution evidence from the repository and is separate from the focused package reproduction. [tests/contracts/conftest.py:L1-L34]

## 5. QA gates

Gate applicability follows AQG r0, section 10 (Python calculations), with G5 from section 9; the following are executable audit criteria, not new standard acceptance factors.

| Gate | Applicability and route | Module-specific checks |
| --- | --- | --- |
| G0 | Applicable: Python objects, SI units and supplied evidence | C01–C03 |
| G1 | Applicable: import through the package's offline tests; no target-program model is written by this package | C04; [src/digitalmodel/subsea/umbilicals/__init__.py:L20-L46] |
| G2 | Applicable: independent mass/pressure/catenary worksheets | C05–C09 |
| G3 | NOT_APPLICABLE: static scalar calculations, no dynamics | [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L23-L30] |
| G4 | NOT_APPLICABLE: no diffraction bodies or meshes | [src/digitalmodel/subsea/umbilicals/__init__.py:L6-L17] |
| G5 | Applicable: retain code pin, exact case inputs and result evidence with the calculation | C10 |

| Check | Gate | Executable operation and numeric pass criterion | Evidence and result |
| --- | --- | --- | --- |
| C01 | G0 | Run existing invalid-element, invalid-section, invalid-layer, invalid-pressure and invalid-lay parameterized tests: 0 unexpectedly accepted cases (count) | PASS; [tests/subsea/umbilicals/test_umbilical_sizing.py:L133-L201] [tests/subsea/umbilicals/test_lay_analysis.py:L137-L184] |
| C02 | G0 | Audit `check_pressure` input keys: 0 omitted element names (count); values use Pa, depth m, supplied gravity m/s² and density kg/m³; run `test_invalid_pressure_inputs` | PASS; [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L287-L298] [tests/subsea/umbilicals/test_umbilical_sizing.py:L182-L201] |
| C03 | G0 | Run missing-ratings and missing-lay-limits tests: 0 PASS statuses for absent evidence (count); audit `QualifiedLimit` tests: 0 accepted blank bases (count) | PASS; [tests/subsea/umbilicals/test_umbilical_sizing.py:L107-L118] [tests/subsea/umbilicals/test_umbilical_sizing.py:L176-L179] [tests/subsea/umbilicals/test_lay_analysis.py:L137-L140] |
| C04 | G1 | Run the combined section-4 reproduction command: 148 passing cases, 0 failed and 0 skipped cases (count); retain separate status for the 2 excluded solver-import discoveries | PASS reproduced on 2026-11-07; totals and status digest in section 4 and the #3899 fix-up comment; [tests/subsea/umbilicals/test_chemical_flow.py:L11-L21] [tests/subsea/manifolds/test_chemical_injection.py:L25-L32] [tests/docs/test_digitalmodel_routing_contract.py:L124-L140] |
| C05 | G2 | V1 worksheet, with V2 input acceptance: compare `size_umbilical` OD, mass and signed weights with the explicit independent worksheet; relative error ≤1e-6 or absolute ≤1e-12 in m, kg/m and N/m; retain upward buoyancy sign | PASS; [tests/subsea/umbilicals/test_umbilical_sizing.py:L51-L70] [tests/subsea/umbilicals/test_umbilical_sizing.py:L121-L130] |
| C06 | G2 | V3: external pressure error 0 Pa; steel burst utilisation error 0, hose collapse utilisation error 0 (dimensionless); opposite-direction demand exactly 0 Pa | PASS; [tests/subsea/umbilicals/test_umbilical_sizing.py:L73-L104] |
| C07 | G2 | V4 plus supplementary bundle hand-check test: `analyze_lay` lengths/radii/tensions within relative 1e-6 or absolute 1e-12 m/N; too-short capacity and insufficient residual limit yield 0 false PASS statuses (count) | PASS; [tests/subsea/umbilicals/test_lay_analysis.py:L31-L73] |
| C08 | G2 | V5 plus supplementary unequal-support and seabed-intersection hand-check tests: clearance error ≤1e-6 relative or ≤1e-12 m; negative clearance produces 0 PASS contact statuses (count) | PASS; [tests/subsea/umbilicals/test_lay_analysis.py:L76-L134] |
| C09 | G2 | R1: check both branch expressions with the existing relative 1e-9/1e-4 tolerances (dimensionless), require 1 positive jump (count); this characterizes the discontinuity but does not detect understated transitional pressure or false feasibility against Blasius | PASS as regression only; no detection check exists for the S11 feasibility error; issue pending (2). [tests/subsea/umbilicals/test_chemical_flow.py:L60-L71] |
| C10 | G5 | Audit a retained record against both commands: 1 code pin, 1 input fingerprint over pinned code/fixtures, 7 named section-4 cases, 148 executed cases, 2 NOT_RUN discoveries, 0 missing case results (count); retain exact fixtures and log totals; no design clearance follows from these counts | NOT_REPRODUCIBLE: the original fingerprint, case logs and boundary observations were not shipped in the repository or PR; source fixtures alone cannot establish execution of those historical runs. [tests/subsea/umbilicals/test_umbilical_sizing.py:L25-L104] [tests/subsea/umbilicals/test_lay_analysis.py:L17-L102] [tests/subsea/umbilicals/test_lay_analysis.py:L187-L216] |

## 6. Known failure modes and pitfalls

sweep: 29 found

Observation date: 2026-11-07. Sweep record at the code pin: the following 29 rows cover distinct boundary/default/unit/error-path findings, including intentional sentinels; counts denote findings, not independent defects. The single exception handler rethrows `ValueError`; no swallowed exception, record-skipping `continue`, bare `return None`, error-path empty container, external-text numeric conversion, epoch default or cache write occurs in the four package files. [src/digitalmodel/subsea/umbilicals/__init__.py:L1-L74] [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L1-L255] [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L1-L306] [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L1-L194]

Reproduce the text sweep, then inspect each enclosing definition, including defaults and finite checks:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|\b(int|float)\(|= 0(\.0)?\b|1970|0\.05|[Uu]nit|[Cc]ache|None|GRAVITY|LAMINAR_RE|TURBULENT_RE' src/digitalmodel/subsea/umbilicals --glob '*.py'
rg -n 'Pa|kg|m/s|N/m|finite|default|massless|datum|fraction' src/digitalmodel/subsea/umbilicals --glob '*.py'
```

| Finding | Symptom / cause and cited location | Detection check | Tracking |
| --- | --- | --- | --- |
| S01 | Density NaN/+infinity passes a ≤0-only guard. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L91-L93] | no detection check exists in direct package tests; downstream finite guards are separate | issue pending (1) |
| S02 | Viscosity NaN/+infinity passes a ≤0-only guard. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L94-L95] | no detection check exists in direct package tests | issue pending (1) |
| S03 | Bore NaN/+infinity passes a ≤0-only guard; extreme bore can underflow area. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L114-L123] | no detection check exists in direct package tests | issue pending (1) |
| S04 | Route length NaN/+infinity passes a ≤0-only guard; unbounded arithmetic can overflow loss. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L117-L118] [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L153-L160] | no detection check exists in direct package tests | issue pending (1) |
| S05 | Non-finite flow can propagate through velocity/Reynolds/friction calculations. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L126-L160] | no detection check exists in direct package tests | issue pending (1) |
| S06 | Non-finite available pressure passes its comparison-only guard and can corrupt utilisation. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L233-L242] | no detection check exists in direct package tests | issue pending (1) |
| S07 | Injection pressure defaults to 0 Pa and has no finite/datum guard. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L206-L206] [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L241-L242] | no detection check exists for invalid injection pressure | issue pending (1) |
| S08 | Vertical drop defaults to 0 m; signed head is allowed but non-finite input is unchecked. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L163-L169] [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L207-L207] | no detection check exists for non-finite drop | issue pending (1) |
| S09 | Zero flow passes `mean_velocity` but reaches a nonpositive-Reynolds rejection in delivery. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L126-L149] [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L236-L239] | no detection check exists in direct package tests | issue pending (3) |
| S10 | Fixed chemical gravity and SI-only input assumptions can disagree with supplied sizing/lay units; no conversion is performed. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L71-L86] [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L163-L169] | C02 checks pressure units, but no automatic mixed-unit detector exists | issue pending (4) |
| S11 | Across 2300 < Re ≤ 4000, held 64/Re lies below Blasius: 39.1% below at Re 2301 and 59.8% below at Re 4000. `friction_drop`, `required_pressure` and `utilisation` are understated relative to that comparison, so `is_feasible` can be falsely True. The code's “conservative for pressure drop” claim is incorrect; the only flag is `flow_regime == "transitional"`; see manifolds S30. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L30-L39] [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L139-L150] [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L233-L255] [src/digitalmodel/subsea/manifolds/docs/agent-pack.md:L186-L186] | C09 records the discontinuity; no detection check exists for understated pressure or false feasibility against Blasius | issue pending (2) |
| S12 | Required pressure/utilisation can be negative under large assisting head; feasibility is only an upper-bound comparison. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L187-L190] [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L240-L242] | no detection check exists for negative required-pressure interpretation | issue pending (1) |
| S13 | `_unevaluated` defaults demand to `None` and returns absent allowable/utilisation; consumers must inspect status. [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L68-L79] | C03: 0 false PASS statuses | #1994 |
| S14 | `_check` accepts optional evidence and returns NOT_EVALUATED if absent; zero minimum demand gives `None` utilisation. [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L82-L95] | C03 covers absent limits; no detection check exists for zero minimum-demand utilisation | issue pending (4) |
| S15 | Standard mapping always returns NOT_EVALUATED; individual PASS results cannot establish compliance. [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L98-L102] [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L264-L271] | C03/C06; V3 aggregate remains FAIL | #1994 |
| S16 | Burst allowable defaults `None`, including at zero differential. [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L121-L121] | C03: 0 PASS statuses without ratings | #1994 |
| S17 | Collapse allowable defaults `None`, independently of burst. [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L122-L122] | C03: 0 PASS statuses without ratings | #1994 |
| S18 | Empty contents and filler densities are allowed; effective wall density and sealed massless layer voids are caller assumptions, not product qualification. [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L107-L111] [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L138-L154] [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L172-L179] | C05 checks synthetic mass and signed buoyancy; physical basis authentication has no detection check | #1994 |
| S19 | Packing remains NOT_EVALUATED after nonnegative area occupancy; no circular packing proof is supplied. [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L203-L208] [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L235-L244] | C05: packing status stays NOT_EVALUATED | #1994 |
| S20 | Burst demand clamps the opposite pressure difference to zero; pressure envelope selection and datum remain caller responsibilities. [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L282-L305] | C02/C06: exact mapping and 0 Pa opposite demand | #1994 |
| S21 | Collapse demand independently clamps the opposite difference to zero; a burst PASS cannot replace collapse evidence. [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L299-L305] | C06: hose collapse utilisation 2.0 gives FAIL | #1994 |
| S22 | Lay bend limit defaults `None`; missing evidence must not become PASS. [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L58-L58] [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L182-L183] | C03: missing-limits test | #1994 |
| S23 | Lay residual limit defaults `None`; touchdown tension is H, not a far-end soil-friction solution. [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L59-L59] [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L27-L30] [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L149-L153] | C03/C07 | #1994 |
| S24 | Companion weight defaults `None`; bundled configuration requires it and positive combined weight. [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L60-L60] [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L93-L99] | C01/C07 | #1994 |
| S25 | Horizontal span defaults `None`; suspended geometry requires finite positive span. [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L61-L61] [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L117-L125] | C01 | #1994 |
| S26 | Lower-support clearance defaults `None`; suspended geometry requires nonnegative support clearance and may still sag through the seabed. [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L62-L62] [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L120-L143] | C01/C08 | #1994 |
| S27 | Suspended touchdown tension/curvature are `None`; the residual check is NOT_EVALUATED, not zero tension. [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L135-L136] | C08: suspended-span absent touchdown assertions | #1994 |
| S28 | Ideal touchdown clearance/contact are zero/PASS without soil or stiffness analysis; configuration interaction remains NOT_EVALUATED. [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L149-L171] | C07 covers geometry only; no detection check exists for omitted soil/bundle physics | #1994 |
| S29 | Numerical overflow/division errors are translated to ValueError; computed geometry is checked for finiteness, not silently replaced. [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L161-L166] | C01: overflow test expects ValueError | #1994 |

Incident review: #488 is a scope-history reference; #1994 carries qualification/sizing/lay follow-up and #1995 carries deferred controls. Their historical absence statements are not current code facts: sizing and lay are now exported, while controls remain explicitly deferred. No confirmed incident requiring a separate historical detection test was identified; rows above distinguish checks from uncovered boundaries. [src/digitalmodel/subsea/umbilicals/__init__.py:L8-L17] [src/digitalmodel/subsea/umbilicals/__init__.py:L34-L46]

## 7. Lessons log

Append new evidence without rewriting earlier entries; retain a public source or repository citation and an issue reference with each future entry.

| Date | Change reference | Lesson | Evidence | Pack change | Shared-log reference |
| --- | --- | --- | --- | --- | --- |
| 2026-11-07 | Initial pack | Keep individual supplied-limit PASS distinct from standard qualification; a suspended span has no touchdown result | [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L264-L271] [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L135-L143] | Sections 2, 5 and 6 separate missing evidence and numeric success | Future entry tracked with #1994 |
| 2026-11-07 | #3899 | Transitional 64/Re understates the Blasius comparison; a boundary regression does not detect false feasibility, and a second accepted element kind does not create an independent worksheet | [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L139-L150] [tests/subsea/umbilicals/test_chemical_flow.py:L60-L71] [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L196-L244] | Sections 3–6 state the error direction, worksheet scope and execution-evidence limits | Existing downstream finding: #2005 |

## 8. Ownership

Review roles for this pack: engineering calculations family quality manager; deputy role: engineering calculations deputy quality manager. Production responsibility: calculation maintainers. Route ambiguous criteria, missing evidence and changed tolerances to the family quality manager; route cross-family conflicts to the maintainers. These are review assignments, not a record of completed approval (AQG r0, section 11).

## 9. Dependencies and implementation inventory

The four package files depend on Python standard-library math/dataclasses/typing/collections interfaces and local sizing helpers; no external solver version is an input to their public scalar calculations. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L66-L69] [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L35-L40] [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L33-L46] [src/digitalmodel/subsea/umbilicals/__init__.py:L20-L46]

The example passes sizing's submerged weight into lay screening; the downstream manifold chemical-distribution function delegates common-tube loss and hydrostatic head to this package. Cross-section schemas are a complementary metadata interface, not an automatically wired conversion to these dataclasses. [examples/subsea/umbilical_sizing_and_lay.py:L45-L64] [src/digitalmodel/subsea/manifolds/chemical_injection.py:L172-L192] [docs/domains/umbilical/sizing-and-lay-analysis.md:L27-L35]

### Map basis and inventory

All map citations resolve at the header code pin, alongside the other repository citations; no separate development-branch retention is required. The code-pin map has 54,591 lines and declares data revision `270c8759c374b67a7f032953e7fcdb14e0224461`. [docs/registry/implementation-map.yaml:L1-L4]

A8 audit result: 0 module map families and 0 implementation rows with paths inside `src/digitalmodel/subsea/umbilicals/`; the section-9 implementation inventory is empty. Ranking therefore selects 0 counted families; the counted-family grep reads NOT_APPLICABLE because no map family has an in-package row. The full code-pin map contains 0 occurrences of `subsea/umbilicals`. This is an inventory result, not evidence that the package has no calculations. [docs/registry/implementation-map.yaml:L1-L54591] [src/digitalmodel/subsea/umbilicals/__init__.py:L6-L14]

Reproduce the inventory by parsing `families` in map order and selecting every `implementations` row whose `path` starts with the package path plus `/`. For any future counted families, sort by descending in-package row count with map-order ties; run each `search_signals[*].pattern` exactly as a case-sensitive Python `re` over pinned package `.py` text, omitting a signal if `re.search(pattern, 'subsea/umbilicals', re.I)` succeeds; assign hits to the innermost enclosing function/class or one module-level definition. The present empty inventory yields 0 counted searches and no counted misses. [docs/registry/implementation-map.yaml:L1-L54591]

### Repository-wide follow-up search

The additional search includes `src/`, `scripts/`, `examples/` and `tests/`; findings here do not change the empty counted-family inventory.

```sh
rg -n 'Darcy|Weisbach|Poiseuille|Blasius|def .*catenary|math\.(?:a?sinh|cosh)|np\.cosh|def .*pressure|UmbilicalAnalysis|get_umbilical_lines|installation_phase' --glob '*.py' src scripts examples tests
```

| Method family | Further implementation or helper | Classification and map follow-up |
| --- | --- | --- |
| Catenary geometry | `scripts/python/digitalmodel/analysis/simple_catenary_equation.py::catenary`, `scripts/python/digitalmodel/analysis/catenary_solver.py::catenary_curve` | Independent geometry formulas outside this package; the package's `analyze_lay` also computes its own formula. Add package rows to the catenary family; issue pending (5). [scripts/python/digitalmodel/analysis/simple_catenary_equation.py:L6-L17] [scripts/python/digitalmodel/analysis/catenary_solver.py:L8-L19] [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L111-L160] |
| Chemical tube hydraulics and delivery | `friction_factor`, `friction_pressure_drop`, `static_pressure`, `check_delivery`; downstream `distribute_chemical` | Package formulas are listed in section 3; the downstream function delegates them. The integration demonstration delegates friction to an external library rather than supplying an independent package formula. Add a method family and explicit helper relationships; issue pending (5). [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L139-L169] [src/digitalmodel/subsea/manifolds/chemical_injection.py:L172-L192] [scripts/integrations/fluids_evaluation.py:L17-L45] |
| Cross-section mass and sealed buoyancy | `size_umbilical`; `examples/subsea/umbilical_sizing_and_lay.py::main` | Package calculation versus example wrapper; do not count the wrapper as another implementation. Add a build-up/weight family; issue pending (5). [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L196-L244] [examples/subsea/umbilical_sizing_and_lay.py:L24-L65] |
| Supplied differential-pressure screening | `check_pressure` | Add a distinct method family/row rather than implying a material pressure-rating solver; issue pending (5). [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L274-L306] |
| Other hydraulic formulation | `WellboreHydraulics.pressure_drop_annulus` | Independent Bingham annular flow in different units, not a duplicate of Newtonian circular-bore delivery; preserve scope distinction during mapping. [src/digitalmodel/well/drilling/hydraulics.py:L103-L135] |
| Solver-based umbilical installation and line types | `solvers/orcaflex/umbilical_analysis_components.py::UmbilicalAnalysis` dispatches to `umbilical_installation_IV_to_host.py::InstallationVtoHost` and `umbilical_installation_lower_2nd_end.py::Lower2ndEnd`; line properties use `orcaflex_linetypes.py::OrcaflexLineTypes.get_umbilical_lines` | Separate OrcaFlex installation route imported by the engine and targeted by the two legacy NOT_RUN discoveries; distinct from this package's static screen. No implementation-map family is claimed. [src/digitalmodel/engine.py:L36-L38] [src/digitalmodel/solvers/orcaflex/umbilical_analysis_components.py:L5-L48] [src/digitalmodel/solvers/orcaflex/umbilical_installation_IV_to_host.py:L17-L25] [src/digitalmodel/solvers/orcaflex/umbilical_installation_lower_2nd_end.py:L17-L25] [src/digitalmodel/solvers/orcaflex/orcaflex_linetypes.py:L15-L23] |

Map follow-up covers every section-3 method: circular area/velocity/Reynolds, friction/delivery/head, build-up/weight, differential pressure and static catenary, whether the map lacks the family or lacks package rows in an existing family; issue pending (5). [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L120-L169] [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L201-L255] [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L196-L244] [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L274-L306] [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L111-L194]

## 10. Coverage and open items

### Files read and discovery boundary

Package coverage is all four Python files, with no package-local tests or pre-existing module docs; tests reside under `tests/subsea/umbilicals/`. The exact code files and test definitions are the evidence for sections 1–6. [src/digitalmodel/subsea/umbilicals/__init__.py:L1-L74] [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L1-L255] [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L1-L306] [src/digitalmodel/subsea/umbilicals/lay_analysis.py:L1-L194] [tests/subsea/umbilicals/test_chemical_flow.py:L1-L183] [tests/subsea/umbilicals/test_lay_analysis.py:L1-L216] [tests/subsea/umbilicals/test_umbilical_sizing.py:L1-L201]

Documentation discovery used `rg -l -i 'umbilical' docs --glob '*.md'` at the code pin. The following inventory records the 42 matching Markdown files; matching passages were read, with the calculation worksheet, historical plan, generic umbilical notes and downstream chemical documentation read in detail. No legacy data, nominal product properties or historical PASS labels in discovery documents are adopted as qualification evidence.

| Discovery document | Matching lines at code pin |
| --- | --- |
| `docs/subsea/cross_sections/offshore_cross_section_report.md` | [docs/subsea/cross_sections/offshore_cross_section_report.md:L1-L1] |
| `docs/subsea/manifold-chemical-distribution.md` | [docs/subsea/manifold-chemical-distribution.md:L109-L109] |
| `docs/capability-map/capability-report-2026-02-14.md` | [docs/capability-map/capability-report-2026-02-14.md:L65-L65] |
| `docs/field-development/subsea-production-systems-mapping.md` | [docs/field-development/subsea-production-systems-mapping.md:L21-L21] |
| `docs/domains/_index.md` | [docs/domains/_index.md:L75-L75] |
| `docs/domains/development/test_validation_patterns.md` | [docs/domains/development/test_validation_patterns.md:L193-L193] |
| `docs/domains/development/pre_development_testing_checklist.md` | [docs/domains/development/pre_development_testing_checklist.md:L39-L39] |
| `docs/domains/development/test_summary_with_validations.md` | [docs/domains/development/test_summary_with_validations.md:L17-L17] |
| `docs/domains/installation/jumpers.md` | [docs/domains/installation/jumpers.md:L11-L11] |
| `docs/domains/development/stability_check_results.md` | [docs/domains/development/stability_check_results.md:L66-L66] |
| `docs/plans/2026-05-05-issue-488-umbilicals-control-systems.md` | [docs/plans/2026-05-05-issue-488-umbilicals-control-systems.md:L1-L1] |
| `docs/research/field-development-source-evidence-1794.md` | [docs/research/field-development-source-evidence-1794.md:L65-L65] |
| `docs/domains/risers/damping.md` | [docs/domains/risers/damping.md:L4-L4] |
| `docs/domains/pipelines/installation/installation_analysis.md` | [docs/domains/pipelines/installation/installation_analysis.md:L42-L42] |
| `docs/research/subsea-pipelines-industry-practice.md` | [docs/research/subsea-pipelines-industry-practice.md:L156-L156] |
| `docs/plans/2026-05-05-issue-494-flexible-pipe-modeling.md` | [docs/plans/2026-05-05-issue-494-flexible-pipe-modeling.md:L10-L10] |
| `docs/domains/testing/engine_test_implementation_plan.md` | [docs/domains/testing/engine_test_implementation_plan.md:L373-L373] |
| `docs/domains/submarine-cables/tdp_compression.md` | [docs/domains/submarine-cables/tdp_compression.md:L3-L3] |
| `docs/domains/testing/engine_test_strategy_summary.md` | [docs/domains/testing/engine_test_strategy_summary.md:L65-L65] |
| `docs/domains/qgis/dxf/dxf_import_settings.md` | [docs/domains/qgis/dxf/dxf_import_settings.md:L26-L26] |
| `docs/domains/testing/engine_test_strategy.md` | [docs/domains/testing/engine_test_strategy.md:L88-L88] |
| `docs/domains/qgis/qgis_project1.md` | [docs/domains/qgis/qgis_project1.md:L30-L30] |
| `docs/domains/orcaflex/templates/subsea/jumper_hybrid/README.md` | [docs/domains/orcaflex/templates/subsea/jumper_hybrid/README.md:L56-L56] |
| `docs/domains/orcaflex/templates/platforms/fpso_hybrid/README.md` | [docs/domains/orcaflex/templates/platforms/fpso_hybrid/README.md:L151-L151] |
| `docs/domains/orcaflex/ORCAFLEX_MODULE_COMPLETION.md` | [docs/domains/orcaflex/ORCAFLEX_MODULE_COMPLETION.md:L188-L188] |
| `docs/domains/orcaflex/templates/README.md` | [docs/domains/orcaflex/templates/README.md:L43-L43] |
| `docs/domains/orcaflex/templates/mooring_systems/turret_mooring_hybrid/README.md` | [docs/domains/orcaflex/templates/mooring_systems/turret_mooring_hybrid/README.md:L147-L147] |
| `docs/domains/drilling/drilling.md` | [docs/domains/drilling/drilling.md:L148-L148] |
| `docs/domains/orcaflex/templates/umbilicals/umbilical_hybrid/README.md` | [docs/domains/orcaflex/templates/umbilicals/umbilical_hybrid/README.md:L1-L1] |
| `docs/domains/orcaflex/templates/VALIDATION_REPORT.md` | [docs/domains/orcaflex/templates/VALIDATION_REPORT.md:L53-L53] |
| `docs/domains/project-docs/UPDATED_BASELINE_AFTER_ASSETUTILITIES_FIX.md` | [docs/domains/project-docs/UPDATED_BASELINE_AFTER_ASSETUTILITIES_FIX.md:L94-L94] |
| `docs/domains/umbilical/umbilical.md` | [docs/domains/umbilical/umbilical.md:L3-L3] |
| `docs/domains/umbilical/umbilical_end.md` | [docs/domains/umbilical/umbilical_end.md:L3-L3] |
| `docs/domains/umbilical/sizing-and-lay-analysis.md` | [docs/domains/umbilical/sizing-and-lay-analysis.md:L1-L1] |
| `docs/domains/orcaflex/templates/USAGE_GUIDE.md` | [docs/domains/orcaflex/templates/USAGE_GUIDE.md:L69-L69] |
| `docs/domains/project-docs/MODEL_GENERATOR_IMPLEMENTATION_SUMMARY.md` | [docs/domains/project-docs/MODEL_GENERATOR_IMPLEMENTATION_SUMMARY.md:L30-L30] |
| `docs/domains/domains_index.md` | [docs/domains/domains_index.md:L293-L293] |
| `docs/domains/orcaflex/examples/ORCAFLEX_MODELS.md` | [docs/domains/orcaflex/examples/ORCAFLEX_MODELS.md:L175-L175] |
| `docs/domains/orcaflex/reference/text_data_file_format.md` | [docs/domains/orcaflex/reference/text_data_file_format.md:L54-L54] |
| `docs/domains/orcaflex/QUICKSTART_MODEL_GENERATOR.md` | [docs/domains/orcaflex/QUICKSTART_MODEL_GENERATOR.md:L183-L183] |
| `docs/domains/fatigue/surf.md` | [docs/domains/fatigue/surf.md:L2-L2] |
| `docs/domains/fatigue/fatigue_reliability.md` | [docs/domains/fatigue/fatigue_reliability.md:L2-L2] |

The worksheet documents the current build-up and lay example; the historical plan proposes different locations and a different turbulent correlation, so its proposals are not substituted for the delivered Blasius implementation (#1994). [docs/domains/umbilical/sizing-and-lay-analysis.md:L16-L72] [docs/plans/2026-05-05-issue-488-umbilicals-control-systems.md:L14-L20] [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L139-L150]

The calculation-citation contract was readable; existing outputs expose basis strings and qualification statuses but no structured citation sidecars, tracked by issue pending (4). No client-derived value is included and no private sidecar entry is required for the synthetic worksheets. [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L53-L75] [tests/subsea/umbilicals/test_umbilical_sizing.py:L4-L7] [tests/subsea/umbilicals/test_lay_analysis.py:L4-L6]

| Open item | Responsible role | Tracking and completion criterion |
| --- | --- | --- |
| U1: the section-3 UNVERIFIED standards edition/title/clause mapping; no licensed clause text checked | Maintainers with calculations quality manager | #1994 for API 17E / ISO 13628-5; #1995 for deferred API 17F; establish editions and clause titles without copying licensed values. [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L4-L11] [src/digitalmodel/subsea/umbilicals/__init__.py:L16-L17] |
| Direct chemical-flow finite-input, numeric-range and negative-required-pressure interpretation gaps, S01–S08/S12 | Calculation maintainers | issue pending (1); direct boundary tests and explicit finite-result policy. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L91-L118] [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L233-L255] |
| Transition interpretation and qualified correlation, S11 | Calculation maintainers | issue pending (2); correct upper-bound description and qualify the regime policy; downstream effect also recorded under #2005. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L30-L39] [src/digitalmodel/subsea/manifolds/docs/agent-pack.md:L186-L186] |
| Zero-flow API contract, S09 | Calculation maintainers | issue pending (3); test explicit zero-rate acceptance or rejection. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L126-L149] |
| Structured provenance, mixed-unit safeguards and absent utilisation interpretation, S10/S14 | Calculation maintainers | issue pending (4); define result citation/unit contract and test zero minimum demand with optional utilisation. [src/digitalmodel/subsea/umbilicals/umbilical_sizing.py:L68-L102] |
| Implementation-map families and rows from section 9 | Maintainers with calculations quality manager | issue pending (5); add every section-3 method and delegate relationship before those families become counted. [src/digitalmodel/subsea/umbilicals/__init__.py:L6-L14] |
| Product qualification, packing, helical/flooding effects, pressure envelopes, stiffness/contact, bundle interaction and dynamics, S15–S29 | Calculation maintainers | #1994; independently qualified data and appropriate physical analysis; scalar worksheet success is insufficient. [docs/domains/umbilical/sizing-and-lay-analysis.md:L125-L142] |
| Deferred control architecture and FMEA | Calculation maintainers | #1995; separate package and evidence, not an added claim for this package. [src/digitalmodel/subsea/umbilicals/__init__.py:L16-L17] |
| Historical plan/coverage alignment and future shared lesson record | Documentation maintainers | #1994; reconcile proposed paths/correlation with delivered code and record the future lesson link. [docs/plans/2026-05-05-issue-488-umbilicals-control-systems.md:L14-L22] [src/digitalmodel/subsea/umbilicals/__init__.py:L8-L17] |
| Two legacy engine discoveries not collected | Test maintainers | #1994; isolate mocked workflows from solver-interface discovery before running them under an offline-only policy. [tests/specialized/umbilical_analysis/test_umbilical_analysis_line_properties.py:L8-L17] [tests/specialized/umbilical_analysis/test_umbilical_analysis_installation.py:L8-L17] [src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py:L9-L13] |

Pending markers refer to numbered issue drafts; replace them with opened issue numbers before clearance. This draft records evidence and proposed checks; it does not record completed independent clearance (AQG r0, section 11).
