# Engineering calculations knowledge pack

| Header | Value |
| --- | --- |
| Module | infrastructure/calculations |
| Repository | digitalmodel |
| Package | src/digitalmodel/infrastructure/calculations |
| Pack revision | 0 |
| Code revision | 309d343e08a2f83aebc6ea5344e586407b80c1f8 |
| Analysis family | Engineering calculations; EC |
| Quality role | Engineering calculations family quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The package exports elastic plate buckling, slenderness, characteristic and design resistance, usage-factor calculators, coefficient and direction types, and a legacy dictionary calculation. [src/digitalmodel/infrastructure/calculations/__init__.py:L11-L43]

The infrastructure domain is registered as beta; the registry does not give this package a separate maturity rating at the code pin. [docs/registry/module-routing.yaml:L212-L221]

The elastic implementation checks positive modulus, thickness and breadth and a Poisson ratio strictly between zero and one half; its longitudinal branch ignores length in the actual stress calculation. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L118-L128] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L152-L164]

`FREE` and `ELASTIC_RESTRAINT` are declared edge conditions but use fallback coefficients rather than dedicated boundary-condition solutions. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L24-L29] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L65-L88] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L154-L161]

Treat this pack as an implementation guide pending qualification, rather than evidence of design acceptance; the module labels methods DNV-RP-C201 while the active tests compare tracked and raw results and unit conversions. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L395-L413] [tests/test_plate_buckling_units.py:L62-L177]

## 2. Inputs and outputs

### Inputs

| Input | Unit and permitted range | Default and source |
| --- | --- | --- |
| `youngs_modulus`, `poisson_ratio`, `thickness`, `breadth` in elastic methods | Pa, dimensionless, m, m; modulus and dimensions positive; Poisson ratio strictly inside the checked interval | Required arguments; tracked quantities are converted by decorators. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L103-L128] |
| `length`, `edge_condition` in directional elastic methods | m; no explicit positive-length guard; enum includes unsupported fallback conditions | Length required; simply supported edge default. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L130-L161] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L166-L195] |
| `characteristic_resistance`, `elastic_buckling_stress` | Same stress unit; ratio dimensionless; nonpositive elastic stress returns zero | Required scalar arguments; no decorator. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L267-L285] |
| Stress dictionaries, `yield_strength`, `von_mises_stress`, `interaction_exponent` | Consistent stress units supplied by caller; exponent dimensionless; absolute applied stresses used | Missing applied keys become zero; missing elastic keys become unity; exponent has a code default. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L287-L334] |
| Characteristic resistance methods: yield and slenderness | Positive yield in caller's stress unit; dimensionless slenderness; nonpositive slenderness returns yield | Required arguments; no unit decorator. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L345-L393] |
| DNV-labelled resistance methods: yield, modulus, dimensions, material factor, transverse pressure and `k4_factor` | Consistent stress and length units by dimensional use; pressure must share the yield unit; factors dimensionless; no explicit input guards | Material factors and transverse factor have code defaults; pressure defaults to zero; no defaults are reproduced from a standard here. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L395-L521] |
| Usage-factor scalars and dictionaries | Applied and allowable stresses in the same unit; result dimensionless | Missing applied keys become zero; missing design resistance keys become unity; interaction factor has a code default. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L532-L612] |
| Legacy `plate_data` | Modulus and yield comments say psi; dimension comments say inches; dictionary values are not converted or validated | Every key has a built-in default; `YieldPoint` is read but not used in returned expressions. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L616-L657] |

The direction enum calls longitudinal x, transverse y and shear xy; equivalent slenderness discards applied-stress signs while basic usage takes an absolute value and biaxial usage retains the product of signed normal stresses. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L32-L36] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L310-L312] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L544-L578]

UNVERIFIED: A package-wide coordinate origin, compression-sign convention and phase convention are not established; require caller documentation before combining results (U3; issue pending (2)).

### Outputs

| Output | Unit, shape and interpretation |
| --- | --- |
| `calculate_base_factor` | Stress scale in Pa for SI inputs; returns a plain float even for tracked inputs, because no return unit is declared. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L103-L128] [tests/test_plate_buckling_units.py:L266-L276] |
| Directional elastic stresses and `calculate_all_elastic_buckling_stresses` | Pa for decorated directional calls; aggregate dictionary keys `longitudinal`, `transverse`, `shear`; tracked calls return tracked pascal quantities. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L130-L137] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L249-L256] [tests/test_plate_buckling_units.py:L284-L318] |
| Slenderness and usage | Dimensionless scalars; aggregate usage adds `biaxial` to the three directional keys; invalid denominators can return zero or infinity. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L280-L334] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L544-L612] |
| Characteristic and DNV-labelled design resistances | Stress in the caller's consistent unit; no tracked-return decorator. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L345-L521] |
| Legacy results | Dictionary of four stress-like outputs plus shear coefficient and tangent-modulus ratio; dimensional qualification of its shear expression remains open. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L636-L657] |

The executable demonstration prints elastic stresses in MPa and slenderness ratios; no file format is read or written by that main block. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L660-L699]

## 3. Method and governing standards

These rows inventory implementation behaviour; no numerical coefficient or standard table value is reproduced. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L39-L88] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L395-L521]

| Method family | Definition and implemented operation | Reference status |
| --- | --- | --- |
| Elastic plate buckling | `ElasticBucklingCalculator.calculate_base_factor` computes a modulus-dependent stress scale times squared thickness/breadth; directional methods multiply by boundary and aspect-ratio coefficients; `BucklingCoefficients` owns the transverse and shear coefficient functions. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L54-L88] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L103-L226] | Classical plate theory and DNV are claimed by the class docstring, without edition or equation. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L91-L97] |
| Reduced and equivalent slenderness | `SlendernessCalculator.calculate_slenderness_ratio` takes a square root of resistance/elastic stress; `calculate_equivalent_slenderness` combines directional power terms with yield/equivalent stress. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L267-L334] | The implementation gives no clause identifier. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L259-L334] |
| Characteristic resistance | `UltimateStrengthCalculator.calculate_characteristic_resistance_serviceability` uses a fourth-power slenderness reduction; `calculate_characteristic_resistance_ultimate` switches reduction branch with slenderness. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L345-L393] | Class docstring attributes the methods to DNV standards without edition. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L337-L343] |
| DNV-labelled longitudinal, transverse and shear resistance | `calculate_dnv_longitudinal_resistance`, `calculate_dnv_transverse_resistance`, `calculate_dnv_shear_resistance` each compute their own slenderness and reduction; the transverse method includes a simplified pressure reduction. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L395-L521] | DNV-RP-C201, *Buckling strength of plated structures*; edition and clause unspecified in these methods and migration documentation; clause text not checked against the licensed copy. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L402-L413] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L437-L451] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L487-L499] [docs/domains/structural/plate_capacity_migration.md:L232-L240] |
| Usage and interaction | `UsageFactorCalculator.calculate_usage_factor` divides absolute applied by allowable stress; `calculate_biaxial_usage_factor` takes a clipped square root of normal and shear interaction terms; `calculate_all_usage_factors` supplies dictionary defaults and delegates. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L532-L612] | DNV attribution is generic in the class docstring. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L524-L530] |
| Legacy plate calculation | `calculate_plate_buckling_212` independently computes edge, tangent-modulus and shear expressions; it does not delegate to the elastic calculator. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L616-L657] | Module-level DNV-RP-C201 and unspecified API attribution; no API number, edition or clause. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L1-L10] |

UNVERIFIED: DNV-RP-C201 edition, clause mapping, API designation and compliance of these expressions, including a calculation-output `Citation` sidecar, remain unqualified (U1; issue pending (1)).

The transverse coefficient docstring describes length/breadth whereas its elastic caller supplies breadth/length; this is a documented contract inconsistency, not a resolved equation correction. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L54-L68] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L188-L192]

## 4. Validated worked examples

Validated cases: **0**; the selected tests compare routes through the same implementation rather than an independent expected engineering value. [tests/test_plate_buckling_units.py:L62-L177]

### Regression examples

| Case | Input and expected comparison | Tolerance basis | Existing test node and offline result |
| --- | --- | --- | --- |
| R1 base scale, tracked versus raw | `steel_params_raw` and `steel_params_tracked`; expected relative discrepancy 0.0, dimensionless, between stress scales in Pa | Test assertion `rel=1e-10`; a regression tolerance, not a standard acceptance limit. [tests/test_plate_buckling_units.py:L34-L56] [tests/test_plate_buckling_units.py:L62-L81] | `tests/test_plate_buckling_units.py::TestTrackedMatchesRaw::test_base_factor_matches` — PASS at the code pin, before and after documentation changes. [tests/test_plate_buckling_units.py:L66-L81] |
| R2 longitudinal stress, tracked versus raw | Same fixtures; expected relative discrepancy 0.0, dimensionless, between stresses in Pa | Test assertion `rel=1e-10`. [tests/test_plate_buckling_units.py:L83-L98] | `tests/test_plate_buckling_units.py::TestTrackedMatchesRaw::test_longitudinal_stress_matches` — PASS at the code pin, before and after documentation changes. [tests/test_plate_buckling_units.py:L83-L98] |
| R3 length-unit conversion | Corresponding metre and millimetre tracked inputs; expected relative discrepancy 0.0, dimensionless, between longitudinal stresses in Pa | Test assertion `rel=1e-6`. [tests/test_plate_buckling_units.py:L161-L177] | `tests/test_plate_buckling_units.py::TestAutoConversion::test_mm_converts_to_m` — PASS at the code pin, before and after documentation changes. [tests/test_plate_buckling_units.py:L161-L177] |

Result record: `PYTHONPATH=src uv run --no-sources python -m pytest tests/test_plate_buckling_units.py tests/test_plate_capacity.py -q -rA -p no:cacheprovider --basetemp=<scratch>`; no addopts override; BASE and AFTER each recorded 16 passed, 0 failed, 1 module skipped, 0 NOT_RUN against code revision `309d343e08a2f83aebc6ea5344e586407b80c1f8` (documentation changes only).

The historical capacity test module skips at its import guard for a removed analysis package, citing #949; its tests are not numerical validation evidence. [tests/test_plate_capacity.py:L23-L40]

The module main block and the component example script are demonstration leads, excluded from the case table because they lack a corresponding active independent-value test. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L660-L699] [examples/domains/stress/plate_capacity_examples.py:L277-L329]

UNVERIFIED: Independent numerical validation and the migration document's published-benchmark and production-readiness claims are not established by these results (U2; issue pending (1)). [docs/domains/structural/plate_capacity_migration.md:L260-L291]

## 5. QA gates

Gate assignment: AQG r0, section 10, family **Python calculations**; G5 also applies under section 9 (AQG r0, *Analysis QA gates*, revision 0, sections 9–10).

| Gate | Applicability and module check |
| --- | --- |
| G0 | Applicable: inputs, units and citations; C0a and C0b check the elastic API, with reference gaps retained as U1. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L103-L128] [tests/test_plate_buckling_units.py:L138-L208] |
| G1 | Applicable: Python import and tests; C1 uses the Python route directly, with no solver load route. [tests/test_plate_buckling_units.py:L21-L31] |
| G2 | Applicable: C2 provides elementary scale and ratio hand checks; these do not qualify DNV reduction branches. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L125-L128] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L544-L548] |
| G3 | NOT_APPLICABLE: no time integration in the scalar calculation methods (AQG r0, section 10, Python calculations). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L249-L256] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L604-L612] |
| G4 | NOT_APPLICABLE: no diffraction mesh or hydrodynamic solve in these calculation methods (AQG r0, section 10, Python calculations). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L103-L128] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L395-L521] |
| G5 | Applicable: C5 requires calculation evidence and source traceability; no report-signature clearance is claimed (AQG r0, section 9). [tests/test_plate_buckling_units.py:L216-L264] |

The numeric criteria below define pack checks, not licensed-standard tolerances.

| ID | Quantity and executable route | Numeric pass criterion and unit | Evidence |
| --- | --- | --- | --- |
| C0a | Run `TestAutoConversion` and `TestDimensionalityError` in `tests/test_plate_buckling_units.py` through the section-4 command | Converted longitudinal stress relative discrepancy ≤ 1e-6 dimensionless; exactly 2 expected dimensionality exceptions | Pytest node outcomes; guards and conversion assertions. [tests/test_plate_buckling_units.py:L138-L208] |
| C0b | Call `calculate_base_factor` with each of E=0 Pa, nu=0.5, t=0 m, b=0 m, holding the other raw fixture values fixed | Exactly 4 `ValueError` exceptions; count unit: cases | Scratch hand-check record; existing input guards. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L118-L123] [tests/test_plate_buckling_units.py:L34-L43] |
| C1 | Import the calculation package and run the section-4 suite | 8 declared package exports import; 16 active tests pass; 0 failing tests; count unit: symbols/tests; historical skip separately recorded | Export list and all active test outcomes. [src/digitalmodel/infrastructure/calculations/__init__.py:L38-L43] [tests/test_plate_buckling_units.py:L62-L318] |
| C2 | With the raw test fixture, double thickness in `calculate_base_factor`; separately call `calculate_usage_factor(100e6, 200e6)` in Pa | Scale ratio equals 4.0 within 1e-12 dimensionless; usage equals 0.5 within 1e-12 dimensionless | Scratch hand calculation from squared thickness dependence and a direct applied/allowable ratio. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L125-L128] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L544-L548] [tests/test_plate_capacity.py:L329-L335] |
| C5 | Match the BASE and AFTER test summaries, record the code pin and SHA-256 of the two package Python sources, and resolve all pack citations; retain records alongside any report | 16 matching passing node IDs; 0 source digest differences; 0 unresolved citation ranges; count unit: nodes/files/ranges | Scratch outcome, digest and citation records; provenance tests remain additional regression evidence. [tests/test_plate_buckling_units.py:L216-L264] [tests/test_plate_buckling_units.py:L284-L318] |

Reproduce C0b and C2 offline with this snippet, using the installed project environment and `PYTHONPATH=src`:

```python
from digitalmodel.infrastructure.calculations.plate_buckling import (
    ElasticBucklingCalculator, UsageFactorCalculator,
)
c = ElasticBucklingCalculator()
p = [210e9, 0.3, 0.012, 0.8]  # raw test fixture: Pa, 1, m, m
rejected = 0
for i, value in enumerate([0, 0.5, 0, 0]):
    q = p.copy()
    q[i] = value
    try:
        c.calculate_base_factor(*q)
    except ValueError:
        rejected += 1
assert rejected == 4
b = c.calculate_base_factor(*p)
q = p.copy()
q[2] *= 2
assert abs(c.calculate_base_factor(*q) / b - 4) <= 1e-12
assert abs(UsageFactorCalculator.calculate_usage_factor(100e6, 200e6) - 0.5) <= 1e-12
```

## 6. Known failure modes and pitfalls

Silent-failure sweep: inspect every package Python file at the code pin; candidate sites below include benign initialisations and demonstrations as well as unguarded defaults. Dates in this section denote observation, **2026-11-07**, rather than incident dates.

Reproduction commands (case-sensitive):

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|\.get\(|\b(float|int)\(|max\(0|psi|# in|@unit_checked|= [0-9]+\.[0-9]+' src/digitalmodel/infrastructure/calculations --glob '*.py'
rg -n 'cache|write|open\(|except|continue|return None|return \[\]|return \{\}' src/digitalmodel/infrastructure/calculations --glob '*.py'
```

sweep: 88 found (candidate sites: 77 grep hits plus 11 manually inspected default/unit/initialisation sites).

| Site | Symptom or classification | Detection and follow-up |
| --- | --- | --- |
| S01 | Plate import failure becomes an unavailable flag and an ImportWarning; package import can still succeed. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/__init__.py:L20-L20] |
| S02 | Missing pipe or stress module import is suppressed without a warning. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/__init__.py:L31-L31] |
| S03 | Empty export list is a benign initialisation, but can remain empty after a failed plate import. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/__init__.py:L35-L35] |
| S04 | Coefficient dataclass field has a fixed default; standard edition and coefficient qualification remain open. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L44-L44] |
| S05 | Coefficient dataclass field has a fixed default; standard edition and coefficient qualification remain open. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L45-L45] |
| S06 | Coefficient dataclass field has a fixed default; standard edition and coefficient qualification remain open. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L46-L46] |
| S07 | Coefficient dataclass field has a fixed default; standard edition and coefficient qualification remain open. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L49-L49] |
| S08 | Coefficient dataclass field has a fixed default; standard edition and coefficient qualification remain open. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L50-L50] |
| S09 | Coefficient dataclass field has a fixed default; standard edition and coefficient qualification remain open. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L51-L51] |
| S10 | Unsupported boundary condition substitutes the simply supported default coefficient. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L70-L70] |
| S11 | Unsupported boundary condition substitutes the simply supported default coefficient. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L88-L88] |
| S12 | Decorator assumes SI stress/length inputs for raw floats; tracked quantities carry conversion metadata. | C0a; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L103-L103] |
| S13 | Explicit positive-input guard raises ValueError; this is a rejection path, not a silent fallback. | C0b; none; reviewed guard. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L118-L118] |
| S14 | Explicit positive-input guard raises ValueError; this is a rejection path, not a silent fallback. | C0b; none; reviewed guard. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L122-L122] |
| S15 | Decorator assumes SI stress/length inputs for raw floats; tracked quantities carry conversion metadata. | C0a; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L130-L130] |
| S16 | Simply supported default boundary is implicit unless the caller supplies the edge argument. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L137-L137] |
| S17 | Unsupported boundary condition substitutes the simply supported default coefficient. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L159-L159] |
| S18 | Decorator assumes SI stress/length inputs for raw floats; tracked quantities carry conversion metadata. | C0a; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L166-L166] |
| S19 | Simply supported default boundary is implicit unless the caller supplies the edge argument. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L173-L173] |
| S20 | Decorator assumes SI stress/length inputs for raw floats; tracked quantities carry conversion metadata. | C0a; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L197-L197] |
| S21 | Simply supported default boundary is implicit unless the caller supplies the edge argument. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L204-L204] |
| S22 | Simply supported default boundary is implicit unless the caller supplies the edge argument. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L234-L234] |
| S23 | Nonpositive elastic stress returns zero slenderness after logging a warning (return at line 282). | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L280-L280] |
| S24 | Interaction exponent has an implicit fixed default; no range or finiteness check. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L292-L292] |
| S25 | Nonpositive equivalent stress or yield returns zero equivalent slenderness (return at line 307). | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L306-L306] |
| S26 | Missing applied directional stress becomes zero; its sign is then discarded. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L310-L310] |
| S27 | Missing applied directional stress becomes zero; its sign is then discarded. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L311-L311] |
| S28 | Missing applied directional stress becomes zero; its sign is then discarded. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L312-L312] |
| S29 | Missing elastic directional stress becomes unity in an unstated stress unit. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L314-L314] |
| S30 | Missing elastic directional stress becomes unity in an unstated stress unit. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L315-L315] |
| S31 | Missing elastic directional stress becomes unity in an unstated stress unit. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L316-L316] |
| S32 | Zero interaction accumulator is a benign initialisation; invalid directional terms can be omitted later. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L320-L320] |
| S33 | Nonpositive accumulated interaction produces zero equivalent slenderness; invalid directions may have been skipped by positivity guards. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L332-L332] |
| S34 | Nonpositive yield is explicitly rejected, but this resistance guard lacks an active test. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L358-L358] |
| S35 | Nonpositive slenderness, including a negative value, returns full yield resistance. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L361-L361] |
| S36 | Nonpositive yield is explicitly rejected, but this resistance guard lacks an active test. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L380-L380] |
| S37 | Nonpositive slenderness, including a negative value, returns full yield resistance. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L383-L383] |
| S38 | Material factor has an implicit code default and no positivity guard; reference qualification remains open. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L400-L400] |
| S39 | Fixed coefficient or reduction-branch constant; reviewed as arithmetic, with standard provenance and independent branch checks unresolved. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L415-L415] |
| S40 | Fixed coefficient or reduction-branch constant; reviewed as arithmetic, with standard provenance and independent branch checks unresolved. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L417-L417] |
| S41 | Fixed coefficient or reduction-branch constant; reviewed as arithmetic, with standard provenance and independent branch checks unresolved. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L419-L419] |
| S42 | Transverse buckling factor has an implicit code default with no reference or range check. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L433-L433] |
| S43 | Omitted pressure becomes zero, disabling pressure reduction. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L434-L434] |
| S44 | Material factor has an implicit code default and no positivity guard; reference qualification remains open. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L435-L435] |
| S45 | Fixed coefficient or reduction-branch constant; reviewed as arithmetic, with standard provenance and independent branch checks unresolved. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L453-L453] |
| S46 | Fixed coefficient or reduction-branch constant; reviewed as arithmetic, with standard provenance and independent branch checks unresolved. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L455-L455] |
| S47 | Fixed coefficient or reduction-branch constant; reviewed as arithmetic, with standard provenance and independent branch checks unresolved. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L456-L456] |
| S48 | Fixed coefficient or reduction-branch constant; reviewed as arithmetic, with standard provenance and independent branch checks unresolved. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L458-L458] |
| S49 | Pressure multiplier starts at unity; zero or negative pressure keeps it unchanged. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L464-L464] |
| S50 | Pressure geometry term is clipped at zero; applicability and pressure sign are not validated. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L466-L466] |
| S51 | Negative pressure reduction is clipped to zero without an invalid-input error. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L470-L470] |
| S52 | Material factor has an implicit code default and no positivity guard; reference qualification remains open. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L485-L485] |
| S53 | Fixed coefficient or reduction-branch constant; reviewed as arithmetic, with standard provenance and independent branch checks unresolved. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L505-L505] |
| S54 | Fixed coefficient or reduction-branch constant; reviewed as arithmetic, with standard provenance and independent branch checks unresolved. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L507-L507] |
| S55 | Fixed coefficient or reduction-branch constant; reviewed as arithmetic, with standard provenance and independent branch checks unresolved. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L510-L510] |
| S56 | Fixed coefficient or reduction-branch constant; reviewed as arithmetic, with standard provenance and independent branch checks unresolved. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L513-L513] |
| S57 | Fixed coefficient or reduction-branch constant; reviewed as arithmetic, with standard provenance and independent branch checks unresolved. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L514-L514] |
| S58 | Fixed coefficient or reduction-branch constant; reviewed as arithmetic, with standard provenance and independent branch checks unresolved. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L515-L515] |
| S59 | Fixed coefficient or reduction-branch constant; reviewed as arithmetic, with standard provenance and independent branch checks unresolved. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L518-L518] |
| S60 | Nonpositive allowable selects an infinity-or-zero return instead of rejecting the input. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L544-L544] |
| S61 | For nonpositive allowable, positive applied stress returns infinity; zero or negative applied stress returns zero. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L546-L546] |
| S62 | Interaction factor has an implicit fixed default and no input-range check. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L553-L553] |
| S63 | Any nonpositive design resistance selects an infinity return. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L569-L569] |
| S64 | Infinity construction is numeric, not conversion of external text; nonfinite output needs caller rejection. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L571-L571] |
| S65 | Negative biaxial interaction radicand is clipped to zero, which can hide an unsupported interaction factor. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L578-L578] |
| S66 | Interaction factor has an implicit fixed default and no input-range check. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L584-L584] |
| S67 | Missing applied directional stress silently becomes zero in the aggregate usage calculation. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L596-L596] |
| S68 | Missing applied directional stress silently becomes zero in the aggregate usage calculation. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L597-L597] |
| S69 | Missing applied directional stress silently becomes zero in the aggregate usage calculation. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L598-L598] |
| S70 | Missing design directional resistance silently becomes unity in the aggregate usage calculation. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L600-L600] |
| S71 | Missing design directional resistance silently becomes unity in the aggregate usage calculation. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L601-L601] |
| S72 | Missing design directional resistance silently becomes unity in the aggregate usage calculation. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L602-L602] |
| S73 | Legacy dictionary silently supplies modulus (psi) from a built-in default; no unit conversion or required-key check. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L627-L627] |
| S74 | Legacy dictionary silently supplies Poisson ratio from a built-in default; no unit conversion or required-key check. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L628-L628] |
| S75 | Legacy dictionary silently supplies edge coefficient from a built-in default; no unit conversion or required-key check. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L629-L629] |
| S76 | Legacy dictionary silently supplies thickness (inches) from a built-in default; no unit conversion or required-key check. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L630-L630] |
| S77 | Legacy dictionary silently supplies breadth (inches) from a built-in default; no unit conversion or required-key check. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L631-L631] |
| S78 | Legacy dictionary silently supplies length (inches) from a built-in default; no unit conversion or required-key check. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L632-L632] |
| S79 | Legacy dictionary silently supplies tangent modulus (psi) from a built-in default; no unit conversion or required-key check. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L633-L633] |
| S80 | Legacy dictionary silently supplies yield (psi; read but unused) from a built-in default; no unit conversion or required-key check. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L634-L634] |
| S81 | Legacy shear coefficient depends on square root of absolute length; consistent length-unit rescaling changes the coefficient. | no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L645-L645] |
| S82 | Main-block demonstration literal with SI convention; illustrative input, not an independently validated expected value. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L669-L669] |
| S83 | Main-block demonstration literal with SI convention; illustrative input, not an independently validated expected value. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L670-L670] |
| S84 | Main-block demonstration literal with SI convention; illustrative input, not an independently validated expected value. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L671-L671] |
| S85 | Main-block demonstration literal with SI convention; illustrative input, not an independently validated expected value. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L672-L672] |
| S86 | Main-block demonstration literal with SI convention; illustrative input, not an independently validated expected value. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L673-L673] |
| S87 | Main-block demonstration literal with SI convention; illustrative input, not an independently validated expected value. | no detection check exists; issue pending (1). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L684-L684] |
| S88 | Empty slenderness dictionary is a benign main-block accumulator, filled by the subsequent loop. | no detection check exists; none; reviewed initialisation. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L687-L687] |

No external-text numeric conversion, cache write, record-skipping `continue`, error-path empty return or `return None` occurs in the two inspected Python files; the two `float` calls construct infinity rather than parse text. [src/digitalmodel/infrastructure/calculations/__init__.py:L1-L43] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L1-L699]

Additional coverage findings: the legacy capacity test import guard prevents the slenderness, resistance and usage tests from running; no detection check exists for restoring that coverage (#949). The migration document's validation and readiness claims therefore remain U2 rather than evidence. [tests/test_plate_capacity.py:L23-L40] [tests/test_plate_capacity.py:L211-L366] [docs/domains/structural/plate_capacity_migration.md:L260-L291]

## 7. Lessons log

Append new dated rows; preserve earlier evidence and record any changed standard edition explicitly.

| Date | Entry | Lesson and pack change | Evidence | Shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-11-07 | Initial pack | Distinguish declared boundary choices from implemented solutions; sections 1, 2 and 6 expose the fallback paths. | [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L24-L29] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L65-L88] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L154-L161] | Not assigned; maintain with input-contract work, issue pending (2). |
| 2026-11-07 | Initial pack | Separate unit regression from numerical qualification; section 4 labels comparisons as regression. | [tests/test_plate_buckling_units.py:L62-L177] | Not assigned; maintain with qualification work, issue pending (1). |

## 8. Ownership

Review assignments for this pack: engineering calculations family quality manager; deputy role: deputy engineering calculations quality manager; producing role: engineering calculations contributor. The maintainer assigns people to these roles; this table appoints no individual.

Escalate an ambiguous standard clause, missing basis, tolerance change or a failed calculation check to the family quality manager. Require a reviewer from a different provider before clearance (AQG r0, *Analysis QA gates*, revision 0, section 11). Keep the pack DRAFT until that review completes.

## 9. Dependencies and implementation inventory

| Relationship | Dependency or consumer |
| --- | --- |
| Upstream runtime | Python typing, dataclasses, math, logging and enum; `assetutilities.units.unit_checked` supplies unit enforcement. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L13-L19] |
| Optional package imports | `pipe_properties` and `stress_calculations` are attempted and their import failure is suppressed. [src/digitalmodel/infrastructure/calculations/__init__.py:L25-L33] |
| Test dependency | `assetutilities.units.TrackedQuantity`, Pint's dimensionality exception and pytest; active fixtures provide raw and tracked SI values. [tests/test_plate_buckling_units.py:L16-L56] |
| Demonstration consumer | Component and legacy examples import this package; the same script imports structural capacity names that differ from the current capacity module's declarations. [examples/domains/stress/plate_capacity_examples.py:L19-L29] [src/digitalmodel/structural/structural_analysis/capacity.py:L9-L17] |
| Internal helper relationship | The aggregate elastic and usage methods delegate to their directional methods; they are helpers rather than independent implementations. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L249-L256] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L604-L612] |

### Map inventory and A8

Map reference: `origin/codex/implmap-work`, commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; map data revision `270c8759c374b67a7f032953e7fcdb14e0224461` (the data revision is also retained in the default-branch map header). [docs/registry/implementation-map.yaml:L1-L3]

A8 measurement: in-package implementation rows **0**, module map families **0**, counted families **0**; section-9 map inventory is empty; counted-family grep **NOT_APPLICABLE**, because no map family has an implementation row in the package. The measurement uses the map reference above, rather than replacing it with a later default-branch inventory.

Reproduce the inventory against the pinned map, using PyYAML and Python `re` for any future counted-family grep; the current inventory requires no signal filtering or enclosing-definition assignment because the counted set is empty:

```python
import subprocess
import yaml
map_pin = "3256bab95f84da3d9fadce8d5dcf8458d66d1eb4"
text = subprocess.check_output([
    "git", "show", map_pin + ":docs/registry/implementation-map.yaml"
], text=True)
map_data = yaml.safe_load(text)
prefix = "src/digitalmodel/infrastructure/calculations/"
families = [
    (family["id"], [row for row in family["implementations"]
                    if row["path"].startswith(prefix)])
    for family in map_data["families"]
]
ranked = sorted((item for item in families if item[1]),
                key=lambda item: -len(item[1]))  # stable map order on ties
assert len(ranked) == 0
assert sum(len(rows) for _, rows in ranked) == 0
```

Map follow-up: add elastic plate buckling, reduced/equivalent slenderness, characteristic resistance, DNV-labelled directional design resistance, usage/interaction and legacy plate buckling families; these are the section-3 families, absent from the counted package inventory (issue pending (3)). [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L54-L657]

### Repository-wide discovery, including scripts

Discovery command:

```sh
rg -n 'ElasticBucklingCalculator|SlendernessCalculator|UltimateStrengthCalculator|UsageFactorCalculator|calculate_plate_buckling_212|def .*buckling|def .*slenderness' . --glob '*.py' --glob '!agent-pack.md'
```

These findings support the map follow-up and do not change the empty counted-family A8 result.

| Definition or route | Classification and evidence |
| --- | --- |
| In-package legacy `calculate_plate_buckling_212` | Further formula route already inventoried in section 3; independently computes its expressions. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L616-L657] |
| `structural/structural_analysis/buckling.py`: `PlateBucklingAnalyzer.elastic_buckling_stress`, `reduced_slenderness`, `johnson_ostenfeld`, `check_plate_buckling` | Separate formula implementation outside the package; map follow-up, not a package miss. [src/digitalmodel/structural/structural_analysis/buckling.py:L26-L148] |
| `infrastructure/base_solvers/structural/plate_buckling.py`: `PlateBuckling` | Separate engine formula route, including elastic resistance, reduced slenderness, characteristic resistance and directional design stresses; outside-package map follow-up. [src/digitalmodel/infrastructure/base_solvers/structural/plate_buckling.py:L214-L342] [src/digitalmodel/infrastructure/base_solvers/structural/plate_buckling.py:L702-L820] |
| `structural/plate_capacity/elastic_buckling.py`: `square_plate_elastic_buckling_stress` | Separate square-plate formula kernel; external to package scope. [src/digitalmodel/structural/plate_capacity/elastic_buckling.py:L15-L55] |
| `structural/plate_capacity/classical_elastic_buckling.py`: `plate_buckling_coefficient`, `rectangular_plate_elastic_buckling_stress` | Computes the modal coefficient itself; stress method scales and delegates to the square kernel. [src/digitalmodel/structural/plate_capacity/classical_elastic_buckling.py:L66-L90] [src/digitalmodel/structural/plate_capacity/classical_elastic_buckling.py:L93-L121] |
| `scripts/studies/ship_plate_buckling.py`: `_reference_row` | Computes a modal minimisation and study ratios itself while delegating the square stress scale; outside-package formula route. [scripts/studies/ship_plate_buckling.py:L213-L264] |
| `scripts/studies/generate_classical_plate_buckling.py`: `write_coefficient_csv` | Delegating export helper; not an independent coefficient implementation. [scripts/studies/generate_classical_plate_buckling.py:L16-L36] |
| `infrastructure/common/plate_buckling.py` | Deprecated re-export shim for the engine implementation; not an independent calculation. [src/digitalmodel/infrastructure/common/plate_buckling.py:L6-L19] |

Do not substitute one route as a hand reference for another without reconciling units, boundary conditions, validity domains and standard editions; retain the route reconciliation in issue pending (3).

## 10. Coverage and open items

### Reading inventory

| Coverage | Files and result |
| --- | --- |
| Whole package | Both Python files, including every class, helper and main block; no Python subpackage is excluded. [src/digitalmodel/infrastructure/calculations/__init__.py:L1-L43] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L1-L699] |
| Module documentation discovery | `rg -l 'infrastructure[./]calculations|digitalmodel\.calculations' docs --glob '*.md'` found the four documents listed below. |
| Documentation read | `docs/domains/infrastructure/calculations/README.md` is a placeholder; no public API contract is supplied there. [docs/domains/infrastructure/calculations/README.md:L1-L17] |
| Documentation read | `docs/domains/structural/plate_capacity_migration.md` describes old import paths, example calls and unqualified validation claims. [docs/domains/structural/plate_capacity_migration.md:L22-L38] [docs/domains/structural/plate_capacity_migration.md:L142-L169] [docs/domains/structural/plate_capacity_migration.md:L260-L291] |
| Documentation read | `docs/domains/structural/api_std_2rd_migration_summary.md` names the attempted pipe/stress calculation modules; those modules are not exported by the package's actual guarded import path. [docs/domains/structural/api_std_2rd_migration_summary.md:L11-L17] [src/digitalmodel/infrastructure/calculations/__init__.py:L25-L43] |
| Documentation read | `docs/domains/plate-buckling/buckling-code-review-2026-06-26.md` distinguishes the engine, structural and unit-aware calculation routes; its historical test counts are not adopted. [docs/domains/plate-buckling/buckling-code-review-2026-06-26.md:L10-L19] |
| Tests read | `tests/test_plate_buckling_units.py` and `tests/test_plate_capacity.py`; active suite defines 16 tests; historical capacity tests are blocked by a module import skip. [tests/test_plate_buckling_units.py:L62-L318] [tests/test_plate_capacity.py:L23-L40] |
| Other evidence | Routing registry, the section-9 formula/export paths, component demonstration and hook configuration; the proposed work concerns source qualification and input contracts. [docs/registry/module-routing.yaml:L212-L221] [examples/domains/stress/plate_capacity_examples.py:L19-L29] |

Active test inventory (all under `tests/test_plate_buckling_units.py`):

| Class | Methods | Source |
| --- | --- | --- |
| `TestTrackedMatchesRaw` | `test_base_factor_matches`, `test_longitudinal_stress_matches`, `test_transverse_stress_matches`, `test_shear_stress_matches` | [tests/test_plate_buckling_units.py:L62-L130] |
| `TestAutoConversion` | `test_psi_converts_to_pa`, `test_mm_converts_to_m` | [tests/test_plate_buckling_units.py:L138-L177] |
| `TestDimensionalityError` | `test_meters_where_pa_expected`, `test_pa_where_meters_expected` | [tests/test_plate_buckling_units.py:L185-L208] |
| `TestProvenance` | `test_output_has_provenance`, `test_provenance_includes_input_creation`, `test_provenance_includes_function_source`, `test_conversion_recorded_in_provenance`, `test_base_factor_no_return_tracking` | [tests/test_plate_buckling_units.py:L216-L276] |
| `TestOutputUnit` | `test_longitudinal_returns_pa`, `test_transverse_returns_pa`, `test_shear_returns_pa` | [tests/test_plate_buckling_units.py:L284-L318] |

### Follow-up register

Assign follow-up implementation to the engineering calculations maintainer and review to the section-8 quality role; draft references below require issue creation before final clearance.

| Item | Remaining work and evidence | Tracking |
| --- | --- | --- |
| U1 | Qualify DNV-RP-C201 edition, clauses, API designation and calculation-output Citation sidecars; resolve aspect-ratio documentation; no licensed clause text was checked. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L54-L68] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L188-L192] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L395-L521] | issue pending (1) |
| U2 | Supply independently sourced expected values and test node IDs for resistance branches and worked examples; reconcile migration validation claims, placeholder documentation and the demonstration's structural imports; the external example imports names not declared by the present capacity module. [docs/domains/structural/plate_capacity_migration.md:L260-L291] [docs/domains/infrastructure/calculations/README.md:L5-L17] [examples/domains/stress/plate_capacity_examples.py:L19-L29] [src/digitalmodel/structural/structural_analysis/capacity.py:L9-L17] | issue pending (1) |
| U3 | Establish coordinate/sign conventions and input-domain contracts; qualify every section-6 fallback, clipping branch and unit assumption before design use. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L310-L334] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L532-L657] | issue pending (2) |
| Coverage debt | Migrate the historical capacity tests to current imports and recover slenderness, resistance, usage and legacy value checks; no code or test workaround is included here. [tests/test_plate_capacity.py:L23-L40] [tests/test_plate_capacity.py:L211-L366] | #949 |
| Map coverage | Add section-3 method families and reconcile the section-9 discovery routes before a later map revision counts them; inventory is currently empty for this package. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L54-L657] | issue pending (3) |

Private sidecar: no entry is needed for this pack's test-fixture and implementation descriptions; no client-derived numerical examples are reproduced. Retain that restriction when adding future examples.
