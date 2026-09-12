# Plan for #1582: Normalize diffraction matrix units across AQWA and OrcaWave backends

> **Status:** draft
> **Complexity:** T3
> **Date:** 2026-09-12
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1582
> **Client:** N/A
> **Lane:** lane:codex
> **Review artifacts:** scripts/review/results/2026-09-12-plan-1582-claude.md | ...-codex.md | ...-gemini.md

---

## Resource Intelligence Summary

### Existing repo code

- **Found:** `src/digitalmodel/hydrodynamics/diffraction/output_schemas.py:27-50` — `Unit` enum declares nine
  coefficient unit strings, including `ADDED_MASS_LINEAR = "kg"`, `ADDED_MASS_ANGULAR = "kg.m"`,
  `ADDED_MASS_ROTATIONAL = "kg.m^2"`, `DAMPING_LINEAR = "N.s/m"`, `DAMPING_COUPLING = "N.s/rad"`,
  `DAMPING_ANGULAR = "N.m.s/rad"`. The enum names the three *blocks*; it does not bind a unit to an
  individual `(i, j)` cell, and it carries no scale factor to or from any solver's native basis.
- **Found:** `output_schemas.py:67-77` — canonical `ADDED_MASS_UNITS` / `DAMPING_UNITS` dicts keyed
  `{"linear", "coupling", "angular"}`. These were introduced by PR #1636 (see Documents consulted) and are
  consumed today by exactly one producer.
- **Found:** `output_schemas.py:222-245` — `HydrodynamicMatrix` carries four fields only:
  `matrix`, `frequency`, `matrix_type`, `units`. It has no reference point, no DOF ordering, no sign
  convention, and no conversion revision. `__post_init__` (`:230-232`) asserts shape `(6, 6)` and nothing else.
- **Found:** `output_schemas.py:538-575` — `validate_matrix_set()` checks count-vs-frequency agreement,
  symmetry, NaN/Inf, and frequency ordering. It reads `matrix.units` **not at all**. This is the mechanism
  by which dimensionally incompatible matrices pass.
- **Gap (5 competing unit-key conventions across producers, verified below):**

  | Producer | `units` key set | Site |
  |---|---|---|
  | canonical (#1636) | `linear` / `coupling` / `angular` | `output_schemas.py:67-77` |
  | `orcawave_runner` | canonical (imports the dicts) | `orcawave_runner.py:765-766` |
  | `aqwa_converter` | `linear-linear` / `linear-angular` / `angular-angular` | `aqwa_converter.py:388-403` |
  | `solver/orcawave_converter` | `linear-linear` / `linear-angular` / `angular-angular` | `solver/orcawave_converter.py:321-334` |
  | `wamit_reference_loader` | `linear` / `coupled` / `rotational` | `wamit_reference_loader.py:644-656` |
  | `orcaflex_exporter` | no dict — a per-`(i, j)` function | `orcaflex_exporter.py:445-465` |

  The single consumer, `polars_exporter._build_matrix_records`, derives `linear` / `angular` / `coupling`
  from `(i, j)` at `polars_exporter.py:161-167` and resolves it with `matrix.units.get(unit_key, "")` at
  `:168` — a silent empty-string default.
- **Gap:** `aqwa_lis_parser.AQWALISParser.parse_added_mass_table` (`aqwa_lis_parser.py:150-209`) and
  `parse_damping_table` (`:211-269`) both return `Dict[float, np.ndarray]`. Neither reads an AQWA units
  declaration, and neither attaches any unit to what it returns. The native basis of an AQWA `.LIS` matrix
  is therefore asserted nowhere in the parse path.
- **Gap:** `aqwa_ah1_parser` reads `water_density` from the `.AH1` `GENERAL` record
  (`aqwa_ah1_parser.py:315-322`) with a default of `1025.0` (`:89`) and never range-checks it. A value near
  `1.025` would indicate a tonne-basis deck; nothing in the parser distinguishes the two.
- **Gap:** `solver/orcawave_data_extraction.py:217-244` assembles the 6x6 added-mass matrix by assigning
  each off-diagonal cell to both mirror positions (`matrix[0, 3] = matrix[3, 0] = entry.AddedMassXRx` and
  fourteen siblings). The linear-angular block and the angular-linear block are, by construction, the same
  stored number; no code path can currently attach a different unit to each.
- **Gap:** `HydrostaticResults` (`output_schemas.py:334-355`) carries `displacement_volume` (m^3) and
  `mass` (kg) as independent floats with no consistency relation between them and no density field.

### Standards

Not applicable at the constant level. This issue defines an internal dimensional contract and introduces no
standards-derived numeric constant, so no `Citation` sidecar per `.claude/rules/calc-citation-contract.md`
is required. Seawater density used in test fixtures will be a fixture parameter, not a shipped constant.

### LLM Wiki pages consulted

No relevant wiki pages. The subject is an internal type contract in a public repository; no wiki content is
read or written, which is why `Client:` is `N/A`.

### Documents consulted

- **PR [#1636](https://github.com/vamseeachanta/digitalmodel/pull/1636)** — MERGED 2026-07-26T23:40:51Z,
  merge commit `4d465406`, titled *"fix(diffraction): convert OrcFxAPI tonne values to kg at the results
  boundary (#1550)"*. Ten files: six modified, four test files added. Its own body enumerates what it
  deliberately excluded, including **W4 — "converging the five competing unit-shape conventions"**, which it
  states "is a cross-module refactor and deserves its own review." That excluded remainder is the scope of
  this plan.
- **Issue [#1550](https://github.com/vamseeachanta/digitalmodel/issues/1550)** — CLOSED. The focused
  OrcaWave labeling defect named in this issue's Boundaries. No part of it is re-opened here.
- **Issue [#1579](https://github.com/vamseeachanta/digitalmodel/issues/1579)** — OPEN, `status:needs-plan`,
  *"Make diffraction inertia-tensor origin strict and consistent across backends"*. Owns reference-point
  semantics that this issue's provenance field will record.
- **Issue [#1580](https://github.com/vamseeachanta/digitalmodel/issues/1580)** — OPEN, `status:needs-plan`,
  *"Define a solver-neutral diffraction restoring-stiffness contract"*. Owns restoring composition.
- **Issue [#1825](https://github.com/vamseeachanta/digitalmodel/issues/1825)** — OPEN epic,
  *"[Epic] Diffraction results agree across solvers — one strict contract, three-way benchmarks"*; the
  parent of #1582.
- **`tests/hydrodynamics/diffraction/test_matrix_unit_completeness.py:1-30`** — the module docstring landed
  by #1636 already enumerates the five conventions and records that the `aqwa_converter` /
  `solver.orcawave_converter` shape has "NO key the exporter looks for; all 36 cells export with unit ''".
  Its assertions, however, are built exclusively on an `OrcaWaveRunner` fixture (`:64-71`), so the AQWA
  path it describes is asserted nowhere.
- **`docs/plans/`** — 147 plan files present; `grep -l '1550\|1582\|1579\|1580'` returns only
  `2026-08-03-issue-1633-ship-benchmark-verdict.md` (an incidental mention of #1550's scale-invariance
  finding). No prior plan addresses matrix unit normalization. This plan is the first.
- **`src/.../wamit_reference_loader.py:53-66`** — `_NONDIM_EXPONENT`, a 6x6 integer array giving the WAMIT
  length exponent per `(i, j)` (3 / 4 / 5 across the four blocks). This is existing, working prior art for
  a per-cell dimensional table and will be the structural model for the new canonical table rather than a
  second independent invention.

### Gaps identified

- No per-cell dimensional contract exists. Every unit today is a block-level string, so the 18 mirror cells
  of the coupling block cannot be labelled independently even in principle.
- No native-to-canonical scale factor is declared for any AQWA term. The AQWA native basis is not recorded
  at parse time at all.
- No fail-closed rejection exists for an unknown unit key, a missing reference point, a transposed DOF
  ordering, or a mixed-term scale error. All four pass today.
- No round-trip carries native values or conversion provenance — `to_dict()` emits normalized values only.
- No dimensional identity (mass = displacement volume x density) is asserted anywhere.

### Evidence (embedded verification)

**Issue statuses** (verified 2026-09-12 via `gh issue view`, repo `vamseeachanta/digitalmodel`):

- `#1582` — OPEN — "Normalize diffraction matrix units across AQWA and OrcaWave backends"; labels
  `cat:engineering, domain:hydro, lane:codex, machine:licensed-win-1, status:needs-plan`; parent `#1825`.
- `#1550` — CLOSED — "Added-mass/damping units inconsistency in DiffractionResults: matrix labeled kg/kg·m²
  but magnitudes consistent with te/te·m² (~1000× off)"
- `#1579` — OPEN — "Make diffraction inertia-tensor origin strict and consistent across backends"
- `#1580` — OPEN — "Define a solver-neutral diffraction restoring-stiffness contract"
- `#1633` — OPEN, `status:plan-approved` — "bug(validation): OrcaWave benchmark suite compares OrcaWave to
  itself…"
- `#1825` — OPEN — "[Epic] Diffraction results agree across solvers — one strict contract, three-way
  benchmarks"
- `#1447` — CLOSED — "Add repo-level units regression test (kN<->N factor-1000 trap)"

**PR state** (`gh pr view 1636 --json state,mergedAt,mergeCommit`):

```
state       MERGED
mergedAt    2026-07-26T23:40:51Z
mergeCommit 4d4654066dc96d33e9319517ae28ffe69021bca9
```

`git log -S "ADDED_MASS_UNITS" --oneline -- src/digitalmodel/hydrodynamics/diffraction/output_schemas.py`
returns exactly one commit, `4d465406` — confirming the canonical dicts arrived with #1636 and nothing since.

**File existence** (2026-09-12):

- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/output_schemas.py`
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/diffraction_units.py`
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py`
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/aqwa_ah1_parser.py`
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py`
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/polars_exporter.py`
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py`
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/wamit_reference_loader.py`
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_data_extraction.py`
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_converter.py`
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py`
- EXISTS: `tests/hydrodynamics/diffraction/fixtures/`
- MISSING (new — this plan will create): `src/digitalmodel/hydrodynamics/diffraction/matrix_units.py`
- MISSING (new — this plan will create): `tests/hydrodynamics/diffraction/test_matrix_unit_contract.py`
- MISSING (new — this plan will create): `tests/hydrodynamics/diffraction/test_cross_backend_normalization.py`
- MISSING (new — this plan will create): `tests/hydrodynamics/diffraction/test_matrix_unit_provenance.py`
- MISSING (new — this plan will create): `tests/hydrodynamics/diffraction/test_dimensional_identities.py`

**Line excerpts** (reviewers: verify the line numbers match).

`sed -n 27,50p output_schemas.py` — the block-level unit vocabulary that exists today:

```
27	class Unit(Enum):
28	    """Standard units for hydrodynamic coefficients"""
...
33	    # Added mass units
34	    ADDED_MASS_LINEAR = "kg"          # Linear-linear coupling
35	    ADDED_MASS_ANGULAR = "kg.m"       # Linear-angular coupling
36	    ADDED_MASS_ROTATIONAL = "kg.m^2"  # Angular-angular coupling
37	
38	    # Damping units
39	    DAMPING_LINEAR = "N.s/m"          # Linear damping
40	    DAMPING_COUPLING = "N.s/rad"      # Linear-angular coupling
41	    DAMPING_ANGULAR = "N.m.s/rad"     # Angular damping
```

`sed -n 67,77p output_schemas.py` — the canonical dicts from #1636, three keys for four blocks:

```
67	ADDED_MASS_UNITS: Dict[str, str] = {
68	    "linear": Unit.ADDED_MASS_LINEAR.value,        # kg
69	    "coupling": Unit.ADDED_MASS_ANGULAR.value,     # kg.m
70	    "angular": Unit.ADDED_MASS_ROTATIONAL.value,   # kg.m^2
71	}
72	
73	DAMPING_UNITS: Dict[str, str] = {
74	    "linear": Unit.DAMPING_LINEAR.value,           # N.s/m
75	    "coupling": Unit.DAMPING_COUPLING.value,       # N.s/rad
76	    "angular": Unit.DAMPING_ANGULAR.value,         # N.m.s/rad
77	}
```

`sed -n 222,232p output_schemas.py` — the container whose only invariant is shape:

```
222	@dataclass
223	class HydrodynamicMatrix:
224	    """6x6 hydrodynamic coefficient matrix (added mass or damping)"""
225	    matrix: np.ndarray              # 6x6 matrix
226	    frequency: float                # Frequency at which computed (rad/s)
227	    matrix_type: str                # "added_mass" or "damping"
228	    units: Dict[str, str]           # Units for each coupling type
229	
230	    def __post_init__(self):
231	        """Validate matrix dimensions"""
232	        assert self.matrix.shape == (6, 6), f"Matrix must be 6x6, got {self.matrix.shape}"
```

`sed -n 161,168p polars_exporter.py` — the silent default at the consumer seam:

```
161	                    unit_key = (
162	                        "linear"
163	                        if i < 3 and j < 3
164	                        else "angular"
165	                        if i >= 3 and j >= 3
166	                        else "coupling"
167	                    )
168	                    unit = matrix.units.get(unit_key, "")
```

`sed -n 388,403p aqwa_converter.py` — the AQWA key set, and an entry that is not a unit at all:

```
388	    def _get_added_mass_units() -> Dict[str, str]:
389	        """Get added mass unit dictionary"""
390	        return {
391	            'linear-linear': 'kg',
392	            'linear-angular': 'kg.m',
393	            'angular-angular': 'kg.m^2'
394	        }
...
397	    def _get_damping_units() -> Dict[str, str]:
398	        """Get damping unit dictionary"""
399	        return {
400	            'linear-linear': 'N.s/m',
401	            'linear-angular': 'N.s or N.m.s/rad',
402	            'angular-angular': 'N.m.s/rad'
403	        }
```

`'N.s or N.m.s/rad'` is a disjunction of two units in a field a consumer must resolve to one.

`sed -n 644,656p wamit_reference_loader.py` — a third key set, and a different label for the same block:

```
644	def _default_units(matrix_type: str) -> Dict[str, str]:
645	    """Default SI unit labels for matrix types."""
646	    if matrix_type == "added_mass":
647	        return {
648	            "linear": "kg",
649	            "coupled": "kg.m",
650	            "rotational": "kg.m^2",
651	        }
652	    return {
653	        "linear": "N.s/m",
654	        "coupled": "N.m.s/rad",
655	        "rotational": "N.m.s/rad",
656	    }
```

The damping coupling block is `N.m.s/rad` here and `N.s/rad` in `DAMPING_UNITS:75`. Two producers label the
same physical block with two different units; neither is wrong for its own mirror half, which is precisely
the defect — one string cannot label both halves.

`sed -n 744,766p orcawave_runner.py` — what #1636 landed, and the boundary it drew:

```
744	        # OrcFxAPI reports these on a tonne basis - addedMass in te / te.m /
745	        # te.m^2 and damping in te/s - while DiffractionResults is an SI/kg
746	        # type (aqwa_converter, solver.orcawave_converter and
747	        # wamit_reference_loader all populate it in kg). Convert here so this
748	        # producer agrees with its siblings; previously the values were passed
749	        # through unconverted under kg labels, making them 1000x low (#1550).
...
754	        added_mass_raw = tonnes_to_kg(
755	            np.asarray(diffraction.addedMass, dtype=float)[sort_idx]
756	        )
757	        damping_raw = tonnes_to_kg(
758	            np.asarray(diffraction.damping, dtype=float)[sort_idx]
759	        )
...
765	        am_units = dict(ADDED_MASS_UNITS)
766	        dp_units = dict(DAMPING_UNITS)
```

`sed -n 217,230p solver/orcawave_data_extraction.py` — mirror cells forced equal at extraction:

```
217	                # Diagonal terms
218	                matrix[0, 0] = entry.AddedMassX
...
225	                # Off-diagonal terms (symmetric)
226	                matrix[0, 1] = matrix[1, 0] = entry.AddedMassXY
227	                matrix[0, 2] = matrix[2, 0] = entry.AddedMassXZ
228	                matrix[0, 3] = matrix[3, 0] = entry.AddedMassXRx
229	                matrix[0, 4] = matrix[4, 0] = entry.AddedMassXRy
230	                matrix[0, 5] = matrix[5, 0] = entry.AddedMassXRz
```

`sed -n 53,66p wamit_reference_loader.py` — the per-cell exponent table this plan will generalize:

```
53	# Exponent k for WAMIT non-dimensionalization: A_ij / (rho * L^k)
54	# Row DOF i, Column DOF j. Linear DOFs (1-3) contribute 1, rotational (4-6) contribute 2.
55	# k = sum of contributions.
56	_NONDIM_EXPONENT = np.array(
57	    [
58	        [3, 3, 3, 4, 4, 4],
...
61	        [4, 4, 4, 5, 5, 5],
```

**Gap proofs**:

- `grep -n "reference_point\|dof_order\|sign_convention\|conversion_revision" src/.../diffraction/*.py` →
  `reference_point` appears only in `input_schemas.py:144,157-162`, `parametric_spec_generator.py:299`, and
  `resolver.py:118,262`. `dof_order`, `sign_convention`, and `conversion_revision` return **no hits
  anywhere**. Reference-point semantics exist on the input side and are absent from every output container.
- `dataclasses.fields(HydrodynamicMatrix)` → `['matrix', 'frequency', 'matrix_type', 'units']` — confirms
  the four-field output container.
- `dataclasses.fields(DiffractionResults)` → `[... 'phase_convention', 'unit_system']` — a single
  results-level `unit_system` string, with no per-matrix provenance beneath it.
- `grep -rn "_get_added_mass_units\|_get_damping_units" tests/` → two hits, both in
  `tests/hydrodynamics/diffraction/test_solver_orcawave_converter.py:282,290`, both asserting the
  `OrcaWaveConverter` dict's own contents. No test carries either producer's dict through
  `polars_exporter`, which is why the all-cells-blank outcome below has never failed a build.

**Reproduction proofs.** A read-only probe (written to the session scratchpad, not to the repo; no source or
test file was modified) builds two synthetic `DiffractionResults` — one labelled with the AQWA producer's
dict, one with the canonical dict and values 1000x smaller — and runs the shipped validators and exporter.
Interpreter: `D:\ws\digitalmodel\.venv\Scripts\python.exe`.

```
=== A. producer unit-dict key sets ===
canonical  ADDED_MASS_UNITS keys : ['angular', 'coupling', 'linear']
aqwa_converter._get_added_mass_units: ['angular-angular', 'linear-angular', 'linear-linear']
solver.orcawave_converter          : ['angular-angular', 'linear-angular', 'linear-linear']
aqwa_converter._get_damping_units  : {'linear-linear': 'N.s/m', 'linear-angular': 'N.s or N.m.s/rad', 'angular-angular': 'N.m.s/rad'}

=== B. do existence/shape validators reject the 1000x-apart pair? ===
AQWA-labelled                validate_matrix_set(added_mass) -> []
AQWA-labelled                validate_matrix_set(damping)    -> []
OrcaWave-labelled(1/1000)    validate_matrix_set(added_mass) -> []
OrcaWave-labelled(1/1000)    validate_matrix_set(damping)    -> []
A33 AQWA = 1000000.0  A33 OrcaWave = 1000.0  ratio = 1000.0

=== C. exported unit strings, AQWA-labelled path ===
added_mass cells=72 blank-unit cells=72
damping    cells=72 blank-unit cells=72
example blank: {'frequency': 0.4, 'period': 15.707963267948966, 'dof_i': 'SURGE', 'dof_j': 'SURGE', 'value': 1000000.0, 'unit': ''}

=== D. mirror-block labelling ===
DAMPING_UNITS = {'linear': 'N.s/m', 'coupling': 'N.s/rad', 'angular': 'N.m.s/rad'}
coupling key labels BOTH mirror blocks: N.s/rad

=== E. output-side provenance fields present on HydrodynamicMatrix? ===
['matrix', 'frequency', 'matrix_type', 'units']

=== F. transposed DOF ordering: does anything reject it? ===
validate_matrix_set after mirroring a coupling term -> []

=== G. round-trip ===
unit_system round-trips: SI
matrix units round-trip: {'linear-linear': 'kg', 'linear-angular': 'kg.m', 'angular-angular': 'kg.m^2'}
matrix dict keys: ['frequency', 'matrix', 'matrix_type', 'units']
```

- Reproduced at: 2026-09-12.
- Failure mode observed matches issue claim: **YES**. The issue states "Existence and shape checks can
  therefore pass dimensionally incompatible matrices." Block B is that claim exactly: two matrices whose
  `A33` differ by a factor of 1000 both return an empty issue list from `validate_matrix_set`, because that
  validator never reads `units`. Block C is a second, stronger instance the issue's Scope implies: on the
  AQWA producer path **72 of 72 exported cells carry `unit=''`**, so a consumer has no unit to check
  against. Block D shows the remaining structural defect — a three-key dict gives the same string to both
  mirror halves of the coupling block, though `B[SURGE, ROLL]` (force per roll rate, `N.s/rad`) and
  `B[ROLL, SURGE]` (roll moment per surge velocity, `N.m.s/m`) are differently spelt quantities. Block F
  shows a transposed coupling term surviving validation, since symmetry is the only structural check.

<!-- Source count: issue body (1), repo code across eleven modules (2), PR #1636 + its merge commit (3),
     issues #1550/#1579/#1580/#1825/#1447 (4), test_matrix_unit_completeness.py docstring (5),
     docs/plans/ survey (6), live reproduction probe (7). Count: 7. -->

---

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-09-12-issue-1582-matrix-unit-normalization.md` |
| Implementation — canonical contract | `src/digitalmodel/hydrodynamics/diffraction/matrix_units.py` |
| Implementation — container fields | `src/digitalmodel/hydrodynamics/diffraction/output_schemas.py` |
| Implementation — AQWA native declaration | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py`, `aqwa_ah1_parser.py`, `aqwa_converter.py` |
| Implementation — OrcaWave native declaration | `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_converter.py`, `solver/orcawave_data_extraction.py`, `orcawave_runner.py` |
| Implementation — consumers | `src/digitalmodel/hydrodynamics/diffraction/polars_exporter.py`, `orcaflex_exporter.py`, `wamit_reference_loader.py` |
| Tests — per-cell contract | `tests/hydrodynamics/diffraction/test_matrix_unit_contract.py` |
| Tests — cross-backend equality | `tests/hydrodynamics/diffraction/test_cross_backend_normalization.py` |
| Tests — provenance + round-trip | `tests/hydrodynamics/diffraction/test_matrix_unit_provenance.py` |
| Tests — dimensional identities | `tests/hydrodynamics/diffraction/test_dimensional_identities.py` |
| Fixtures | `tests/hydrodynamics/diffraction/fixtures/synthetic_matrices/` |
| Plan review — Claude | `scripts/review/results/2026-09-12-plan-1582-claude.md` |
| Plan review — Codex | `scripts/review/results/2026-09-12-plan-1582-codex.md` |
| Plan review — Gemini | `scripts/review/results/2026-09-12-plan-1582-gemini.md` |
| Docs update | `docs/plans/README.md` |

---

## Deliverable

A `matrix_units` module declaring, per `(i, j)` cell of the 6x6 added-mass and damping matrices, a canonical
SI dimension and a tested native-to-canonical scale factor for each backend, together with a
`MatrixUnitProvenance` record on `HydrodynamicMatrix` that carries native units, normalized units, reference
point, DOF ordering, sign convention and conversion revision through serialization — such that a matrix
whose unit basis cannot be established is rejected at parse time rather than accepted and exported blank.

---

## Pseudocode

Canonical dimension table (generalizing `wamit_reference_loader._NONDIM_EXPONENT`):

```
DIMENSION[matrix_type][i][j] = Dimension(mass_exp, length_exp, time_exp, angle_exp)

    added mass  A_ij : force_or_moment_i / acceleration_j
        i<3, j<3   M L^0 T^0 A^0      kg
        i<3, j>=3  M L^1 T^0 A^-1     kg.m/rad
        i>=3, j<3  M L^1 T^0 A^0      kg.m
        i>=3, j>=3 M L^2 T^0 A^-1     kg.m^2/rad

    damping     B_ij : force_or_moment_i / velocity_j
        i<3, j<3   M L^0 T^-1 A^0     N.s/m
        i<3, j>=3  M L^1 T^-1 A^-1    N.s/rad
        i>=3, j<3  M L^1 T^-1 A^0     N.m.s/m
        i>=3, j>=3 M L^2 T^-1 A^-1    N.m.s/rad

    The two off-diagonal blocks are NOT the same dimension. Radian is
    dimensionless in magnitude, so the two blocks share a scale factor of 1 for
    every unit system considered here; they do not share a label, and the
    contract records both.
```

```
function canonical_unit(matrix_type, i, j) -> str:
    validate matrix_type in {"added_mass", "damping"}     # else raise
    validate 0 <= i < 6 and 0 <= j < 6                    # else raise
    return DIMENSION[matrix_type][i][j].si_label()
```

```
function scale_to_canonical(matrix_type, native_basis, i, j) -> float:
    # native_basis is an explicit enum member, never inferred from magnitude
    if native_basis not in KNOWN_BASES:
        raise UnknownUnitBasisError(native_basis)   # fail closed, do not guess
    d = DIMENSION[matrix_type][i][j]
    return (native_basis.mass_in_kg ** d.mass_exp) *
           (native_basis.length_in_m ** d.length_exp) *
           (native_basis.time_in_s ** d.time_exp) *
           (native_basis.angle_in_rad ** d.angle_exp)
```

```
function normalize(matrix_6x6, matrix_type, provenance) -> (matrix_6x6, provenance)
    require provenance.native_basis is declared        # else UnknownUnitBasisError
    require provenance.reference_point is a 3-vector   # else MissingReferencePointError
    require provenance.dof_order == CANONICAL_DOF_ORDER # else DofOrderError
    out = empty 6x6
    for i, j in 6x6:
        out[i][j] = matrix[i][j] * scale_to_canonical(matrix_type, provenance.native_basis, i, j)
    return out, provenance.with(normalized_units=..., conversion_revision=REVISION,
                                native_matrix=matrix)
```

```
function validate_matrix_units(matrix_set) -> list[str]:
    # extends validate_matrix_set; reads units, which it does not do today
    for each matrix:
        for i, j in 6x6:
            declared = matrix.resolve_unit(i, j)
            if declared is missing or empty:      report "cell (i,j) carries no unit"
            if declared != canonical_unit(type, i, j): report mismatch
        if matrix.provenance is None:             report "no unit provenance"
    return issues
```

The AQWA native basis will be established by explicit declaration, in this precedence order, with no
magnitude-based guessing at any step:

```
1. an explicit basis on the DiffractionSpec / converter call     -> use it
2. the AQWA deck's own units record, when the parser reads one   -> use it
3. otherwise                                                     -> raise UnknownUnitBasisError
```

The `.AH1` `GENERAL` density will be used only as a **cross-check** that raises when it contradicts the
declared basis (a declared kg basis with a density near 1.025, or a declared tonne basis with a density near
1025), never as the source of the basis. That keeps the mechanism assertive rather than inferential.

---

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `src/digitalmodel/hydrodynamics/diffraction/matrix_units.py` | canonical per-cell dimension table, `UnitBasis` enum, scale factors, normalize/validate entry points, exception types |
| Create | `tests/hydrodynamics/diffraction/test_matrix_unit_contract.py` | per-cell unit + scale-factor coverage and fail-closed cases |
| Create | `tests/hydrodynamics/diffraction/test_cross_backend_normalization.py` | synthetic AQWA and OrcaWave matrices normalizing to equal physical values |
| Create | `tests/hydrodynamics/diffraction/test_matrix_unit_provenance.py` | provenance completeness and serialization round-trip |
| Create | `tests/hydrodynamics/diffraction/test_dimensional_identities.py` | mass = volume x density, rotational restoring dimensional identity |
| Create | `tests/hydrodynamics/diffraction/fixtures/synthetic_matrices/` | generic full-matrix fixtures, nonzero diagonal and mixed terms, both backends |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/output_schemas.py` | add `MatrixUnitProvenance`; add `resolve_unit(i, j)` to `HydrodynamicMatrix`; extend `to_dict`/`from_dict`; extend `validate_matrix_set` to read units |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` | return matrices with a declared native basis instead of a bare `Dict[float, np.ndarray]` |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/aqwa_ah1_parser.py` | add the density cross-check that raises on a declared-basis contradiction |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py` | replace `_get_added_mass_units` / `_get_damping_units` with the canonical contract; attach provenance |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_converter.py` | same replacement; attach provenance |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_data_extraction.py` | stop forcing mirror cells equal where the backend supplies both; attach the native basis |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` | keep #1636's te→kg conversion; express it through the contract and record it as provenance |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/wamit_reference_loader.py` | retire `_default_units`'s third key set; route through the contract, keeping `_NONDIM_EXPONENT` |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/polars_exporter.py` | replace `units.get(unit_key, "")` with `matrix.resolve_unit(i, j)`; raise rather than emit an empty unit |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py` | delegate `_get_added_mass_unit` / `_get_damping_unit` to the contract |
| Update | `docs/plans/README.md` | index this plan |

`diffraction_units.py` will be left as-is. Its helpers (`tonnes_to_kg` at `:26-28`, `inertia_t_m2_to_kg_m2`
at `:54-56`) remain the arithmetic primitives; the new module supplies the per-cell selection logic that
#1636's post-mortem identified as the thing a call site can get wrong.

---

## TDD Test List

Every test below will be written before the implementation it covers. `A` denotes added mass, `B` damping.
`i`/`j` are 0-based DOF indices in surge, sway, heave, roll, pitch, yaw order.

### `tests/hydrodynamics/diffraction/test_matrix_unit_contract.py`

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_every_added_mass_cell_has_a_canonical_unit` | all 36 A cells resolve to a non-empty unit | `canonical_unit("added_mass", i, j)` for all 36 | 36 non-empty strings, no `""` |
| `test_every_damping_cell_has_a_canonical_unit` | all 36 B cells resolve to a non-empty unit | `canonical_unit("damping", i, j)` for all 36 | 36 non-empty strings, no `""` |
| `test_added_mass_linear_linear_block_unit` | 9 cells, i<3 and j<3 | `(i, j)` in the 3x3 upper-left | `"kg"` |
| `test_added_mass_linear_angular_block_unit` | 9 cells, i<3 and j>=3 | e.g. `(0, 3)` | `"kg.m/rad"` |
| `test_added_mass_angular_linear_block_unit` | 9 cells, i>=3 and j<3 — the mirror block | e.g. `(3, 0)` | `"kg.m"` |
| `test_added_mass_angular_angular_block_unit` | 9 cells, i>=3 and j>=3 | e.g. `(3, 3)` | `"kg.m^2/rad"` |
| `test_damping_four_blocks_units` | the four B blocks, parametrized | `(0,0) (0,3) (3,0) (3,3)` | `"N.s/m"`, `"N.s/rad"`, `"N.m.s/m"`, `"N.m.s/rad"` |
| `test_mirror_blocks_are_labelled_distinctly` | `(i,j)` and `(j,i)` across the off-diagonal blocks differ in label | all 9 mirror pairs, both matrix types | 18 assertions, each pair unequal |
| `test_orcawave_tonne_scale_linear_linear` | te→kg on the A linear block | basis `ORCAWAVE_TE`, `(0, 0)` | `1000.0` |
| `test_orcawave_tonne_scale_linear_angular` | te.m→kg.m | basis `ORCAWAVE_TE`, `(0, 3)` | `1000.0` |
| `test_orcawave_tonne_scale_angular_linear` | mirror block takes the same factor | basis `ORCAWAVE_TE`, `(3, 0)` | `1000.0` |
| `test_orcawave_tonne_scale_angular_angular` | te.m^2→kg.m^2 | basis `ORCAWAVE_TE`, `(3, 3)` | `1000.0` |
| `test_orcawave_tonne_damping_scale_all_blocks` | te/s→kg/s uniform across the four B blocks | basis `ORCAWAVE_TE`, all 36 | `1000.0` for all 36 |
| `test_aqwa_si_scale_is_identity_all_cells` | an AQWA kg-basis deck needs no scaling | basis `AQWA_SI_KG`, all 72 A+B cells | `1.0` for all 72 |
| `test_aqwa_tonne_scale_all_cells` | an AQWA tonne-basis deck scales by 1000 everywhere | basis `AQWA_TE`, all 72 | `1000.0` for all 72 |
| `test_scale_factors_compose_to_identity` | native→canonical→native returns the input | each basis, all 72 cells | round-trip equal to `1e-12` relative |
| `test_unknown_unit_basis_raises` | fail closed on an undeclared basis | `native_basis="unknown"` | `UnknownUnitBasisError` |
| `test_absent_unit_basis_raises_not_defaults` | absence is not silently SI | `native_basis=None` | `UnknownUnitBasisError`, not `1.0` |
| `test_unknown_matrix_type_raises` | fail closed on a bad type | `matrix_type="stiffness"` | `ValueError` |
| `test_out_of_range_index_raises` | fail closed on a bad index | `(6, 0)` | `IndexError`/`ValueError` |
| `test_exporter_raises_rather_than_emitting_blank_unit` | the `.get(key, "")` default is gone | matrix with an incomplete units payload through `PolarsExporter._build_matrix_records` | raises; no record carries `unit=""` |
| `test_aqwa_producer_path_exports_no_blank_unit` | closes the 72-of-72 gap in reproduction block C | `AQWAConverter`-produced results through `PolarsExporter` | 0 blank-unit cells |

### `tests/hydrodynamics/diffraction/test_cross_backend_normalization.py`

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_synthetic_aqwa_and_orcawave_added_mass_agree_after_normalization` | the acceptance criterion "cross-backend synthetic matrices normalize to the same physical values" | one generic synthetic A matrix expressed on the AQWA kg basis and the same physics on the OrcaWave te basis | both normalize to the same 6x6 within `1e-9` relative |
| `test_synthetic_aqwa_and_orcawave_damping_agree_after_normalization` | same for B | matching synthetic B pair | equal within `1e-9` relative |
| `test_agreement_holds_for_mixed_off_diagonal_terms` | the coupling blocks, not just diagonals | fixture with nonzero `(0,3)`, `(3,0)`, `(1,5)`, `(5,1)` | all 36 cells agree |
| `test_unnormalized_pair_disagrees_by_exactly_1000` | the test would fail if normalization were skipped — the assertion can fail | the same pair with normalization bypassed | ratio 1000.0, assertion raises |
| `test_mixed_term_scale_error_fails_closed` | one block converted, another left native | A with `linear` scaled and `angular` native | `MixedScaleError` |
| `test_partial_conversion_of_damping_only_fails_closed` | mirrors #1636's third mutant, at contract level | A normalized, B native, same results object | `MixedScaleError` |
| `test_transposed_dof_ordering_fails_closed` | a matrix whose DOF order is not the canonical order | provenance `dof_order` reversed | `DofOrderError` |
| `test_transposed_matrix_with_asymmetric_coupling_is_detected` | a genuine transpose, not just a relabel | synthetic A with `A[0,3] != A[3,0]`, supplied transposed | detected and raised |
| `test_missing_reference_point_fails_closed` | reference point is required for normalization | provenance with `reference_point=None` | `MissingReferencePointError` |
| `test_reference_points_differing_between_backends_fails_closed` | two backends compared at different origins | AQWA at `[0,0,0]`, OrcaWave at `[0,0,5]` | raises; comparison refused |
| `test_conservation_symmetry_of_normalized_matrix` | scaling preserves the physical symmetry of a symmetric input | symmetric synthetic A on a te basis | normalized result symmetric to `1e-12` |

### `tests/hydrodynamics/diffraction/test_matrix_unit_provenance.py`

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_provenance_records_native_and_normalized_units` | both are retained, not one | normalized `HydrodynamicMatrix` | `native_units` and `normalized_units` both populated and different for a te input |
| `test_provenance_records_reference_point_dof_order_sign_convention` | the four contract fields exist | normalized matrix | all four non-null |
| `test_provenance_records_conversion_revision` | the conversion is versioned | normalized matrix | `conversion_revision` matches the module constant |
| `test_round_trip_preserves_normalized_values` | `to_dict` → `from_dict` | full `DiffractionResults` | normalized matrices equal to `1e-12` |
| `test_round_trip_preserves_native_values` | native values survive serialization | same | native matrices equal to `1e-12` |
| `test_round_trip_preserves_provenance_fields` | provenance survives serialization | same | all provenance fields equal |
| `test_round_trip_of_unnormalized_results_fails_closed` | a results object without provenance cannot be serialized as if normalized | matrix with `provenance=None` | raises |
| `test_from_dict_rejects_unknown_conversion_revision` | forward-compatibility is explicit, not silent | payload with a future revision string | raises with the revision named |
| `test_validate_matrix_set_reports_unit_mismatch` | closes reproduction block B | matrix whose declared unit contradicts the canonical unit | non-empty issue list naming the cell |
| `test_validate_matrix_set_reports_missing_provenance` | closes reproduction block E | matrix with no provenance | non-empty issue list |
| `test_two_results_on_different_bases_are_refused_for_comparison` | the 1000x pair from block B | AQWA kg results vs OrcaWave te results, both unnormalized | raises rather than returning `[]` |

### `tests/hydrodynamics/diffraction/test_dimensional_identities.py`

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_mass_equals_displacement_volume_times_density` | the acceptance criterion's named identity | synthetic hydrostatics, `V = 1.0e4 m^3`, `rho = 1025 kg/m^3` | `mass == 1.025e7 kg` within `1e-9` relative |
| `test_mass_volume_density_identity_fails_on_tonne_basis_mass` | the identity is a real discriminator | same `V` and `rho`, mass supplied in tonnes | raises; the 1000x error is caught |
| `test_mass_volume_density_identity_is_basis_declared` | the check uses the declared basis, not a magnitude heuristic | tonne-basis hydrostatics with a tonne basis declared | passes after normalization |
| `test_rotational_restoring_dimensional_identity` | `C_44` has the dimension of moment per radian | synthetic restoring matrix | `(4,4)` unit resolves to `N.m/rad` |
| `test_restoring_linear_block_dimensional_identity` | `C_33` is force per metre | same | `(2,2)` unit resolves to `N/m` |
| `test_natural_period_is_invariant_under_basis_change` | a conservation-class check: `T_n = 2*pi*sqrt((M+A)/C)` is unchanged by a consistent basis change | synthetic `M`, `A`, `C` on the kg basis and the same physics on the te basis | periods equal to `1e-9` relative |
| `test_natural_period_changes_when_only_added_mass_is_converted` | the invariance test can fail — guards against the #1636 W5 failure mode | `M` native, `A` converted | periods differ; assertion raises |
| `test_added_mass_ratio_against_displaced_mass_is_dimensionless` | `A33 / (rho * V)` is order-unity for a synthetic full-matrix fixture | synthetic fixture with declared `V` and `rho` | ratio within the fixture's stated bounds |

Fixtures will be generic synthetic matrices only — a unit box and a simple synthetic cylinder generated in
the test module from declared geometry. No vessel, project, or client identifier will appear in any fixture
name, file, or comment.

---

## Acceptance Criteria

- [ ] New tests pass: `uv run pytest tests/hydrodynamics/diffraction/test_matrix_unit_contract.py tests/hydrodynamics/diffraction/test_cross_backend_normalization.py tests/hydrodynamics/diffraction/test_matrix_unit_provenance.py tests/hydrodynamics/diffraction/test_dimensional_identities.py -v`
- [ ] Every one of the 72 A and B cells has an explicit dimensional unit, and a scale factor tested for each declared native basis.
- [ ] Cross-backend synthetic matrices normalize to the same physical values within `1e-9` relative.
- [ ] Unknown unit basis, missing reference point, transposed DOF ordering, asymmetric conversion rules and mixed-term scale errors each raise, and each has a test that fails when the guard is removed.
- [ ] Round-trip serialization preserves native values, normalized values and conversion provenance.
- [ ] `mass = displacement volume x density` and the rotational restoring dimensional identity are asserted, and the corresponding negative tests demonstrate that each assertion can fail.
- [ ] `PolarsExporter` emits no cell with `unit=""` from any producer path — verified for `aqwa_converter`, `solver/orcawave_converter`, `orcawave_runner` and `wamit_reference_loader`.
- [ ] `_get_added_mass_units` / `_get_damping_units` / `_default_units` no longer declare independent key sets; `grep -rn "_get_added_mass_units\|_default_units" src/` shows delegation to the contract or no hits.
- [ ] No regression: `uv run pytest tests/hydrodynamics/diffraction/ tests/orcawave/ tests/solver/` shows no new failures against the pre-change baseline, and the baseline count is recorded in the PR.
- [ ] CI lint toolchain run per `.claude/rules/verify-ci-lint-toolchain.md` before push.
- [ ] `scripts/legal/legal-sanity-scan.sh` passes; no client, vessel, or project identifier in any added file.
- [ ] Review artifacts posted to `scripts/review/results/`.

---

## Adversarial Review Summary

<!-- Filled in after the review pass completes. Verdicts pending. -->

| Provider | Verdict | Key findings |
|---|---|---|
| Claude | pending | pending |
| Codex | pending | pending |
| Gemini | pending | pending |

**Overall result:** pending

Revisions made based on review:
- (pending)

---

## Risks and Open Questions

- **Risk — dependency on #1579 (reference-point semantics).** [#1579](https://github.com/vamseeachanta/digitalmodel/issues/1579)
  owns inertia-tensor origin and reference-point semantics across backends and is OPEN at
  `status:needs-plan`. This issue's `MatrixUnitProvenance.reference_point` **consumes** that definition; it
  must not redefine it. The mitigation is to treat the reference point here as an opaque, required
  3-vector plus a frame label supplied by the caller, and to assert only that it is present and that two
  matrices being compared agree on it. Any semantics beyond presence and equality belong to #1579. If #1579
  lands a different frame representation, this plan's provenance field adopts it rather than competing.
- **Risk — dependency on #1580 (restoring composition).** [#1580](https://github.com/vamseeachanta/digitalmodel/issues/1580)
  owns the solver-neutral restoring-stiffness contract. The dimensional-identity tests in this plan assert
  only the *dimension* of restoring terms (`C_33` is `N/m`, `C_44` is `N.m/rad`); they will not assert how a
  restoring matrix is composed, which surface it is read from, or how hydrostatic and mooring contributions
  are combined. The issue's own Scope confines this plan to normalizing hydrostatic mass, displacement
  volume and restoring output "only where required to make the matrix reference contract unambiguous".
  Coordination point: if #1580 introduces its own restoring container, this plan's dimensional tests move to
  target it rather than being duplicated.
- **Risk — a mirror-block relabel is an observable change for downstream consumers.** Labelling
  `B[ROLL, SURGE]` as `N.m.s/m` where it is `N.s/rad` today changes exported CSV unit strings even though no
  numeric value moves. Any consumer that string-matches unit labels will see a difference. Mitigation:
  enumerate label consumers before implementing (`grep -rn "N.s/rad\|kg.m\^2" src/ tests/ docs/`), and state
  the label delta explicitly in the PR body. This is a relabel, not a revaluation, and the plan must not let
  reviewers read it as a numeric change.
- **Risk — the AQWA native basis may not be recoverable from files the repo can read.** If neither the
  `.LIS` nor the `.AH1` carries a machine-readable units declaration, requiring an explicit basis makes
  previously-working AQWA calls raise. This is the intended fail-closed behavior per the issue's acceptance
  criteria, but it is a breaking change for any caller relying on the implicit SI assumption. Mitigation:
  the explicit-declaration path (precedence step 1) gives every caller a migration; the change and its
  migration must be named in the PR body and in `docs/`. Whether a deprecation window is wanted is an open
  question for the approver.
- **Risk — mirror cells are currently forced equal at extraction.** `solver/orcawave_data_extraction.py:226-244`
  writes `matrix[i, j] = matrix[j, i] = <single attribute>`. Where OrcFxAPI genuinely exposes one value per
  mirror pair, the transpose-detection tests cannot be exercised against that producer and must use
  synthetic input. The plan states this openly rather than implying full coverage; the transpose guard is
  tested at the contract boundary, not through that extractor.
- **Risk — regenerating committed benchmark artifacts.** #1636 deferred artifact regeneration (its W6) partly
  on licensed-lane availability. If any committed artifact under `docs/benchmarks/` carries unit strings,
  this relabel may require regeneration on the licensed host. Mitigation: check for affected artifacts
  before implementing; if regeneration is needed and the licensed lane is unavailable, split it into a
  follow-on rather than blocking the contract.
- **Risk — scope creep into #1550.** The te→kg conversion at `orcawave_runner.py:754-759` is settled and
  merged. This plan re-expresses that call through the contract and records it as provenance; it must not
  change the numeric result. A regression test pinning `orcawave_runner`'s output before and after is the
  guard.
- **Open:** should `rad` appear in the canonical labels at all? Writing `kg.m/rad` for `A[0,3]` is
  dimensionally honest and distinguishes the mirror blocks, but diverges from the shorter `kg.m` used
  throughout the repo today and in OrcaFlex's own vessel-type documentation. The alternative is to keep
  `kg.m` for both mirror blocks and carry the distinction only in a structured dimension record, not in the
  display string. Flagged for the approver; the TDD table above assumes the explicit-`rad` form.
- **Open:** is `1e-9` relative the right cross-backend agreement tolerance, or should it be tighter given
  that every scale factor here is an exact power of ten? Stating the tolerance before implementation is
  deliberate, per `.claude/rules/reproducibility-is-not-correctness.md` §4.

---

## Complexity: T3

**T3** — the change spans eleven source modules across two backend families plus a shared output type,
converges five independently-authored unit-key conventions, introduces a new contract module and four new
test modules with roughly fifty tests, changes observable exported unit strings, adds a fail-closed
behavior that is breaking for implicit-SI callers, and requires coordination with two open sibling issues
(#1579, #1580) under epic #1825. Per the T3 definition (large, cross-provider or systemic), this warrants a
three-provider adversarial review at both the plan and the code stage.
