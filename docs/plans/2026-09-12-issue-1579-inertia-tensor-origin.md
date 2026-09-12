# Plan for #1579: Make diffraction inertia-tensor origin strict and consistent across backends

> **Status:** draft
> **Complexity:** T2
> **Date:** 2026-09-12
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1579
> **Parent epic:** https://github.com/vamseeachanta/digitalmodel/issues/1825
> **Client:** N/A
> **Lane:** lane:codex
> **Review artifacts:** scripts/review/results/2026-09-12-plan-1579-claude.md | ...-codex.md | ...-gemini.md

---

## Resource Intelligence Summary

### Existing repo code

- Found: `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:209-215` — `VesselInertia.inertia_tensor_origin` is declared as a bare `str` with default `"body_origin"`. No `Literal`, no enum, no `field_validator`. Every other constrained field in the same class uses `Literal` (`mode:` at line 178), so the free-form typing is a local inconsistency, not a package-wide convention.
- Found: `src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py:652-681` — `AQWABackend._compute_inertia()` reads `inertia.inertia_tensor` and `inertia.radii_of_gyration` and never reads `inertia_tensor_origin`. The emitted PMAS card (line 644-647) is attached to node `98000`, which Deck 1 (lines 499-506) places at `body.vessel.inertia.centre_of_gravity`.
- Found: `src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py:353-360` — `_TENSOR_ORIGIN_MAP` covers only `body_origin` and `centre_of_mass`; the lookup is `.get(origin, "Body origin")`, a silent fallback for every other string.
- Found: `src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py:340` — the free-floating branch hardcodes `body["BodyRadiiOfGyrationOriginType"] = "Body origin"` with no reference to any spec field.
- Found: `src/digitalmodel/hydrodynamics/diffraction/reverse_parsers.py:636-639` — the OrcaWave explicit-mode branch **does** read `BodyInertiaTensorOriginType` through `_reverse_tensor_origin()`. The issue's third claim is therefore stale (see Reproduction proofs); the residual defect is that `_reverse_tensor_origin()` (lines 803-813) silently returns `"body_origin"` for any unrecognised native string.
- Found: `src/digitalmodel/hydrodynamics/diffraction/reverse_parsers.py:108-112` — the AQWA `.dat` reverse parser constructs `VesselInertia(mass=..., centre_of_gravity=[0.0, 0.0, 0.0], inertia_tensor=inertia_tensor)` with no `inertia_tensor_origin` and a hardcoded zero CoG, so a tensor emitted about the CoG node returns as a body-origin tensor about the origin.
- Found: `src/digitalmodel/hydrodynamics/diffraction/resolver.py:508-512` — the resolver already asserts the physical contract in a comment ("Radii of gyration are defined about the centre of mass") and sets `inertia["inertia_tensor_origin"] = "centre_of_mass"` for resolver-estimated tensors. That contract is enforced in exactly one code path and nowhere in the schema.
- Gap: no module, function, or test in the package performs a parallel-axis transformation (Gap proof G3 below).
- Gap: `aqwa_backend.py` contains zero references to `inertia_tensor_origin` (Gap proof G1).
- Gap: `reverse_parsers.py` contains zero references to `BodyRadiiOfGyrationOriginType` (Gap proof G2), so the free-floating round trip drops the radii reference point entirely.

### Standards

| Standard | Status | Source |
|---|---|---|
| Parallel-axis (Huygens–Steiner) theorem | classical mechanics, not a purchased standard — no ledger entry required | derived in-plan; no `Citation` sidecar is owed per `.claude/rules/calc-citation-contract.md` ("the constant is derived from the code itself … not a standard") |
| ANSYS AQWA reference manual — PMAS card reference-node semantics | **not verified in this session** | vendor-licensed; off-repo. Resolved as an Open Question below, not asserted. |
| OrcaWave `BodyRadiiOfGyrationOriginType` admissible values | **not verified in this session** | vendor-licensed; off-repo. Resolved as an Open Question below, not asserted. |

### LLM Wiki pages consulted

No relevant wiki pages. The work is a schema/serialisation contract inside `digitalmodel`; no standards-derived constant is introduced, so no wiki citation target is owed.

### Documents consulted

- Issue [#1579](https://github.com/vamseeachanta/digitalmodel/issues/1579) body — four claims, five acceptance criteria, and a Boundaries section that assigns output-unit normalization to #1550.
- Related issue [#1582](https://github.com/vamseeachanta/digitalmodel/issues/1582) — OPEN, `status:needs-plan`, "Normalize diffraction matrix units across AQWA and OrcaWave backends". Consumes the reference-point semantics this issue defines.
- Related issue [#1580](https://github.com/vamseeachanta/digitalmodel/issues/1580) — OPEN, `status:needs-plan`, "Define a solver-neutral diffraction restoring-stiffness contract". Same reference-point dependency.
- Related issue [#1550](https://github.com/vamseeachanta/digitalmodel/issues/1550) — CLOSED. Output-unit normalization; explicitly out of scope per the issue's own Boundaries.
- Related issue [#610](https://github.com/vamseeachanta/digitalmodel/issues/610) — OPEN, `status:plan-review`. Licensed end-to-end acceptance test. This plan will not consume it; all fixtures will be synthetic.
- Parent epic [#1825](https://github.com/vamseeachanta/digitalmodel/issues/1825) — OPEN, "[Epic] Diffraction results agree across solvers — one strict contract, three-way benchmarks".
- `tests/hydrodynamics/diffraction/test_reverse_parsers.py:808-871` — class `TestOrcaWaveInputParserInertiaOrigin` already covers OrcaWave origin parse and round trip, landed in commit `55dd409f` (2026-07-18, "fix(orcaflex,orcawave): resolve 66 verified model-generation review findings"). These tests will be extended, not duplicated.
- `tests/hydrodynamics/diffraction/test_resolver.py:297-314` — `test_explicit_inertia_tensor_origin_is_centre_of_mass` pins the resolver contract at `resolver.py:512`.
- `docs/plans/` — no prior plan for #1579 and no prior diffraction-inertia plan (Gap proof G8).
- Drive-file index: not consulted. The issue is a code-contract defect with no data-discovery surface, and its Boundaries section forbids client or project data; a drive search would only risk pulling identifiers into a public-repo plan.

### Gaps identified

- No parallel-axis transformation exists anywhere in the package; it must be built from scratch.
- No schema-level rejection of unknown `inertia_tensor_origin` values exists; the field admits any string including the empty string.
- No contract binds a radii-of-gyration-derived tensor to a centre-of-mass reference outside `resolver.py`; both backends derive `I = m·k²` independently (`aqwa_backend.py:671-681`, `orcawave_backend.py:226-236`) with no origin handling at all.
- No test file covers inertia reference points (Gap proofs G6, G7); no existing test exercises a nonzero-CoG tensor with products of inertia through either backend.
- No cross-backend physical-equivalence assertion exists for inertia; the package's inertia tests compare each backend against its own prior output only.

### Evidence (embedded verification)

**Repository state** (verified 2026-09-12):

```
$ git log --oneline -1
6f808a61 feat(orcaflex): add BaseFile variation models and name-keyed merge to modular_generator (#2101)
```

**Issue statuses** (verified 2026-09-12 via `gh issue view --json number,title,state,labels`):

- `#1579` — OPEN — "Make diffraction inertia-tensor origin strict and consistent across backends" — labels: `cat:engineering`, `domain:hydro`, `lane:codex`, `machine:licensed-win-1`, `status:needs-plan`; parent `#1825`
- `#1582` — OPEN — "Normalize diffraction matrix units across AQWA and OrcaWave backends" — `status:needs-plan`, `lane:codex`
- `#1580` — OPEN — "Define a solver-neutral diffraction restoring-stiffness contract" — `status:needs-plan`, `lane:codex`
- `#1550` — CLOSED — "Added-mass/damping units inconsistency in DiffractionResults …"
- `#610` — OPEN — "OrcaWave: add licensed end-to-end acceptance test for arbitrary mesh workflow" — `status:plan-review`
- `#1825` — OPEN — "[Epic] Diffraction results agree across solvers — one strict contract, three-way benchmarks" — `epic`

**File existence** (2026-09-12):

- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py`
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py`
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py`
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/reverse_parsers.py`
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/resolver.py`
- EXISTS: `tests/hydrodynamics/diffraction/test_reverse_parsers.py`
- MISSING (new — this plan will create): `src/digitalmodel/hydrodynamics/diffraction/inertia_reference.py`
- MISSING (new — this plan will create): `tests/hydrodynamics/diffraction/test_inertia_reference.py`
- MISSING (new — this plan will create): `tests/hydrodynamics/diffraction/test_inertia_origin_contract.py`

**Line excerpts.**

`input_schemas.py:209-215` — the free-form field:

```
    inertia_tensor_origin: str = Field(
        "body_origin",
        description=(
            "Origin for inertia tensor specification: "
            "'body_origin' or 'centre_of_mass'"
        ),
    )
```

`aqwa_backend.py:652-681` — the origin-blind emission path:

```
    @staticmethod
    def _compute_inertia(
        inertia: VesselInertia,
    ) -> tuple[float, float, float, float, float, float]:
        """Return (Ixx, Iyy, Izz, Ixy, Ixz, Iyz) from spec inertia.

        If an explicit inertia_tensor is provided it takes precedence;
        otherwise the moments are computed from mass and radii of gyration.
        """
        if inertia.inertia_tensor is not None:
            t = inertia.inertia_tensor
            return (
                t.get("Ixx", 0.0),
                t.get("Iyy", 0.0),
                t.get("Izz", 0.0),
                t.get("Ixy", 0.0),
                t.get("Ixz", 0.0),
                t.get("Iyz", 0.0),
            )
        # Compute from radii of gyration: I = m * k^2
        mass = inertia.mass
        kxx, kyy, kzz = inertia.radii_of_gyration  # type: ignore[misc]
        return (
            mass * kxx**2,
            mass * kyy**2,
            mass * kzz**2,
            0.0,
            0.0,
            0.0,
        )
```

`aqwa_backend.py:499-506` — node 98000 is placed at the CoG:

```
                # Add CoG node (element 98000) for mass reference
                cog = body.vessel.inertia.centre_of_gravity
                cards.append(
                    f"{struct_idx:>6d}{98000:>5d}         "
                    f"{_fmt_coord(cog[0])}"
                    f"{_fmt_coord(cog[1])}"
                    f"{_fmt_coord(cog[2])}"
                )
```

`aqwa_backend.py:644-647` — the PMAS card is written against that node:

```
            cards.append(
                f"{_WS:>5s}{idx:>1d}PMAS{_WS:>5s}98000"
                f"{_fmt_float(ixx)}{_fmt_float(ixy)}{_fmt_float(ixz)}"
                f"{_fmt_float(iyy)}{_fmt_float(iyz)}{_fmt_float(izz)}"
            )
```

`orcawave_backend.py:353-360` — the silent unknown-value fallback:

```
        _TENSOR_ORIGIN_MAP = {
            "body_origin": "Body origin",
            "centre_of_mass": "Centre of mass",
        }
        origin = getattr(inertia, "inertia_tensor_origin", "body_origin")
        body["BodyInertiaTensorOriginType"] = _TENSOR_ORIGIN_MAP.get(
            origin, "Body origin"
        )
```

`orcawave_backend.py:335-340` — the hardcoded radii origin:

```
        body[radii_key] = [
            [r[0], 0, 0],
            [0, r[1], 0],
            [0, 0, r[2]],
        ]
        body["BodyRadiiOfGyrationOriginType"] = "Body origin"
```

`reverse_parsers.py:803-813` — the silent reverse fallback:

```
    @staticmethod
    def _reverse_tensor_origin(orcawave_origin: str) -> str:
        """Map OrcaWave BodyInertiaTensorOriginType back to spec value.

        Exact reverse of orcawave_backend._TENSOR_ORIGIN_MAP.
        """
        origin_map = {
            "body origin": "body_origin",
            "centre of mass": "centre_of_mass",
        }
        return origin_map.get(str(orcawave_origin).lower(), "body_origin")
```

`reverse_parsers.py:108-112` — the AQWA reverse parser drops both the origin and the CoG:

```
        vessel_inertia = VesselInertia(
            mass=mass,
            centre_of_gravity=[0.0, 0.0, 0.0],
            inertia_tensor=inertia_tensor,
        )
```

`resolver.py:508-512` — the contract stated in one place only:

```
        # Radii of gyration are defined about the centre of mass, so the
        # estimated diagonal tensor is CG-relative. Label it accordingly
        # instead of inheriting the schema default ("body_origin"), which
        # would mis-state the reference point whenever the CoG is offset.
        inertia["inertia_tensor_origin"] = "centre_of_mass"
```

**Gap proofs** (2026-09-12, ripgrep/Select-String over the package):

- G1: `inertia_tensor_origin` in `src/.../diffraction/aqwa_backend.py` → **0 matches** → confirms the AQWA backend never reads the declared origin.
- G2: `BodyRadiiOfGyrationOriginType` in `src/.../diffraction/reverse_parsers.py` → **0 matches** → confirms the free-floating radii reference point is dropped on reverse parse.
- G3: `parallel.?axis` (case-insensitive) across `src/.../diffraction/*.py` → **0 matches** → confirms no parallel-axis transformation exists in the package.
- G4: `inertia_tensor_origin` across `src/.../diffraction/reverse_parsers.py` → matches at line **636 only** → the OrcaWave explicit branch handles it; the AQWA `parse()` path and the OrcaWave free-floating branch do not.
- G5: `Literal[` in `input_schemas.py` → lines **178, 497, 503** — `inertia_tensor_origin` (line 209) is absent from that list.
- G6: `1579` across `tests/hydrodynamics/diffraction/*.py` → **0 matches**.
- G7: `tests/hydrodynamics/diffraction/*inertia*` → **0 files**.
- G8: `docs/plans/*157*` → only `2026-07-13-issue-1571-…` and `2026-07-13-issue-1574-…`; `docs/plans/*inertia*` → **0 files** → confirms no prior plan for this issue.
- G9: `inertia_tensor_origin` repo-wide → **8 sites** (4 in `src/`, 4 in `tests/`) and **0 in any `.yml`/`.yaml`/`.json` fixture** → the blast radius of tightening the field is bounded and no committed spec fixture will be invalidated.

**Reproduction proofs** (verify-against-repo-state). Executed 2026-09-12 with `D:\ws\digitalmodel\.venv\Scripts\python.exe` against working tree `6f808a61`, using a throwaway scratchpad script that imports the package and calls the real code paths. Warnings elided.

```
$ python scratchpad/repro_1579.py

=== CLAIM 0: schema accepts arbitrary origin strings ===
  ACCEPTED inertia_tensor_origin='body_origin' -> 'body_origin'
  ACCEPTED inertia_tensor_origin='centre_of_mass' -> 'centre_of_mass'
  ACCEPTED inertia_tensor_origin='waterline' -> 'waterline'
  ACCEPTED inertia_tensor_origin='' -> ''
  ACCEPTED inertia_tensor_origin='BODY_ORIGIN' -> 'BODY_ORIGIN'
  ACCEPTED inertia_tensor_origin='keel' -> 'keel'

=== CLAIM 2: AQWA _compute_inertia is origin-blind ===
  origin=   'body_origin' -> (1.0e+08, 2.0e+08, 2.5e+08, 3.0e+06, 4.0e+06, 5.0e+06)
  origin='centre_of_mass' -> (1.0e+08, 2.0e+08, 2.5e+08, 3.0e+06, 4.0e+06, 5.0e+06)
  origin=     'waterline' -> (1.0e+08, 2.0e+08, 2.5e+08, 3.0e+06, 4.0e+06, 5.0e+06)

=== CLAIM 4: rog-derived tensor default origin ===
  VesselInertia(radii_of_gyration=[12.0, 40.0, 42.0]).inertia_tensor_origin = 'body_origin'

=== CLAIM 3: OrcaWave reverse-parse of tensor origin ===
         'Body origin' -> 'body_origin'
      'Centre of mass' -> 'centre_of_mass'
           'Waterline' -> 'body_origin'
   'Some future value' -> 'body_origin'
```

- Reproduced at: 2026-09-12.
- **Claim 1 — "OrcaWave silently maps unknown origin strings to body origin": REPRODUCES.** `orcawave_backend.py:358-360` is a `dict.get` with a `"Body origin"` default. Reinforced by Claim 0 above: the schema admits `'waterline'`, `'keel'` and `''`, so a caller can reach that fallback through ordinary validated input rather than only through a programming error.
- **Claim 2 — "AQWA attaches PMAS to the centre-of-gravity node but emits the supplied tensor unchanged, ignoring the declared origin": REPRODUCES.** `_compute_inertia()` returns byte-identical components for all three declared origins, and Gap proof G1 shows the field is never read in that file. The node the card references is placed at the CoG by `aqwa_backend.py:499-506`.
- **Claim 3 — "OrcaWave reverse parsing drops the native tensor-origin field": DOES NOT REPRODUCE as written.** `reverse_parsers.py:636-639` parses it, and `test_reverse_parsers.py:808-871` guards it; the behaviour landed in `55dd409f` (2026-07-18), after the issue was filed. **The real residual behaviour, which this plan will address, is threefold:** (a) `_reverse_tensor_origin()` silently maps every unrecognised native string to `'body_origin'` (output above: `'Waterline'` and `'Some future value'` both return `'body_origin'`) — a fail-open reinterpretation of an unknown reference point as the body origin; (b) the OrcaWave free-floating branch (`reverse_parsers.py:617-626`) never reads `BodyRadiiOfGyrationOriginType` (Gap proof G2); (c) the AQWA `.dat` reverse parser (`reverse_parsers.py:108-112`) declares neither an origin nor the CoG, so a CoG-referenced AQWA tensor returns as a body-origin tensor with a zero CoG.
- **Claim 4 — "Tensors derived from radii of gyration do not have an enforced centre-of-mass reference contract": REPRODUCES.** `VesselInertia(radii_of_gyration=…)` reports `inertia_tensor_origin == 'body_origin'`, contradicting `resolver.py:508-512`, which states in the same package that radii of gyration are centre-of-mass-relative. The contract holds only on the resolver's estimation path; a spec authored directly, or built by `vessel_deck_builder.py:191-199` / `parametric_spec_generator.py:307-316`, inherits the wrong label. `orcawave_backend.py:340` then hardcodes `"Body origin"` for the radii it emits, propagating the contradiction into the deck.

<!-- Distinct sources consulted: issue body, 5 source files, 2 test files, git history (55dd409f), 5 sibling/parent issues, docs/plans index, live reproduction run. Count: 9 sub-section sources — minimum 3 satisfied. -->

---

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-09-12-issue-1579-inertia-tensor-origin.md` |
| Implementation — transform | `src/digitalmodel/hydrodynamics/diffraction/inertia_reference.py` |
| Implementation — schema | `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py` |
| Implementation — AQWA forward | `src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py` |
| Implementation — OrcaWave forward | `src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py` |
| Implementation — reverse | `src/digitalmodel/hydrodynamics/diffraction/reverse_parsers.py` |
| Tests — transform | `tests/hydrodynamics/diffraction/test_inertia_reference.py` |
| Tests — contract | `tests/hydrodynamics/diffraction/test_inertia_origin_contract.py` |
| Tests — extended | `tests/hydrodynamics/diffraction/test_input_schemas.py`, `test_aqwa_backend.py`, `test_orcawave_backend.py`, `test_reverse_parsers.py`, `test_resolver.py` |
| Plan review — Claude | `scripts/review/results/2026-09-12-plan-1579-claude.md` |
| Plan review — Codex | `scripts/review/results/2026-09-12-plan-1579-codex.md` |
| Plan review — Gemini | `scripts/review/results/2026-09-12-plan-1579-gemini.md` |
| Docs updates | `docs/plans/README.md` (index row) |
| Wiki updates | none — no standards-derived constant is introduced |

---

## Deliverable

A strict, fail-closed inertia reference-point contract in the diffraction input schema, plus a `inertia_reference` module implementing the full parallel-axis transformation (products of inertia included), such that a spec declaring an inertia tensor about either the body origin or the centre of mass will produce physically equivalent AQWA and OrcaWave models, will survive a round trip through both reverse parsers with its reference point intact, and will be rejected at parse time if the reference point is unknown or physically inconsistent.

---

## Pseudocode

**New module — `inertia_reference.py`:**

```
CANONICAL_KEYS = (Ixx, Iyy, Izz, Ixy, Ixz, Iyz)

function to_matrix(tensor_dict) -> 3x3 symmetric array:
    read the six canonical keys, defaulting absent keys to 0.0
    reject any key in tensor_dict outside the canonical six  # fail-closed on typos
    return [[Ixx, Ixy, Ixz], [Ixy, Iyy, Iyz], [Ixz, Iyz, Izz]]

function translate_inertia(I, mass, d):
    # d is the vector FROM the current reference point TO the new one.
    # Huygens-Steiner, full form: moving the reference point AWAY from the
    # centre of mass ADDS m*(|d|^2 * identity - outer(d, d)); moving TOWARD
    # the centre of mass subtracts it. Callers pass the signed shift.
    S = mass * (dot(d, d) * identity(3) - outer(d, d))
    return I + S

function to_centre_of_mass(I, mass, origin_label, cog):
    if origin_label == "centre_of_mass": return I
    if origin_label == "body_origin":    return translate_inertia(I, mass, -cog)
    raise ValueError(unknown origin_label)   # no default branch

function to_body_origin(I, mass, origin_label, cog):
    mirror of the above

function assert_physically_admissible(I, mass, context):
    # conservation-class comparator per .claude/rules/reproducibility-is-not-correctness.md
    require I symmetric to tolerance
    require all eigenvalues > 0                      # positive definiteness
    require each pair of principal moments to satisfy the triangle inequality
    otherwise raise InertiaReferenceError naming context, the tensor, and which
    invariant failed -- never warn-and-continue
```

**Schema change — `input_schemas.py`:**

```
inertia_tensor_origin: Optional[Literal["body_origin", "centre_of_mass"]] = None

model_validator(mode="after") resolve_inertia_reference():
    if inertia_tensor is not None:
        if inertia_tensor_origin is None:
            raise ValueError(
                "inertia_tensor requires an explicit inertia_tensor_origin "
                "('body_origin' or 'centre_of_mass'); the reference point "
                "cannot be inferred")
        reject any inertia_tensor key outside the canonical six
    else:                                  # radii-of-gyration path
        if inertia_tensor_origin == "body_origin":
            raise ValueError(
                "radii_of_gyration are defined about the centre of mass; "
                "inertia_tensor_origin='body_origin' is inconsistent")
        inertia_tensor_origin = "centre_of_mass"   # the enforced contract
```

**AQWA forward — `aqwa_backend._compute_inertia`:**

```
I = to_matrix(inertia.inertia_tensor)  or  diag(m*k^2) when only radii are given
I_cm = to_centre_of_mass(I, mass, inertia.inertia_tensor_origin, inertia.centre_of_gravity)
assert_physically_admissible(I_cm, mass, context="AQWA PMAS deck 4")
emit I_cm, because the PMAS card references node 98000 which deck 1 places at the CoG
```

**OrcaWave forward:**

```
strict lookup: _TENSOR_ORIGIN_MAP[origin]  -- KeyError becomes a raised ValueError
free-floating branch: emit the radii origin from the resolved contract rather than
a hardcoded literal, and raise if the declared contract cannot be expressed in the
native key's admissible value set
```

**Reverse parsers:**

```
_reverse_tensor_origin: strict dict lookup, raise on unknown native string
OrcaWave free-floating: read BodyRadiiOfGyrationOriginType and fail closed unless it
    matches the centre-of-mass contract
AQWA parse(): read node 98000 coordinates from deck 1 as centre_of_gravity, and
    declare inertia_tensor_origin="centre_of_mass" because the PMAS card references
    that node
```

---

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `src/digitalmodel/hydrodynamics/diffraction/inertia_reference.py` | parallel-axis transform, canonical tensor conversion, admissibility invariants, `InertiaReferenceError` |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py` | replace the free-form `str` at line 209 with an `Optional[Literal[...]]`; add the resolving model validator |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py` | `_compute_inertia` (652-681) will transform to the CoG reference and assert admissibility before emission |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py` | strict origin lookup (353-360); emit the resolved radii origin rather than the hardcoded literal at line 340 |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/reverse_parsers.py` | strict `_reverse_tensor_origin` (803-813); free-floating radii origin; AQWA `parse()` CoG + origin recovery (108-112) |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/resolver.py` | keep line 512's contract but source it from the shared constant so the two cannot drift |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/vessel_deck_builder.py` | the `VesselInertia(...)` constructions at 191-199 and 223-229 will declare an explicit origin |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/parametric_spec_generator.py` | the `VesselInertia(...)` construction at 307-316 will declare an explicit origin |
| Create | `tests/hydrodynamics/diffraction/test_inertia_reference.py` | TDD suite for the transform module |
| Create | `tests/hydrodynamics/diffraction/test_inertia_origin_contract.py` | TDD suite for schema strictness, forward emission, round trip, cross-backend equivalence |
| Modify | `tests/hydrodynamics/diffraction/test_reverse_parsers.py` | extend `TestOrcaWaveInputParserInertiaOrigin` (808-871) with the unknown-value rejection case |
| Modify | `tests/hydrodynamics/diffraction/test_input_schemas.py` | schema rejection cases |
| Update | `docs/plans/README.md` | index this plan |

---

## TDD Test List

All tests will be written before the corresponding implementation. Every row uses generic synthetic input only: a rectangular test body, mass `m = 1.0e7 kg`, centre of gravity `c = [10.0, 2.0, -5.0] m`, and a body-origin tensor `I_o` carrying nonzero products `Ixy = 3.0e6`, `Ixz = 4.0e6`, `Iyz = 5.0e6 kg·m²`. `I_cm` denotes `I_o − m(|c|²δ − c⊗c)`, computed once by hand in the fixture module and asserted against the code, never captured from the code.

| # | Test name | File | What it verifies | Expected input | Expected output |
|---|---|---|---|---|---|
| 1 | `test_translate_inertia_zero_shift_is_identity` | `test_inertia_reference.py` | a null shift changes nothing | `I_o`, `m`, `d=[0,0,0]` | `I_o` exactly |
| 2 | `test_translate_inertia_matches_hand_computed_diagonal` | `test_inertia_reference.py` | diagonal parallel-axis terms are correct | `I_o`, `m`, `d=c` | `Ixx + m(c_y²+c_z²)`, `Iyy + m(c_x²+c_z²)`, `Izz + m(c_x²+c_y²)` |
| 3 | `test_translate_inertia_matches_hand_computed_products` | `test_inertia_reference.py` | **product** terms carry the `−m·d_i·d_j` coupling, not zero | `I_o`, `m`, `d=c` | `Ixy − m·c_x·c_y`, `Ixz − m·c_x·c_z`, `Iyz − m·c_y·c_z` |
| 4 | `test_translate_inertia_round_trip_is_involutive` | `test_inertia_reference.py` | conservation-class invariant: out and back recovers the input | `I_o` → CoM → body origin | `I_o` within `1e-9` relative |
| 5 | `test_translate_inertia_preserves_trace_invariant` | `test_inertia_reference.py` | `trace(I) − 2m|d|²` is invariant under the shift | `I_o`, `m`, `d=c` | equality within `1e-9` relative |
| 6 | `test_to_matrix_rejects_unknown_tensor_key` | `test_inertia_reference.py` | typo keys fail closed rather than defaulting to 0.0 | `{"Ixx":…, "Iyx": 1.0}` | `InertiaReferenceError` |
| 7 | `test_admissibility_rejects_non_positive_definite` | `test_inertia_reference.py` | an impossible tensor is refused | `diag(1.0, 1.0, −1.0)` | `InertiaReferenceError` naming the failed invariant |
| 8 | `test_admissibility_rejects_triangle_inequality_violation` | `test_inertia_reference.py` | `Ixx + Iyy < Izz` is refused | `diag(1.0, 1.0, 5.0)` | `InertiaReferenceError` |
| 9 | `test_admissibility_accepts_offset_cog_fixture` | `test_inertia_reference.py` | the synthetic fixture itself is admissible in both frames | `I_o` and `I_cm` | no raise |
| 10 | `test_schema_rejects_unknown_origin_string` | `test_input_schemas.py` | AC 1 — unknown values fail validation | `inertia_tensor_origin="waterline"` | `ValidationError` |
| 11 | `test_schema_rejects_empty_origin_string` | `test_input_schemas.py` | the empty string is not a silent body origin | `inertia_tensor_origin=""` | `ValidationError` |
| 12 | `test_schema_rejects_case_variant_origin` | `test_input_schemas.py` | `"BODY_ORIGIN"` is not silently accepted | `inertia_tensor_origin="BODY_ORIGIN"` | `ValidationError` |
| 13 | `test_schema_requires_origin_when_tensor_given` | `test_input_schemas.py` | the reference point is never inferred | `inertia_tensor=I_o`, origin omitted | `ValidationError` naming `inertia_tensor_origin` |
| 14 | `test_schema_forces_centre_of_mass_for_radii_path` | `test_input_schemas.py` | AC — radii-derived tensors are CoM-relative | `radii_of_gyration=[12,40,42]`, origin omitted | `inertia_tensor_origin == "centre_of_mass"` |
| 15 | `test_schema_rejects_body_origin_with_radii` | `test_input_schemas.py` | the contradiction is refused, not coerced | `radii_of_gyration=[…]`, `origin="body_origin"` | `ValidationError` |
| 16 | `test_aqwa_body_origin_tensor_is_transformed_to_cog` | `test_inertia_origin_contract.py` | AC — AQWA never silently reads a body-origin tensor as CoM-relative | spec with `I_o`, `origin="body_origin"`, offset `c` | deck-4 PMAS components equal `I_cm`, all six, products included |
| 17 | `test_aqwa_centre_of_mass_tensor_is_emitted_unchanged` | `test_inertia_origin_contract.py` | the no-op branch does not transform twice | spec with `I_cm`, `origin="centre_of_mass"` | deck-4 PMAS components equal `I_cm` |
| 18 | `test_aqwa_pmas_node_matches_declared_cog` | `test_inertia_origin_contract.py` | the emitted reference node is the CoG the transform assumed | same spec | deck-1 node `98000` coordinates equal `c` |
| 19 | `test_aqwa_rejects_inadmissible_transformed_tensor` | `test_inertia_origin_contract.py` | fail-closed at emission, not a warning | tensor whose CoM image violates the triangle inequality | `InertiaReferenceError`, no `.dat` written |
| 20 | `test_orcawave_emits_declared_origin_exactly` | `test_inertia_origin_contract.py` | AC — forward emission is exact for both values | specs with each origin | `BodyInertiaTensorOriginType` is `"Body origin"` / `"Centre of mass"` respectively |
| 21 | `test_orcawave_raises_on_unmappable_origin` | `test_inertia_origin_contract.py` | the `.get(…, "Body origin")` fallback is gone | a `VesselInertia` mutated post-validation to an unknown origin | `ValueError` naming the value |
| 22 | `test_orcawave_radii_origin_declares_centre_of_mass` | `test_inertia_origin_contract.py` | the hardcoded literal at line 340 no longer contradicts `resolver.py:512` | free-floating spec | `BodyRadiiOfGyrationOriginType` matches the resolved contract |
| 23 | `test_orcawave_reverse_rejects_unknown_native_origin` | `test_reverse_parsers.py` | `_reverse_tensor_origin` fails closed | `BodyInertiaTensorOriginType: "Waterline"` | `ValueError`; **must not** return `"body_origin"` |
| 24 | `test_orcawave_round_trip_preserves_body_origin_tensor` | `test_inertia_origin_contract.py` | AC — round-trip preservation, body-origin case | spec → `.yml` → spec | origin and all six components preserved within `1e-9` relative |
| 25 | `test_orcawave_round_trip_preserves_centre_of_mass_tensor` | `test_inertia_origin_contract.py` | AC — round-trip preservation, CoM case | spec → `.yml` → spec | origin and all six components preserved |
| 26 | `test_aqwa_round_trip_preserves_reference_point` | `test_inertia_origin_contract.py` | closes the `reverse_parsers.py:108-112` drop | spec → `.dat` → spec | recovered `inertia_tensor_origin == "centre_of_mass"`, recovered `centre_of_gravity == c` |
| 27 | `test_aqwa_round_trip_recovers_original_body_origin_tensor` | `test_inertia_origin_contract.py` | the transform is invertible through the deck | spec with `I_o`/`body_origin` → `.dat` → spec → transform back | `I_o` within `1e-9` relative |
| 28 | `test_cross_backend_equivalence_body_origin_input` | `test_inertia_origin_contract.py` | AC — equivalent physical models from one spec | one spec, both backends | both decks, reduced to the CoM frame, agree within `1e-9` relative on all six components |
| 29 | `test_cross_backend_equivalence_centre_of_mass_input` | `test_inertia_origin_contract.py` | same, CoM-declared input | one spec, both backends | as above |
| 30 | `test_cross_backend_equivalence_is_origin_declaration_invariant` | `test_inertia_origin_contract.py` | the two declarations of the *same physical body* converge | spec A (`I_o`/`body_origin`) and spec B (`I_cm`/`centre_of_mass`) | AQWA decks byte-equal; OrcaWave CoM-reduced tensors equal within `1e-9` |
| 31 | `test_resolver_contract_constant_is_shared` | `test_resolver.py` | `resolver.py:512` and the schema cannot drift apart | resolver-estimated spec | origin equals the shared module constant, still `"centre_of_mass"` |
| 32 | `test_vessel_deck_builder_declares_origin` | `test_inertia_origin_contract.py` | the tightened schema does not break in-repo constructors | a record driven through `vessel_deck_builder` | validates; origin explicitly declared |
| 33 | `test_parametric_spec_generator_declares_origin` | `test_inertia_origin_contract.py` | same for the parametric path | a generated spec | validates; origin explicitly declared |

Tests 3, 16, 27, 28, 29 and 30 carry the products-of-inertia and offset-CoG coverage the issue's acceptance criteria require. Tests 4, 5, 7, 8 and 19 are conservation-class comparators per `.claude/rules/reproducibility-is-not-correctness.md`: they assert against invariants the result must satisfy regardless of what the code computes, rather than against a golden captured from the code being changed.

---

## Acceptance Criteria

- [ ] Unknown `inertia_tensor_origin` values fail schema validation (tests 10-12), and an inertia tensor supplied without an explicit origin fails validation (test 13).
- [ ] Radii-of-gyration-derived tensors resolve to `centre_of_mass` and a contradictory `body_origin` declaration is rejected (tests 14-15).
- [ ] The AQWA PMAS emission applies the full parallel-axis transformation, products included, and the emitted reference node matches the CoG the transform assumed (tests 16-18).
- [ ] AQWA never silently interprets a body-origin tensor as centre-of-mass-relative: the transform is applied, or an `InertiaReferenceError` is raised and no deck is written (tests 16, 19).
- [ ] OrcaWave emits the declared origin exactly and raises on an unmappable value (tests 20-21); the free-floating radii origin no longer contradicts `resolver.py:512` (test 22).
- [ ] Both reverse parsers preserve the reference point, and both fail closed on unknown native values (tests 23-26).
- [ ] Synthetic body-origin and centre-of-mass inputs describing the same physical body produce physically equivalent backend models (tests 28-30).
- [ ] New tests pass: `uv run pytest tests/hydrodynamics/diffraction/test_inertia_reference.py tests/hydrodynamics/diffraction/test_inertia_origin_contract.py -v`
- [ ] No regression: `uv run pytest tests/hydrodynamics/diffraction/ -q` passes, with `test_reverse_parsers.py::TestOrcaWaveInputParserInertiaOrigin` and `test_resolver.py::test_explicit_inertia_tensor_origin_is_centre_of_mass` still green.
- [ ] `scripts/legal/legal-sanity-scan.sh` passes; the plan and all fixtures carry no client, vessel, or project identifier.
- [ ] The CI lint toolchain is run exactly as configured per `.claude/rules/verify-ci-lint-toolchain.md` before any push.
- [ ] Review artifacts are posted to `scripts/review/results/`.

---

## Adversarial Review Summary

<!-- Filled in after the review pass completes. Do not post to GitHub until this section is populated. -->

| Provider | Verdict | Key findings |
|---|---|---|
| Claude | (pending) | (pending) |
| Codex | (pending) | (pending) |
| Gemini | (pending) | (pending) |

**Overall result:** (pending)

Revisions made based on review:
- (none yet)

---

## Risks and Open Questions

- **Risk — sibling contract defects consume this issue's output.** [#1582](https://github.com/vamseeachanta/digitalmodel/issues/1582) (unit normalization across AQWA and OrcaWave) and [#1580](https://github.com/vamseeachanta/digitalmodel/issues/1580) (solver-neutral restoring-stiffness contract) are both OPEN at `status:needs-plan` and both consume the reference-point semantics defined here. A stiffness or damping matrix is meaningless without its reference point, and `orcawave_backend.py:375` and `:390` currently hardcode `"Body origin"` for exactly those two matrices. The `Literal` type and the transform module introduced here should be written so #1580 can reuse them without a second definition; conversely, if #1580 lands first with its own origin enum, the two will have to be reconciled. Sequencing should be surfaced at approval.
- **Risk — out-of-scope boundary.** Output-unit normalization remains owned by [#1550](https://github.com/vamseeachanta/digitalmodel/issues/1550) (CLOSED) per this issue's own Boundaries section. The inertia unit conversions at `orcawave_backend.py:215-220` and `reverse_parsers.py:718-723` will be left untouched; the transform module will operate in the spec's native `kg·m²` and the existing conversion will be applied after it, so a reviewer must confirm no double conversion is introduced.
- **Risk — the AQWA PMAS reference-point semantics are asserted from our own code, not from the vendor manual.** The evidence that the emitted tensor is CoG-referenced is that `aqwa_backend.py:499-506` places node `98000` at the CoG and `:623` comments it as "AQWA convention for CoG". Per `.claude/rules/mechanism-before-publication.md`, that is a hypothesis about the solver until the AQWA reference manual's PMAS card definition is read. If PMAS proves to be structure-origin-referenced, the transform direction inverts and tests 16-18 invert with it. The transform module will therefore be written direction-agnostic, with the direction chosen at a single call site, so an inverted finding is a one-line change plus test updates rather than a redesign. **This must be resolved before implementation begins.**
- **Risk — `BodyRadiiOfGyrationOriginType` admissible values are unverified.** Changing `orcawave_backend.py:340` from the hardcoded `"Body origin"` to a centre-of-mass declaration is a physics change to every free-floating deck the package emits. If OrcaWave's key does not admit a centre-of-mass value for a free-floating body, the fail-closed alternative applies: keep today's emission and raise when the spec declares a contract the native key cannot express. The plan will not flip the literal on the strength of `resolver.py:508-512` alone.
- **Risk — schema tightening is a breaking change for external callers.** Requiring an explicit origin whenever `inertia_tensor` is supplied will reject specs that validate today. In-repo blast radius is bounded and measured: 8 code sites and **zero** `.yml`/`.yaml`/`.json` fixtures (Gap proof G9), and three in-repo constructors (`reverse_parsers.py:108`, `vessel_deck_builder.py:191`/`:223`, `parametric_spec_generator.py:307`) will be updated in the same change. External consumers of `pip install digitalmodel` will see a `ValidationError`. This is the fail-closed posture the issue asks for — the alternative, defaulting to `body_origin`, is the exact behaviour that produced the defect — but the break should be called out in the release notes and confirmed at approval.
- **Risk — complexity escalation.** T2 is assigned on the measured blast radius. If the AQWA manual check inverts the transform direction, or if the OrcaWave radii-origin question forces a native-file verification against a licensed install, the scope crosses into T3 and a third review provider should be added.
- **Open:** should the transform be applied at emission time (both backends), or should the spec be canonicalised to a single reference point at resolve time and the backends emit whatever the canonical frame gives them? Canonicalising at resolve time is simpler but loses the author's declared frame on round trip, which acceptance criterion 2 forbids. This plan proposes emission-time transformation with the declared frame preserved on the spec. Flagged for the user at approval.
- **Open:** should `InertiaReferenceError` subclass `ValueError` so existing `except ValueError` handlers in `spec_converter.py` and the CLI keep working, or be a distinct exception so callers must handle it explicitly? Fail-closed bias argues for a distinct type; compatibility argues for the subclass. Proposed: subclass `ValueError` with a distinct name, so neither property is lost.
- **Open:** the empirical tolerance `1e-9` relative is proposed on the basis that every operation is a double-precision arithmetic transform with no solver in the loop. It will be fixed in the test module before the implementation is written, per `.claude/rules/reproducibility-is-not-correctness.md` item 4, and must not be relaxed afterwards to accommodate an observed discrepancy.

---

## Complexity: T2

**T2** — the change spans six source files and five test files, but all within one package, with one small new module and no new architecture, no cross-repo or cross-provider harness surface, and a measured blast radius of 8 code sites and zero committed spec fixtures (Gap proof G9). The physics is a single classical transform with a closed-form check available. It exceeds T1 because it is multi-file, changes a public schema in a breaking direction, and requires cross-backend equivalence tests rather than single-module coverage. It falls short of T3 because nothing is systemic beyond the diffraction package. Two named conditions would escalate it: an inverted AQWA PMAS reference-point finding, or a licensed-install verification requirement for the OrcaWave radii-origin key.
