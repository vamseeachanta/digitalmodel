# Units convention — force, moment, pressure (SI, kN↔N)

> Scope: `src/digitalmodel/{drilling_riser, orcaflex, subsea/mooring_analysis,
> motion_forecast}`. Enforced by
> `tests/contracts/test_force_units_boundary_contract.py` (`@pytest.mark.contracts`).
> Origin: issue #1447 — hardening against the kN↔N (factor-1000) trap that caused
> three silent incidents in the drilling-riser twin epic (#1372).

## Why this exists

The factor-1000 between kilonewtons and newtons is invisible to the type system:
`float` is `float`. Three incidents shipped where kN met N at an unenforced seam:

| Incident | Seam | Symptom |
|----------|------|---------|
| #1373 / PR#1379 | telemetry schema | kN fields under an "SI internally" claim |
| #1375 / PR#1410 | `drift_off.py` env-force (kN) vs `DPState` thrust (N) | **silently reported "station held" for a vessel that actually drifts off** |
| #1376 / PR#1422 | `code_check_engine` moment (kN·m) | kN·m moment mistaken for a dimensionless UC |

Each was caught only by an adversarial plan reviewer — never by CI. This
convention plus its contract test make the seam machine-checkable.

## Rule 1 — internal SI, conversions at a named boundary

- The twin / motion-forecast / analytical-engine layers are **SI internally**:
  newtons (N), newton-metres (N·m), pascals (Pa).
- kN / kN·m / MPa are permitted **only at legacy `orcaflex` / `mooring` interfaces**
  (their public parameters and library data are historically kN).
- Every conversion happens **once, at a single named boundary**, via a
  module-level constant — never an inline unexplained `* 1000` scattered through
  the math. The canonical pattern is `telemetry_inputs.py`:

  ```python
  _KN_TO_N = 1000.0

  def _kn_to_n(value):
      return None if value is None else float(value) * _KN_TO_N
  ```

## Rule 2 — every bare-float force name carries a unit suffix

Any function parameter, dataclass field, or pydantic field that is a **bare
float** (`float` / `Optional[float]`, or a bare numeric default) and whose name
contains one of:

> `tension` · `force` · `thrust` · `moment` · `mbl` · `pretension` ·
> `capacity` · `load`

**must** end in one of these unit suffixes:

| Suffix | Unit | | Suffix | Unit |
|--------|------|-|--------|------|
| `_n`   | newton (N)      | | `_pa`  | pascal (Pa) |
| `_kn`  | kilonewton (kN) | | `_kpa` | kilopascal (kPa) |
| `_nm`  | newton-metre (N·m) | | `_mpa` | megapascal (MPa) |
| `_knm` | kilonewton-metre (kN·m) | | | |

Suffix matching is case-insensitive, so legacy spellings `_kN`, `_kNm`, `_MPa`
are honoured. Examples:

```python
top_tension_n: float          # ✅ newtons
total_available_thrust_kn: float   # ✅ kilonewtons
sling_mbl_kN: float           # ✅ legacy spelling, accepted
max_tension: float            # ❌ unitless — which is it, kN or N?
```

The rule applies to **all new** force-class names in the four packages, including
SI-native modules. Do **not** rename existing public parameters to satisfy it —
the pre-existing offenders (SI-native names like `static_tension`, and legacy kN
interfaces like `mooring_design.check_mbl`'s `max_tension_kn` inputs whose kN-ness
lives in a comment) are **frozen** in the test's `LEGACY_ALLOWLIST`
(`# frozen 2026-07-06, do not extend`). New code must carry the suffix; the
allowlist must never grow.

## Rule 3 — cross a magnitude window at the seam

Force values that cross a kN↔N boundary should be sanity-checked against a
plausible magnitude window (the contract's `PLAUSIBLE` table). Each window is
chosen so that both a ×1000 and a ÷1000 corruption of a typical value fall
outside it — a factor-1000 slip then fails loudly instead of silently.

## What the contract test enforces

1. **AST naming scan** over the four packages — a new unsuffixed force-class
   bare-float name fails the build (frozen legacy names are allowlisted).
2. **Five boundary conversion checks**, each with a hand-calculated expectation
   and a documented ×1000/÷1000 mutation-rejection proof:
   `envelope.py` (N→kN), `drift_off.py` (kN→N GO/NO-GO flip — the #1375 guard),
   `telemetry_inputs.py` (parse), `mooring_design.check_mbl` (kN), and
   `installation_analysis.calculate_sling_tension` (kN).
3. A **central plausible-magnitude window table** shared by the boundary checks.

## Out of scope (deferred)

- Migrating mooring / riser / orcaflex modules to the shared pint registry
  (`digitalmodel.units`) — that is workspace-hub#1484's thread. This convention
  guards the seams; it does not pint-ify them.
- Force nouns outside the Rule-2 regex (`weight`, `buoyancy`, `slam`, `daf`, …) —
  covered only where they cross a numeric seam in a boundary test.
- Label-only kN touchpoints that are strings, not conversions (e.g.
  `metric="max_tension_kN"`).
- kPa/Pa pressure seams beyond those surfaced by the scan.
