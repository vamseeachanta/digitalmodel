# Liquid valve sizing qualification — slice 1

The liquid sizing change rejects unrepresentable positive results while
preserving physical validation, exact-zero flow and the exponent scaling from
#2855. The bounded qualification suite passes 300 cases. Main integration
remains unresolved; this record does not establish full-suite success or
equipment suitability.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-26 | Record the equations, independent expected values, current test counts and remaining integration work for liquid sizing. |

## Plan-lite

### Files and scope

| File | Responsibility |
| --- | --- |
| [valve_sizing.py](../../src/digitalmodel/subsea/hipps/valve_sizing.py) | Shared liquid validation, exponent-scaled choking differential, capacity and fixed-factor coefficient inversion. |
| [test_liquid_sizing.py](../../tests/subsea/hipps/test_liquid_sizing.py) | 104 liquid-qualification cases, including published arithmetic, constructed range probes, exact zeros and invalid inputs. |
| [test_valve_sizing.py](../../tests/subsea/hipps/test_valve_sizing.py) | 11 preserved #2855 regression cases for representable extreme differentials, unrepresentable final differentials and invalid pressure heads. |

Public signatures and result shapes are preserved. Unit constants, properties
and adjusted equipment factors remain caller inputs. No equipment default or
design acceptance limit is introduced.

### Equations and applicability

The public [Control Valve Handbook, sixth edition (August 2023)](https://www.emerson.com/is/content/emerson/es/final-control/isolation-valves/documents/control-valve-handbook-en-3661206.pdf),
section 5.8.2, p. 113, gives the attached-fittings choking differential and the
pressure drop used for sizing. Section 5.8.3 gives the liquid coefficient
relation; its algebraic rearrangement gives capacity:

```text
dp_max = (FLP/Fp)^2 * (p1 - FF*pv)
dp_sizing = min(p1 - p2, dp_max)
q = N1 * Fp * C * sqrt(dp_sizing/SG)
C = q / (N1 * Fp * sqrt(dp_sizing/SG))
```

Here `SG` is the fluid-to-reference-water density ratio. `C` denotes Cv or Kv
only with the corresponding caller-supplied unit constant `N1`. Pressures use
consistent units; `p1`, `p2` and `pv` are absolute pressures. Factors remain
fixed during inversion.

The [Control Valve Sourcebook — Oil & Gas (2013)](https://www.emerson.com/is/content/emerson/en/final-control/pressure-management/jt-valves/documents/guide-control-valve-sourcebook-oil-gas-fisher-en-135836.pdf),
chapter 3, pp. 3-3 and 3-5, gives the same flow and attached-fittings relations.
Handbook section 5.5 bounds applicability to fully turbulent, single-phase
Newtonian flow. Both public PDFs returned HTTP 200 when rechecked on
2026-10-26 UTC; equation pages were inspected. Response addresses, timestamps
and source copies are retained with the check evidence outside the repository.

### Independent expected values

Expected values come from the cited example or constructed arithmetic written
in each test's docstring. Production sizing functions do not generate the
expected values.

| Cases | Expected-value derivation |
| --- | --- |
| Normalized choking and inversion | With `N1=Fp=SG=FF=1`, `FLP=0.5`, `p1=100` and `pv=0`, `dp_max=25`. For `C=2`, sizing drops 16 and 25 give rates 8 and 10; inversion gives `C=2`. |
| Published propane example | Handbook section 5.8.4, pp. 114–115: rounded inputs give `dp=314.7-289.7=25` psi, `dp_max=(0.81/0.90)^2*(314.7-0.83*124.3)=171.3` psi to one decimal place, and `Cv=800/(0.90*sqrt(25/0.50))=125.7` to one decimal place. The test compares against published Cv 125.7 with absolute tolerance 0.05 and no relative tolerance. |
| Decimal scaling | `FLP=1e-200` and `p1=1e300`, with the other factors normalized, give `dp_max=1e-400*1e300=1e-100`, `q=1e-50` and inverse coefficient 1. |
| Binary scaling | With `FLP=2^r`, `Fp=2^f`, `p1=2^p`, `C=N1=SG=FF=1` and `p2=pv=0`, derive `dp_max=2^(2r-2f+p)`, then `dp_sizing=min(2^p,dp_max)` and `q=2^(f+log2(dp_sizing)/2)`. Powers and square roots in these cases are exact in binary64. |
| Exact zeros | After physical validation, `C=0` or `dp_sizing=0` gives zero flow; zero demand gives zero required coefficient even when unit-coefficient capacity is unrepresentable. Effective drop and choking flags are checked separately. |
| Floating-point range | Stated powers produce unrepresentable positive results, such as `q=1e309` or `q=5e-400`; these require ValueError. The choking-overflow probe has final differential `1e402`, beyond binary64 range. |
| Invalid physical inputs | Non-finite or invalid factors, reverse flow, invalid liquid inlet and nonpositive or non-finite pressure head require ValueError, including at zero demand. |

### Compatibility with #2855

The shared validator checks the pressure head before evaluating the choking
differential. It preserves #2855's `frexp`/`ldexp` exponent scaling and maps
overflow of the final differential to ValueError. Capacity and zero-demand
inversion use this shared validator. Exact-zero capacity returns after
validation and before overflow-prone products.

All 11 #2855 cases are retained byte-for-byte from the inspected main reference.
Passing them establishes numerical compatibility for those cases. GitHub still
reports a branch conflict; main has not been merged into this branch. The
maintainers must resolve integration while preserving both changes and rerun
the suite before merging.

### Validation

| Group | Passed |
| --- | ---: |
| Liquid qualification | 104 |
| Preserved #2855 regressions | 11 |
| Existing HIPPS cases | 37 |
| Existing capping-stack cases | 32 |
| Existing pipeline-pressure cases | 116 |
| Total | 300 |

The existing groups total 185 cases. The suite has zero failures and zero
skips. The isolated command below uses the provided uv executable; `SCRATCH`
denotes an external temporary directory:

```sh
uv run --no-project --with pytest --with pyyaml python -m pytest tests/subsea/hipps tests/subsea/capping_stack tests/subsea/pipeline/test_pipeline_pressure.py -q -rA -p no:cacheprovider --basetemp="$SCRATCH/bt-final"
```

### Later slices and limits

Later slices cover gas sizing and catalogue iteration, transient benchmarks
and sampled pressure-rate interpretation, and closure/actuator inputs with
pipeline integration against readable standards evidence.

API 17O/IEC 61511 clause-level compliance, equipment-factor qualification,
viscous/multiphase applicability and cavitation adequacy remain NOT_EVALUATED.
Choking status does not establish adequacy. Other positive-flow intermediate
range failures may require input-unit rescaling even where the final
real-valued result is representable. Gas arithmetic and full-suite success
remain outside this evidence.
