# Worked offline harmonic resonance analysis

## Summary

A public textbook spring-mass example with constant coefficients gives an
undamped natural frequency of 7.0 rad/s, a period of 0.9 s and a damping ratio
of 1.0. The input basis is [Calculus Volume 3, 2016 edition, section 7.3,
Example 7.20][S1]. The independent arithmetic below also follows the shipped
[E4](damping-basis.md#e4-resonance) and
[E5](damping-basis.md#e5-critical-damping-and-additions) derivations. The example
calls `critical_dof` on a grid that brackets the analytic root and compares its
outputs with independent scalar arithmetic. Execution requires only Python,
NumPy and SciPy; it reads the offline helper and writes a result in a supplied
temporary directory.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-25 | Added a synthetic oscillator and executable comparisons to demonstrate the offline resonance and damping interface. |
| 2 | 2026-10-25 | Replaced assumed coefficients with a cited public worked example and derived the grids and numerical error budget to establish input provenance. |

## Inputs and numerical assumptions

The physical coefficients are taken from [S1], whose worked solution is
`q'' + 14*q' + 49*q = 0` for a 1 kg mass. They are textbook model inputs,
not measurements or design data. The helper's `inertia` receives the source's
lumped translational mass; `added_mass` is zero because the source equation
contains no separate hydrodynamic mass. The `heave` key selects translation
in the helper; it does not qualify this textbook model as hydrodynamic data.

| Quantity | Symbol | Value | Unit | Basis |
| --- | --- | --- | --- | --- |
| Lumped mass | m | 1.0 | kg | [S1], Example 7.20 |
| Added mass | a | 0.0 | kg | Mapping of the single source mass, with no additional mass term |
| Restoring stiffness | K | 49.0 | N/m | [S1], Example 7.20 worked solution |
| Linear damping | B | 14.0 | N s/m | [S1], Example 7.20, identical at every sample |
| Bracketing angular-frequency grid | omega | [3.5, 5.25, 8.75, 10.5] | rad/s | Derived as [1/2, 3/4, 5/4, 3/2] times the 7 rad/s analytic root |
| Exclusion grid for refusal example | omega | [8.75, 10.5, 14.0] | rad/s | Derived as [5/4, 3/2, 2] times the same root |
| Relative comparison tolerance | r | 1e-12 | dimensionless | Rounded-up propagated root-error budget below |
| Absolute comparison tolerance | e | 0.0 | each compared quantity's SI unit | All expected outputs are nonzero; only relative error is used |

The grids are computational sampling choices derived from the analytic root,
not additional physical inputs. Half-root and quarter-root offsets place the
root strictly inside the central interval; the exclusion grid starts above it.

The [SciPy 1.14.1 reference, `brentq` parameters `xtol` and `rtol`][S2]
specifies root error at most `xtol + rtol*abs(omega_n)`, with defaults
`xtol = 2e-12 rad/s` and `rtol = 4*epsilon`, where binary64
`epsilon = 2**-52`. At the exact 7 rad/s root, the relative error bound is
`rho = 2e-12/7 + 4*epsilon`. Critical damping has the same relative
sensitivity as frequency; period and damping ratio have reciprocal sensitivity
bounded by `rho/(1-rho)`. A reserve of `32*epsilon` allows more rounded
operations than the scalar expressions and constant interpolation outside the
root solver require. The combined budget `rho/(1-rho) + 32*epsilon` is below
`3e-13`; rounding upward to the next decimal power gives `r = 1e-12`.
The test checks that budget against the installed solver defaults, so changed
defaults cannot silently invalidate the stated criterion.

These tolerances describe numerical agreement, not physical accuracy.
Comparisons use
`abs(actual - expected) <= max(r * abs(expected), e)`.

No empirical physical constants or unit scale conversions are used. The factor
`2*pi` is one full harmonic cycle, as used by E5 for `T_n = 2*pi/omega_n`;
`math.pi` supplies the mathematical constant pi. The factor `2` in critical
damping follows the repeated-root derivation below. Zeros, signs and powers in
the equations are algebraic identities rather than fitted constants. The
revision number, date and Python indexing values are metadata or control values.

## Derivation and independent arithmetic

Let q be displacement in metres and t time in seconds. With effective mass
M = m + a = 1.0 kg, the constant-coefficient equation from [S1], section 7.3,
equation (7.9), is

\[
M\ddot q + B\dot q + Kq = 0.
\]

For the undamped natural frequency, set B to zero and substitute
`q = Q exp(i*omega*t)`. A nonzero Q requires `K - M*omega**2 = 0`, so
`omega_n = sqrt(K/M) = sqrt(49/1) = 7.0 rad/s`, as in [S1], equation (7.5).
This is E4 with zero added mass. The grid brackets the positive root because
`M*omega**2 - K` is negative at 5.25 rad/s and positive at 8.75 rad/s.
It is strictly increasing for positive omega, so this root is unique.
The period is `2*pi/omega_n = 2*pi/7 s`, displayed as 0.9 s following
[S1], section 7.3, and E5. The helper's natural frequency is the **undamped**
frequency; the damped transient frequency and forced-response peak are different
quantities and are not evaluated here.

For critical damping substitute `q = Q exp(s*t)`. The characteristic polynomial
is `M*s**2 + B*s + K = 0`. A repeated root requires the discriminant
`B**2 - 4*M*K = 0`; choosing nonnegative damping gives
`B_critical = 2*sqrt(M*K) = 2*M*omega_n = 14.0 N s/m`, as in
[S1], section 7.3, Case 2, and E5.
The critical repeated root is `s = -B_critical/(2*M) = -omega_n`.
Thus `zeta = B/B_critical = 14/14 = 1.0`: the source system is critically
damped. Its transient response does not oscillate; the calculated natural
period describes the undamped reference system only.

The damping is constant but is still supplied as a sampled vector. At the
in-grid root, linear interpolation gives
`B_n = 14 + (14 - 14)*(7 - 5.25)/(8.75 - 5.25) = 14.0 N s/m`.
Every sampled critical-damping ratio and the interpolated resonant ratio is
therefore 1.0. This checks the interpolation path with a constant function;
it does not test a varying damping law.

| Quantity | Independent calculation | Displayed result | Basis |
| --- | --- | --- | --- |
| Effective mass | m + a | 1.0 kg | [S1] lumped mass |
| Undamped natural frequency | sqrt(K/M) | 7.0 rad/s | [S1], equation (7.5), and E4 |
| Natural period | 2*pi/omega_n | 0.9 s | [S1], section 7.3, and E5 |
| Critical damping | 2*sqrt(M*K) | 14.0 N s/m | [S1], section 7.3, Case 2, and E5 |
| Damping at natural frequency | Linear interpolation of constant B | 14.0 N s/m | [S1] coefficient and interpolation arithmetic above |
| Sampled and resonant damping ratios | B/B_critical | 1.0 | [S1] coefficients and repeated-root condition |

## Executable example

Copy the single Python block below into a script stored in a temporary
directory. Run it with two positional arguments: the repository root containing
`src`, then a supplied temporary output directory. The current working directory
may be anywhere. For example, with those paths supplied through shell variables:
`python "$EXAMPLE_SCRIPT" "$REPOSITORY_ROOT" "$OUTPUT_DIRECTORY"`.
The script reads `damping_physics.py` directly, avoiding package initializers,
and creates only `resonance.json` inside the supplied output directory.
The tests extract and execute this exact block with an isolated interpreter.

```python
import argparse
import importlib.util
import json
import math
from pathlib import Path

parser = argparse.ArgumentParser(description="Public textbook offline oscillator")
parser.add_argument("source_root", type=Path)
parser.add_argument("output_directory", type=Path)
# Grid: [1/2, 3/4, 5/4, 3/2] * 7 rad/s; see the input basis above.
parser.add_argument("--omega", nargs="+", type=float, default=[3.5, 5.25, 8.75, 10.5])
args = parser.parse_args()

helper_path = (
    args.source_root / "src/digitalmodel/hydrodynamics/aqwa/damping_physics.py"
)
spec = importlib.util.spec_from_file_location("offline_damping", helper_path)
helper = importlib.util.module_from_spec(spec)
spec.loader.exec_module(helper)

# Calculus Volume 3 (2016), section 7.3, Example 7.20.
dry_mass = 1.0
added_mass = 0.0
stiffness = 49.0
damping = 14.0
# Rounded-up brentq error budget derived above, not measurement uncertainty.
relative_tolerance = 1e-12
absolute_tolerance = 0.0

# Independent scalar arithmetic from the differential equation and E4-E5.
effective_mass = dry_mass + added_mass
analytic_frequency = math.sqrt(stiffness / effective_mass)
analytic_critical = 2.0 * math.sqrt(effective_mass * stiffness)
expected = {
    "effective_mass_kg": effective_mass,
    "natural_frequency_rad_s": analytic_frequency,
    "natural_period_s": 2.0 * math.pi / analytic_frequency,
    "critical_damping_ns_m": analytic_critical,
    "damping_at_natural_frequency": damping,
    "zeta_at_natural_frequency": damping / analytic_critical,
}

omega = helper.frequency_grid(args.omega)
result = helper.critical_dof(
    omega, [damping] * len(omega), dry_mass, stiffness, added_mass, "heave"
)
actual = {
    "effective_mass_kg": result["denominator"],
    "natural_frequency_rad_s": result["natural_frequency_rad_s"],
    "natural_period_s": result["natural_period_s"],
    # The helper returns effective mass but not critical damping itself (E5).
    "critical_damping_ns_m": (
        2.0 * result["denominator"] * result["natural_frequency_rad_s"]
    ),
    "damping_at_natural_frequency": result["damping_at_natural_frequency"],
    "zeta_at_natural_frequency": result["zeta_at_natural_frequency"],
    "critical_damping_ratio": result["critical_damping_ratio"].tolist(),
}
for name, reference in expected.items():
    if not math.isclose(
        actual[name], reference,
        rel_tol=relative_tolerance, abs_tol=absolute_tolerance,
    ):
        raise AssertionError(f"{name} differs from independent arithmetic")
for ratio in actual["critical_damping_ratio"]:
    if not math.isclose(
        ratio, expected["zeta_at_natural_frequency"],
        rel_tol=relative_tolerance, abs_tol=absolute_tolerance,
    ):
        raise AssertionError("Sampled damping ratio differs from E5")

args.output_directory.mkdir(parents=True, exist_ok=True)
report = {
    "omega_rad_s": omega.tolist(), "actual": actual, "expected": expected,
    "comparison": {
        "relative_tolerance": relative_tolerance,
        "absolute_tolerance": absolute_tolerance,
    },
}
(args.output_directory / "resonance.json").write_text(
    json.dumps(report, indent=2, allow_nan=False) + "\n",
    encoding="utf-8", newline="\n",
)
```

To demonstrate refusal without extrapolation, append
`--omega 8.75 10.5 14.0` to the same invocation, using a fresh output directory.
The unchanged block raises
`ValueError: heave: expected one resonance inside frequency grid; found 0`.
It creates no output directory or result on that failure. The exclusion grid's
lowest frequency exceeds the independently derived root; the refusal therefore
does not depend on an expected value read from the helper.

## Limits and way forward

This example checks a cited, uncoupled, constant-coefficient linear
oscillator. It establishes neither frequency-dependent hydrodynamic accuracy
nor nonlinear, stochastic, coupled-motion or solver validation. No solver executable,
licence check, external-force server or listing is used. The word resonance
here refers only to the undamped E4 root. Only one positive in-grid root is
permitted; there is no extrapolation or selection among multiple roots.

The existing damping audit tests remain the unit-test baseline. This worked
example adds executable documentation rather than asserting missing unit
coverage. A broader module report, builder and index remain separate work.

## Sources

[S1]: https://openstax.org/books/calculus-volume-3/pages/7-3-applications
[S2]: https://docs.scipy.org/doc/scipy-1.14.1/reference/generated/scipy.optimize.brentq.html

- [S1] OpenStax, *Calculus Volume 3*, 2016 edition, section 7.3
  (Applications), Example 7.20 (Critically Damped Spring-Mass System),
  equations (7.5) and (7.9), and Case 2. Accessed 2026-10-25.
- [S2] *SciPy Reference Guide*, version 1.14.1, `scipy.optimize.brentq`,
  Parameters: `xtol` and `rtol`. Accessed 2026-10-25.
