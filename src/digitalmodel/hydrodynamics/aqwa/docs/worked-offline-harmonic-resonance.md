# Worked offline harmonic resonance analysis

## Summary

A synthetic translational oscillator with constant effective mass, stiffness
and linear damping gives an undamped natural frequency of 2.0 rad/s, a period
of 3.1 s and a damping ratio of 0.2. These are hand-calculated results from the
assumptions below and the shipped [E4](damping-basis.md#e4-resonance) and
[E5](damping-basis.md#e5-critical-damping-and-additions) derivations. The example
calls `critical_dof` on a grid that brackets the analytic root and compares its
outputs with independent scalar arithmetic. Execution requires only Python,
NumPy and SciPy; it reads the offline helper and writes a result in a supplied
temporary directory.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-25 | Added a synthetic oscillator and executable comparisons to demonstrate the offline resonance and damping interface. |

## Inputs and numerical assumptions

All dimensional inputs and frequency samples in this table are **assumed,
synthetic demonstration values**; they are not measurements or design data.
The mass labels follow the helper interface: `inertia` is the dry translational
mass and `added_mass` is the constant added translational mass for `heave`.

| Quantity | Symbol | Assumed value | Unit | Basis |
| --- | --- | --- | --- | --- |
| Dry mass | m | 3.0 | kg | Synthetic assumption |
| Added mass | a | 2.0 | kg | Synthetic assumption |
| Restoring stiffness | K | 20.0 | N/m | Synthetic assumption |
| Linear damping | B | 4.0 | N s/m | Synthetic assumption, identical at every sample |
| Bracketing angular-frequency grid | omega | [1.0, 1.5, 2.5, 3.0] | rad/s | Synthetic assumption; the root lies strictly between the middle samples |
| Exclusion grid for refusal example | omega | [2.5, 3.0, 4.0] | rad/s | Synthetic assumption; all samples exceed the root |
| Relative comparison tolerance | r | 1e-9 | dimensionless | Chosen numerical acceptance assumption |
| Absolute comparison tolerance | e | 1e-12 | each compared quantity's SI unit | Chosen numerical acceptance assumption |

The tolerances allow floating-point arithmetic and iterative root finding while
remaining below the precision of the stated inputs. They describe numerical
agreement for this example, not physical accuracy. Comparisons use
`abs(actual - expected) <= max(r * abs(expected), e)`.

No empirical physical constants or unit scale conversions are used. The factor
`2*pi` is one full harmonic cycle, as used by E5 for `T_n = 2*pi/omega_n`;
`math.pi` supplies the mathematical constant pi. The factor `2` in critical
damping follows the repeated-root derivation below. Zeros, signs and powers in
the equations are algebraic identities rather than fitted constants. The
revision number, date and Python indexing values are metadata or control values.

## Derivation and independent arithmetic

Let q be displacement in metres and t time in seconds. With effective mass
M = m + a = 5.0 kg, the constant-coefficient equation is

\[
M\ddot q + B\dot q + Kq = 0.
\]

For the undamped natural frequency, set B to zero and substitute
`q = Q exp(i*omega*t)`. A nonzero Q requires `K - M*omega**2 = 0`, so
`omega_n = sqrt(K/M) = sqrt(20/5) = 2.0 rad/s`. This is E4 with constant added
mass. The grid brackets the positive root because `M*omega**2 - K` is negative
at 1.5 rad/s and positive at 2.5 rad/s. It is strictly increasing for positive
omega, so this root is unique. The period is `2*pi/omega_n = pi s`, displayed
as 3.1 s following E5. The helper's natural frequency is the **undamped**
frequency; the damped transient frequency and forced-response peak are different
quantities and are not evaluated here.

For critical damping substitute `q = Q exp(s*t)`. The characteristic polynomial
is `M*s**2 + B*s + K = 0`. A repeated root requires the discriminant
`B**2 - 4*M*K = 0`; choosing nonnegative damping gives
`B_critical = 2*sqrt(M*K) = 2*M*omega_n = 20.0 N s/m`, as in E5.
The critical repeated root is `s = -B_critical/(2*M) = -omega_n`.
Thus `zeta = B/B_critical = 4/20 = 0.2`.

The damping is constant but is still supplied as a sampled vector. At the
in-grid root, linear interpolation gives
`B_n = 4 + (4 - 4)*(2 - 1.5)/(2.5 - 1.5) = 4.0 N s/m`.
Every sampled critical-damping ratio and the interpolated resonant ratio is
therefore 0.2. This checks the interpolation path with a constant function;
it does not test a varying damping law.

| Quantity | Independent calculation | Displayed result | Basis |
| --- | --- | --- | --- |
| Effective mass | m + a | 5.0 kg | Input assumptions |
| Undamped natural frequency | sqrt(K/M) | 2.0 rad/s | E4 and input assumptions |
| Natural period | 2*pi/omega_n | 3.1 s | E5 and input assumptions |
| Critical damping | 2*sqrt(M*K) | 20.0 N s/m | E5 repeated-root condition |
| Damping at natural frequency | Linear interpolation of constant B | 4.0 N s/m | Input assumptions and interpolation arithmetic above |
| Sampled and resonant damping ratios | B/B_critical | 0.2 | E5 and input assumptions |

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

parser = argparse.ArgumentParser(description="Synthetic offline oscillator")
parser.add_argument("source_root", type=Path)
parser.add_argument("output_directory", type=Path)
# Synthetic grid and coefficients: Inputs and numerical assumptions above.
parser.add_argument("--omega", nargs="+", type=float, default=[1.0, 1.5, 2.5, 3.0])
args = parser.parse_args()

helper_path = (
    args.source_root / "src/digitalmodel/hydrodynamics/aqwa/damping_physics.py"
)
spec = importlib.util.spec_from_file_location("offline_damping", helper_path)
helper = importlib.util.module_from_spec(spec)
spec.loader.exec_module(helper)

dry_mass = 3.0
added_mass = 2.0
stiffness = 20.0
damping = 4.0
relative_tolerance = 1e-9
absolute_tolerance = 1e-12

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
report = {"omega_rad_s": omega.tolist(), "actual": actual, "expected": expected}
(args.output_directory / "resonance.json").write_text(
    json.dumps(report, indent=2, allow_nan=False) + "\n", encoding="utf-8"
)
```

To demonstrate refusal without extrapolation, append
`--omega 2.5 3.0 4.0` to the same invocation, using a fresh output directory.
The unchanged block raises
`ValueError: heave: expected one resonance inside frequency grid; found 0`.
It creates no output directory or result on that failure. The exclusion grid's
lowest frequency exceeds the independently derived root; the refusal therefore
does not depend on an expected value read from the helper.

## Limits and way forward

This example checks a synthetic, uncoupled, constant-coefficient linear
oscillator. It establishes neither frequency-dependent hydrodynamic accuracy
nor nonlinear, stochastic, coupled-motion or solver validation. No executable,
licence check, external-force server or listing is used. The word resonance
here refers only to the undamped E4 root. Only one positive in-grid root is
permitted; there is no extrapolation or selection among multiple roots.

The existing damping audit tests remain the unit-test baseline. This worked
example adds executable documentation rather than asserting missing unit
coverage. A broader module report, builder and index remain separate work.
