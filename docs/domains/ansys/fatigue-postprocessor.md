# ANSYS fatigue postprocessor

`digitalmodel.ansys.fatigue_postprocessor` consumes stress ranges in MPa and
cycle counts supplied by the caller. It evaluates S-N life and cumulative
Miner damage without an ANSYS installation. Generated APDL is command text;
this module neither runs a solver nor establishes a general stress range from
equivalent-stress scalars.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-22 | Document the implemented capabilities and add an executable analytical example to distinguish calculation from design acceptance. |

## Capability inventory

| Item | Implemented boundary |
| --- | --- |
| Built-in curves | DNV-RP-C203 air classes B1, B2, C, C1, D, E, F, F1, F3, W1, W3; custom curves are accepted. Other design-code families are not supplied. |
| Unknown class | Raises `KeyError` and lists valid classes. |
| Environment | `SNcurve.environment` is metadata only. Relabelling an air curve does not supply seawater or corrosion parameters. Legacy labels remain accepted. |
| Threshold | `threshold_mpa` is retained as metadata. Positive ranges use S-N extrapolation; non-positive ranges return infinite cycles. No constant-amplitude endurance cut-off is implemented. |
| Cycle counting | Ranges and cycle counts are inputs; no time-history counting is performed. |
| Damage | Applies the configured SCF, optional thickness correction, Miner summation and design fatigue factor. |
| Extraction | Generates signed differences between equivalent-stress scalars. These can be negative and can underestimate the equivalent stress of a component difference. |
| S-N APDL export | Defines parameters only; it does not evaluate fatigue or apply a threshold. |
| Contours | No contour generation function is provided. |

The numerical table already in the module cites DNV-RP-C203, *Fatigue Design
of Offshore Steel Structures*, October 2011, clause 2.4.4, Table 2-1 (air).
The comparison environment is clause 2.4.5, Table 2-2 (seawater with cathodic
protection). The edition and locators are retained from the repository's
existing source record; this guide does not establish equivalence with a later
edition or claim a new inspection of the original standard.
See the [stored table and evaluator](../../../src/digitalmodel/ansys/fatigue_postprocessor.py)
and the separate [repository S-N transcription](../../../src/digitalmodel/fatigue/c203_sn_tables.py).
No additional standard values are transcribed here.

## Method and analytical inputs

The evaluator uses `log10(N) = log_a - m * log10(S)`. It first evaluates the
first segment and retains it when that cycle count is at or below
`N_transition`; otherwise it evaluates the second segment. This describes the
implemented selection rule, including its use of rounded tabulated intercepts.
The table's nominal transition need not be the exact intersection after rounding.

Miner summation is `D = sum(n_i / N_i)`. The implementation reports allowable
damage `1 / DFF`, utilization `D * DFF`, and life `1 / D` in years when the
input cycle counts represent one year. These equations and the period assumption
are stated in `calculate_fatigue_damage` in the linked evaluator; the separate
[damage implementation](../../../src/digitalmodel/fatigue/damage.py) also documents
Miner summation and annual-cycle scaling. Neither implementation is independent
validation of a design code.

For a hand-checkable example, choose the synthetic curves `N1 = 10**6 / S**2`
and `N2 = 10**7 / S**3`. The coefficients are analytical inputs, not material
data or acceptance limits. They meet at `S = 10 MPa`, `N = 10000` cycles.
Choose annual ranges and counts as follows, unity SCF, no thickness correction,
and a synthetic DFF of 2. The threshold is also chosen as 10 MPa solely to
expose the implemented extrapolation below it.

| Case | Range (MPa) | Annual cycles | Hand-calculated failure cycles | Damage |
| --- | ---: | ---: | ---: | ---: |
| high | 100 | 10 | `10**6 / 100**2 = 100` | 0.1 |
| knee | 10 | 2000 | `10**6 / 10**2 = 10000` | 0.2 |
| low | 1 | 3000000 | `10**7 / 1**3 = 10000000` | 0.3 |

Thus `D = 3/5`, life is `5/3` years, allowable damage is `1/2`, and utilization
is `6/5`. The low-range case controls despite being below the stored threshold.
The summary status is `FAIL` because utilization exceeds unity under the chosen
DFF. This status describes these analytical inputs; it is not a design approval.

## Executable worked example

Run the following with the package available on the Python path. The regression
test executes this exact code block and independently checks the fractions above.

```python
from digitalmodel.ansys.fatigue_postprocessor import (
    FatigueConfig,
    FatigueLoadCase,
    FatiguePostprocessor,
    SNcurve,
)

# Analytical inputs derived in the preceding hand calculation; no code values.
curve = SNcurve(
    name="synthetic",
    log_a1=6,
    m1=2,
    log_a2=7,
    m2=3,
    N_transition=10_000,
    threshold_mpa=10,
    environment="synthetic",
)
config = FatigueConfig(
    sn_curve=curve,
    load_cases=[
        FatigueLoadCase("high", stress_range_mpa=100, num_cycles=10),
        FatigueLoadCase("knee", stress_range_mpa=10, num_cycles=2000),
        FatigueLoadCase("low", stress_range_mpa=1, num_cycles=3_000_000),
    ],
    design_fatigue_factor=2,  # Chosen analytical input, not a prescribed DFF.
    thickness_correction=False,
    scf=1,  # Unity by construction: no stress concentration in this example.
)
processor = FatiguePostprocessor()
result = processor.calculate_fatigue_damage(config)
summary = processor.create_damage_summary_df([result])
print(
    f"damage={result.total_damage:.1f}; "
    f"life={result.fatigue_life_years:.1f} years; "
    f"utilization={result.utilization:.1f}; status={summary.loc[0, 'status']}"
)
```

Expected output:

```text
damage=0.6; life=1.7 years; utilization=1.2; status=FAIL
```

## Known limits and way forward

Equivalent stress is a norm of the deviatoric stress tensor, so subtracting its
values loses directional information. For synthetic opposite uniaxial states
`(+S, 0, 0)` and `(-S, 0, 0)`, both equivalent stresses are `abs(S)`, giving a
scalar difference of zero, while the component difference is `(2*S, 0, 0)` and
its equivalent stress is `2*abs(S)`. The component formula is implemented in
[cylinder_results.py](../../../src/digitalmodel/ansys/cylinder_results.py),
`von_mises`. Generated scalar differences therefore require
engineering interpretation before use as fatigue ranges.

This example supplies no mean-stress correction, environmental selection,
class-specific thickness exponent, design-code DFF selection, cycle counting,
or contour generation. The caller must establish these inputs and the loading
period for a real assessment. The module's defaults do not establish them.
Engineering acceptance beyond the synthetic calculation is **NOT_EVALUATED**.
