# Raw MBL utilisation coverage audit

This executable example audits only `MooringLineDesign.check_mbl`: line tension
divided by minimum breaking load (MBL). It covers ratios below, equal to and
above unity with independently authored synthetic inputs. It applies no design
safety factor and supplies no code-compliance verdict.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| A | 2026-10-26 | Added synthetic boundary cases and hand calculations to audit the raw MBL helper. |
| B | 2026-10-27 | Added the bounded audit plan and validation commands; isolated the audit tests and retained pinned source bytes to preserve existing evidence digests. |

## Plan-lite

- Scope: audit raw tension/MBL ratios below, equal to and above unity.
- Input basis: use only the independently authored Source S synthetic rows and
  supplied tensions below.
- Oracle: compare all nine results with the independent hand divisions in the
  table; check material-library restoration on success and an injected exception.
- Validation: run the mooring and citation suites with the commands below and
  run the engineering-register, repository-structure and file-level checks.
- Exclusions: material qualification, design factors, environmental response,
  licensed solvers, compliance verdicts and a broader coverage inventory.

Run these commands from the repository root. Here `uv` denotes the supplied
executable and `${AUDIT_SCRATCH}` denotes an external temporary-output directory.

```sh
PYTHONPATH=src uv run --no-project --with pytest --with numpy --with pandas --with scipy --with pydantic --with pyyaml python -m pytest tests/orcaflex/test_mooring_design.py tests/orcaflex/test_raw_mbl_audit.py -q -rA -p no:cacheprovider --basetemp="${AUDIT_SCRATCH}/bt-mooring"
PYTHONPATH=src uv run --no-project --with pytest --with numpy --with pandas --with scipy --with pydantic --with pyyaml python -m pytest tests/orcaflex/test_mooring_design_citations.py -q -rA -p no:cacheprovider --basetemp="${AUDIT_SCRATCH}/bt-citations"
```

## Synthetic input basis and hand calculation

Source S: all engineering inputs below were authored for this arithmetic
example. They are synthetic, not material recommendations, measurements or
licensed table values. No private repository or external source is needed.

| Segment material key | Synthetic MBL (kN) |
| --- | ---: |
| `audit_high_mbl` | 2000 |
| `audit_mid_mbl` | 1000 |
| `audit_low_mbl` | 500 |

Each segment has length 1 m. Each library row uses nominal diameter 10 mm,
mass per length 1 kg/m, submerged weight 1 N/m, axial stiffness 10000 kN and
normal drag coefficient 1.0, with the `CHAIN_R4` enum solely as a schema value.
These are Source S placeholders; only MBL enters this helper. The intact and
damaged factor fields are explicitly set to the arbitrary Source S values
2.0 and 3.0 to demonstrate that neither enters the raw ratio.

The same supplied line tension is applied to every segment in each call.
The independent hand calculation is force divided by force in matching units;
the resulting utilisation is dimensionless. Every expected value below is
obtained directly from the displayed division, without calling the helper.

| Supplied tension (kN) | `audit_high_mbl` | `audit_mid_mbl` | `audit_low_mbl` |
| ---: | --- | --- | --- |
| 400 | 400 / 2000 = 0.2 | 400 / 1000 = 0.4 | 400 / 500 = 0.8 |
| 1000 | 1000 / 2000 = 0.5 | 1000 / 1000 = 1.0 | 1000 / 500 = 2.0 |
| 1600 | 1600 / 2000 = 0.8 | 1600 / 1000 = 1.6 | 1600 / 500 = 3.2 |

## Executable example

Run the Python block from the repository root with `PYTHONPATH=src` in an
environment containing the package's analytical Python dependencies. The
temporary dictionary context restores the material library on both success and
exception. Run it in isolation from concurrent library users.

```python
from unittest.mock import patch

from digitalmodel.orcaflex.mooring_design import (
    MOORING_MATERIAL_LIBRARY,
    MooringLineDesign,
    MooringLineSegment,
    MooringMaterialProperties,
    SegmentMaterial,
)


def raw_mbl_audit(tension_kn):
    """Return raw ratios using only the Source S synthetic inputs above."""
    mbl_by_key = {
        "audit_high_mbl": 2000.0,
        "audit_mid_mbl": 1000.0,
        "audit_low_mbl": 500.0,
    }
    synthetic_library = {
        key: MooringMaterialProperties(
            material=SegmentMaterial.CHAIN_R4,
            nominal_diameter=10.0,
            mass_per_unit_length=1.0,
            submerged_weight=1.0,
            mbl=mbl_kn,
            axial_stiffness=10000.0,
            cd_normal=1.0,
        )
        for key, mbl_kn in mbl_by_key.items()
    }
    with patch.dict(MOORING_MATERIAL_LIBRARY, synthetic_library, clear=True):
        design = MooringLineDesign(
            segments=[
                MooringLineSegment(material_key=key, length=1.0)
                for key in mbl_by_key
            ],
            safety_factor_intact=2.0,
            safety_factor_damaged=3.0,
        )
        return design.check_mbl(max_tension_kn=tension_kn)


for tension_kn in (400.0, 1000.0, 1600.0):
    for material_key, utilisation in raw_mbl_audit(tension_kn).items():
        print(f"{tension_kn:.0f} kN | {material_key} | raw utilisation {utilisation:.1f}")
```

Each printed row corresponds to one hand-calculation table cell. The middle
tension gives a below-unity, an equal-to-unity and an above-unity result in one
call. An above-unity ratio indicates tension greater than the supplied MBL;
the helper returns the ratio without issuing a design decision.

## Scope and limits

The existing test checks only that default-material ratios fall in a
below-unity range. The executable smoke tests add exact numerical checks
against the hand-calculation table and check library restoration, including an
injected helper exception. Existing passing assertions are retained.

This audit checks the helper's arithmetic only. It does not validate material
properties, resolve citations for design factors, assess environmental loads,
solve line equilibrium, or perform fatigue, anchor, intact or damaged design
checks. Repeated material keys collapse to one result entry; this example uses
distinct keys so each supplied synthetic segment is reported separately.

Refs #1809. The parent issue remains open for the broader mooring coverage
inventory, knowledge-seed failure scenarios, design-factor citation pilot and
code-compliance checks beyond this raw-ratio helper.
