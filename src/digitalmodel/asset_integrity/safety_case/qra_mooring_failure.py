"""Calculate supplied frequency times consequence; retain tensions as context only.

Basis: UK SCR-2015 regulation 16(1)(d), assessment of likelihood, consequence
and controls; IMO MSC-MEPC.2/Circ.12/Rev.2 (2018), annex section 6.2.2 (data
basis) and appendix 5 section 3.2.3 (PLL = sum of frequency * fatalities).
https://www.legislation.gov.uk/uksi/2015/398/regulation/16
https://www.imo.org/en/OurWork/Safety/Pages/FormalSafetyAssessment.aspx

Consumes the actual CatenaryResult produced by orcaflex.mooring_design's
solver-free solve_catenary. Top and horizontal tensions are retained as load
evidence. Neither a static tension nor a design safety factor determines a
line-break rate or post-break consequences. Both quantities and their bases
are required caller inputs (explicit None leaves the risk NOT_EVALUATED).
No licensed solver, default failure rate, multi-line independence assumption,
dynamic load redistribution or progressive-failure model is used.

Worked synthetic example: solve_catenary with depth 100 m, length 500 m,
submerged weight 1000 N/m and pretension 300 kN returns top tension 300 kN
and horizontal tension 200 kN (T = H + w*h). An assumed 0.02 breaks/year
and 0.5 mean fatalities/break give 0.01 expected fatalities/year. The inputs
are arithmetic assumptions, not a calibrated mooring risk assessment.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING

from pydantic import TypeAdapter

from .mah_register import MajorAccidentHazard, NonNegative

if TYPE_CHECKING:
    from digitalmodel.orcaflex.mooring_design import CatenaryResult


@dataclass(frozen=True)
class MooringFailureResult:
    """Validated hazard and the static load evidence consumed from the design."""

    hazard: MajorAccidentHazard
    top_tension_kn: float
    horizontal_tension_kn: float


def assess_mooring_failure(
    design: CatenaryResult,
    *,
    hazard_id: str,
    description: str,
    frequency_per_year: float | None,
    fatalities_per_event: float | None,
    frequency_basis: str,
    consequence_basis: str,
    barriers: tuple[str, ...],
    responsible_roles: tuple[str, ...],
) -> MooringFailureResult:
    """Form a register entry for one specified line-break scenario.

    Frequency refers to this scenario, not a per-line rate silently multiplied
    by a line count. Consequence is a conditional mean over exposed persons.
    Controls already credited in either input must be stated in its basis.
    Results are not passed to the individual-risk ALARP classifier.
    """
    # CatenaryResult is mutable and permits non-finite numbers upstream;
    # validate the consumed evidence rather than trusting its model annotation.
    adapter = TypeAdapter(NonNegative)
    top = adapter.validate_python(design.top_tension)
    horizontal = adapter.validate_python(design.horizontal_tension)
    if horizontal > top:
        raise ValueError("horizontal tension cannot exceed total top tension")
    hazard = MajorAccidentHazard(
        hazard_id=hazard_id,
        description=description,
        frequency_per_year=frequency_per_year,
        fatalities_per_event=fatalities_per_event,
        frequency_basis=frequency_basis,
        consequence_basis=consequence_basis,
        barriers=barriers,
        responsible_roles=responsible_roles,
    )
    return MooringFailureResult(hazard, top, horizontal)
