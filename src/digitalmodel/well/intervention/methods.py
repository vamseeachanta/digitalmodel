"""Problem-to-method options and explicitly scoped public reference envelopes.

Selection basis: 'Well intervention using rigless techniques' (2003), sections
Wireline, Coiled tubing, Hydraulic workover and Alternate approach:
https://www.offshore-mag.com/production/article/16755400/well-intervention-using-rigless-techniques
Also RPSEA 08121-1502, Task 2 (7 June 2010), Tables 5-6, pp.63-65:
https://edx.netl.doe.gov/dataset/coiled-tubing-drilling-and-intervention-systems-using-cost-effective-vessel/resource/7268bc40-e7ec-4743-9b39-012b107490f0/download

Options are engineering screening associations, not a proprietary selection
method. Applicability depends on diagnosis, completion, well control, lifting,
pressure, barriers and site access. The arrays do not rank treatment efficacy.
"""

from dataclasses import dataclass
from enum import Enum
from typing import Mapping

from .conveyance import ConveyanceResult, wireline_conveyance
from .screening import Band


RIGLESS = "https://www.offshore-mag.com/production/article/16755400/well-intervention-using-rigless-techniques"
RPSEA = "https://edx.netl.doe.gov/dataset/coiled-tubing-drilling-and-intervention-systems-using-cost-effective-vessel/resource/7268bc40-e7ec-4743-9b39-012b107490f0/download"
IATMI = "https://journal.iatmi.or.id/index.php/ojs/article/download/309/316"
RATES = "https://www.sdbeyondpetro.com/workover-rigs-explained-types-uses-buyer-s-guide-2026"


class Problem(str, Enum):
    DECLINING_RATE = "declining_rate"
    SAND = "sand"
    SCALE = "scale"
    WAX_ASPHALTENE = "wax_asphaltene"
    LIQUID_LOADING = "liquid_loading"
    TUBING_FAILURE = "tubing_failure"
    VALVE_FAILURE = "valve_failure"
    INTEGRITY = "integrity"


class Method(str, Enum):
    SLICKLINE = "slickline"
    E_LINE = "e_line"
    COILED_TUBING = "coiled_tubing"
    HWU = "hwu"
    RIG_WORKOVER = "rig_workover"
    RLWI = "riserless_light_well_intervention_vessel"
    RISER_BASED = "riser_based_intervention"
    SUBSEA_RIG = "subsea_rig"


@dataclass(frozen=True)
class MethodOption:
    """A downhole method and its separate access/deployment alternatives."""

    method: Method
    purpose: str
    diagnostic: bool
    access_classes: tuple[Method, ...]
    sources: tuple[str, ...]
    conveyance: ConveyanceResult | None = None


def method_options(
    problem: Problem | str,
    *,
    subsea: bool = False,
    wireline_retrievable_valve: bool = False,
    wireline_inputs: Mapping[str, object] | None = None,
) -> tuple[MethodOption, ...]:
    """Return bounded options; 2003 rigless article and RPSEA Tables 5-6.

    Declining rate and integrity first require diagnostics. Through-tubing
    valve replacement is included only when the caller confirms a wireline-
    retrievable design. Tubing failure requires a tubing-handling unit.
    Standard RLWI wireline access is not assumed to convey CT or pull tubing;
    special open-water CT systems require separate qualification. An access
    option is not a vessel-capability or well-barrier approval.
    For deviated/horizontal line methods supply wireline_inputs using the
    wireline_conveyance keyword set; the existing pump-down solver is called
    once and its result is attached to each line option. None means NOT
    EVALUATED, never feasible. Failed options remain visible with their status.
    """
    problem = Problem(problem)
    if type(subsea) is not bool or type(wireline_retrievable_valve) is not bool:
        raise ValueError("subsea and wireline_retrievable_valve must be booleans")
    sl, el, ct, hw, rig = (
        Method.SLICKLINE,
        Method.E_LINE,
        Method.COILED_TUBING,
        Method.HWU,
        Method.RIG_WORKOVER,
    )
    entries = {
        Problem.DECLINING_RATE: [
            (el, "production logging to diagnose rate loss", True),
            (ct, "cleanout or stimulation after damage diagnosis", False),
        ],
        Problem.SAND: [
            (sl, "limited-volume bailing after access confirmation", False),
            (el, "powered cleaning after tool qualification", False),
            (ct, "circulate and wash fill", False),
        ],
        Problem.SCALE: [
            (sl, "mechanical scraping of accessible deposits", False),
            (ct, "qualified chemical treatment or milling", False),
        ],
        Problem.WAX_ASPHALTENE: [
            (sl, "scraping of accessible deposits", False),
            (ct, "circulation or compatible solvent placement", False),
        ],
        Problem.LIQUID_LOADING: [
            (sl, "service a compatible gas-lift valve", False),
            (ct, "nitrogen unload or velocity-string evaluation", False),
        ],
        Problem.TUBING_FAILURE: [
            (hw, "pull and repair tubing with qualified HWU", False),
            (rig, "pull and replace completion tubing", False),
        ],
        Problem.VALVE_FAILURE: [
            (hw, "tubing-retrievable valve repair", False),
            (rig, "tubing-retrievable valve replacement", False),
        ],
        Problem.INTEGRITY: [
            (el, "locate and characterize the leak", True),
            (hw, "completion repair after barrier assessment", False),
            (rig, "major repair after barrier assessment", False),
        ],
    }[problem]
    if problem == Problem.VALVE_FAILURE and wireline_retrievable_valve:
        entries = [
            (sl, "replace a confirmed wireline-retrievable valve", False)
        ] + entries
    conveyance = None
    if wireline_inputs is not None and any(m in (sl, el) for m, _, _ in entries):
        conveyance = wireline_conveyance(**wireline_inputs)
    options = []
    for method, purpose, diagnostic in entries:
        if not subsea:
            access = ()
        elif method in (sl, el):
            access = (Method.RLWI, Method.RISER_BASED, Method.SUBSEA_RIG)
        elif method == ct:
            access = (Method.RISER_BASED, Method.SUBSEA_RIG)
        else:
            access = (Method.SUBSEA_RIG,)
        sources = (
            (RIGLESS, RPSEA, IATMI) if problem == Problem.SAND else (RIGLESS, RPSEA)
        )
        options.append(
            MethodOption(
                method,
                purpose,
                diagnostic,
                access,
                sources,
                conveyance if method in (sl, el) else None,
            )
        )
    return tuple(options)


@dataclass(frozen=True)
class ReferenceEstimate:
    """Public reference ranges with a unit-hire sensitivity band in USD.

    unit_hire_sensitivity_usd evaluates duration [day] times rate [USD/day]
    at the supplied bounds; it is not a derivative or a total-job cost.

    Duration cases and day-rate market are DIFFERENT contexts. The resulting
    product is a sensitivity envelope, not an observed cost range or a total
    job estimate. All amounts are nominal USD in the rate source's 2026 basis.
    """

    duration_days: Band
    day_rate_usd: Band
    unit_hire_sensitivity_usd: Band
    sources: tuple[str, ...]
    limitations: str
    duration_basis: str
    duration_readiness: str


def reference_estimate(
    method: Method | str,
    *,
    duration_days: Band | None = None,
    duration_basis: str | None = None,
) -> ReferenceEstimate:
    """Return cited reference ranges, never a default offshore budget.

    Durations are case/task envelopes, NOT universal typical durations:
    * slickline: no default; the previously used lower endpoint is unverified.
    * e-line 4-7 d: RPSEA Task 2 (2010), Table 6, BH survey 4 d; IATMI22-187,
      Table 1, powered cleanout 7 d.
    * CT 4-14 d: RPSEA Table 6, sand wash 4 d; IATMI22-187 Table 1, 14 d.
    * HWU: no default; the previously used upper endpoint is unverified.
    * rig 20-30 d: RPSEA Table 6, tubing failure 20 d and casing leak 30 d.

    Selected unit day-rate facts: Beyond Petroleum Equipment, 23 July 2026,
    Workover Rigs Explained, Table 1 (US onshore): grouped wireline/slickline
    USD3500-8000/d (no separate e-line quote), CT 12000-25000/d,
    HWU 20000-38000/d. Heavy-mast Eagle Ford USD18000-28000/d comes from
    'Basin-by-Basin Rate Overview', not the separate Table 1 range.
    Only rate facts are used; no vendor selection procedure is implemented.

    No verified primary-source transferable total-cost bands for subsea RLWI,
    riser-based or rig jobs are embedded. Their cost/duration bands shall be
    supplied to screen_economics with a project-specific basis. Onshore rates
    shall not be assigned to offshore spreads. No escalation or FX is applied.

    Slickline and HWU require explicit duration_days and a nonblank
    duration_basis. Other methods accept the same override. Caller inputs
    carry caller_supplied_scenario readiness, never independent verification;
    retained published ranges carry public_reference_only readiness. Neither
    status establishes suitability for an actual intervention. Duration source
    links are included only when the published duration is used.
    """
    method = Method(method)
    cases = {
        Method.SLICKLINE: (None, None, 3500, 8000, ()),
        Method.E_LINE: (4, 7, 3500, 8000, (RPSEA, IATMI)),
        Method.COILED_TUBING: (4, 14, 12000, 25000, (RPSEA, IATMI)),
        Method.HWU: (None, None, 20000, 38000, ()),
        Method.RIG_WORKOVER: (20, 30, 18000, 28000, (RPSEA,)),
    }
    if method not in cases:
        raise ValueError(
            "subsea cost and duration bands must be supplied with their project basis"
        )
    d0, d1, c0, c1, sources = cases[method]
    if duration_days is None:
        if d0 is None or duration_basis is not None:
            raise ValueError("explicit duration_days and duration_basis are required")
        duration = Band(d0, d1)
        basis = "Published case/task envelope; see duration sources."
        readiness = "public_reference_only"
    else:
        if not isinstance(duration_days, Band):
            raise ValueError("duration_days must be a Band in days")
        if not isinstance(duration_basis, str) or not duration_basis.strip():
            raise ValueError("explicit duration_days and duration_basis are required")
        duration = duration_days
        basis = duration_basis.strip()
        readiness = "caller_supplied_scenario"
        sources = ()
    return ReferenceEstimate(
        duration,
        Band(c0, c1),
        Band(duration.low * c0, duration.high * c1),
        sources + (RATES,),
        "Reference sensitivity only: case durations and 2026 US-onshore unit rates "
        "have different scopes. Excludes mobilization, offshore vessel/rig/support, "
        "weather, services, materials, chemicals, contingency and deferred oil; "
        "no field cost estimate or current quote is established. "
        "Caller-supplied durations are not independently verified.",
        basis,
        readiness,
    )
