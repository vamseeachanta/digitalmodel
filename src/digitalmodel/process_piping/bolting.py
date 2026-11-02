"""PCC-1-informed assembly checks with a separate Appendix 2 design-load screen.

ASME PCC-1-2019 sections 8, 11, 12 and Appendix O (O-7 to O-9).
ASME BPVC VIII-1-2023 Appendix 2, 2-5(c): Wm1 and Wm2 design loads.
Public explanation by FSA, 'What Are M and Y Gasket Design Constants, and
How Are They Used?' (2011): https://www.pumpsandsystems.com/article/what-are-m-and-y-gasket-design-constants-and-how-are-they-used/
Public assembly guidance: ESA/FSA 009/98 (1998), sections 6.1-6.9:
https://www.fluidsealing.com/wp-content/uploads/Safe-Seal-Usage.pdf

m/y are flange DESIGN constants, not PCC-1 assembly stress limits or a
leak-rate guarantee. No tabulated gasket factors or bolt/flange strengths
are provided. Required contact areas, effective b/G, stresses, retained
load and nut factor require engineering/product qualification.
"""

from dataclasses import dataclass
import math

from digitalmodel.citations.schema import Citation
from ._basis import citations, positive, nonnegative


@dataclass(frozen=True)
class BoltUp:
    """Explicit Pa, m, m², N inputs; facing is rf_spiral_wound or rtj.

    bolt_area_m2 is the tensile-stress area of ONE fastener, not shank area.
    PCC-1 target prestress values quoted on root area must be converted by
    Ar/Ats before use here.
    gasket_contact_area_m2 excludes guide/centering rings for spiral wound;
    for RTJ it is a qualified effective contact area, NOT full ring footprint.
    effective_gasket_width_m and reaction diameter follow Appendix 2, 2-5;
    the caller determines these from the specific facing, rather than using
    the full contact width. No RF/RTJ factor defaults are interchangeable.
    flange_load_limit_n is the assessed assembly limit including rotation.
    retained_load_fraction accounts for qualified relaxation/creep losses.
    gasket_inside_diameter_m is the pressure diameter for PCC-1 O-8.
    """

    facing: str
    bolt_count: int
    bolt_area_m2: float
    bolt_diameter_m: float
    target_bolt_stress_pa: float
    maximum_bolt_stress_pa: float
    minimum_bolt_stress_pa: float
    nut_factor: float
    gasket_contact_area_m2: float
    gasket_seating_min_pa: float
    gasket_operating_min_pa: float
    gasket_max_pa: float
    retained_load_fraction: float
    flange_load_limit_n: float
    pressure_pa: float
    gasket_inside_diameter_m: float
    effective_gasket_width_m: float
    gasket_reaction_diameter_m: float
    gasket_m: float
    gasket_y_pa: float


@dataclass(frozen=True)
class BoltUpResult:
    assembly_load_n: float
    torque_per_bolt_nm: float
    assembly_gasket_stress_pa: float
    residual_gasket_stress_pa: float
    seating_design_load_n: float
    operating_design_load_n: float
    checks: dict[str, bool]
    citations: tuple[Citation, ...]


def check_bolt_up(joint: BoltUp) -> BoltUpResult:
    """Compare target load against ALL supplied joint-component criteria.

    F=n Ab Sb; torque per bolt=K*(F/n)*d (PCC-1-2019 sec. 12).
    PCC-1 Appendix O: assembly stress F/Ag; retained operating gasket
    stress=(phi*F-P*pi*Gi²/4)/Ag; compare with min seating/operating and
    max compression. No bolt/flange upper limit is inferred from yield.

    Separate Appendix 2 screen: Wm2=pi*b*G*y;
    Wm1=pi*G²*P/4+2*pi*b*G*m*P. Assembly load is compared to Wm2,
    retained load to Wm1. b is already the EFFECTIVE width (2-5).
    This combined screen does not implement all of Appendix O, flange
    rotation calculations, operating bolt stresses or fatigue. Geometry,
    surface condition, material/temperature compatibility and assembler
    qualification are external checks. Repeat for design and test pressure.
    """
    refs = citations("bolting")
    if joint.facing not in ("rf_spiral_wound", "rtj"):
        raise ValueError("facing must be rf_spiral_wound or rtj")
    if type(joint.bolt_count) is not int or joint.bolt_count < 4:
        raise ValueError("bolt_count must be an integer >=4")
    positive(
        **{
            name: value
            for name, value in vars(joint).items()
            if name not in ("facing", "bolt_count", "pressure_pa")
        }
    )
    nonnegative(pressure_pa=joint.pressure_pa)
    if joint.retained_load_fraction > 1:
        raise ValueError("retained load fraction must be <=1")
    if (
        max(joint.gasket_seating_min_pa, joint.gasket_operating_min_pa)
        > joint.gasket_max_pa
    ):
        raise ValueError("minimum gasket stress exceeds maximum")
    if joint.minimum_bolt_stress_pa > joint.maximum_bolt_stress_pa:
        raise ValueError("minimum bolt stress exceeds maximum")
    if joint.bolt_area_m2 > math.pi * joint.bolt_diameter_m**2 / 4:
        raise ValueError("tensile stress area exceeds bolt shank area")
    if joint.gasket_inside_diameter_m >= joint.gasket_reaction_diameter_m:
        raise ValueError(
            "gasket pressure diameter must be smaller than reaction diameter"
        )
    load = joint.bolt_count * joint.bolt_area_m2 * joint.target_bolt_stress_pa
    retained = load * joint.retained_load_fraction
    end_force = joint.pressure_pa * math.pi * joint.gasket_inside_diameter_m**2 / 4
    seating = (
        math.pi
        * joint.effective_gasket_width_m
        * joint.gasket_reaction_diameter_m
        * joint.gasket_y_pa
    )
    operating = (
        math.pi * joint.gasket_reaction_diameter_m**2 * joint.pressure_pa / 4
        + 2
        * math.pi
        * joint.effective_gasket_width_m
        * joint.gasket_reaction_diameter_m
        * joint.gasket_m
        * joint.pressure_pa
    )
    seating_stress = load / joint.gasket_contact_area_m2
    operating_stress = (retained - end_force) / joint.gasket_contact_area_m2
    checks = {
        "bolt_minimum": joint.target_bolt_stress_pa >= joint.minimum_bolt_stress_pa,
        "bolt_maximum": joint.target_bolt_stress_pa <= joint.maximum_bolt_stress_pa,
        "flange_load": load <= joint.flange_load_limit_n,
        "seating_gasket": seating_stress >= joint.gasket_seating_min_pa,
        "operating_gasket": operating_stress >= joint.gasket_operating_min_pa,
        "gasket_maximum": seating_stress <= joint.gasket_max_pa,
        "appendix2_seating": load >= seating,
        "appendix2_operating": retained >= operating,
    }
    return BoltUpResult(
        load,
        joint.nut_factor * load / joint.bolt_count * joint.bolt_diameter_m,
        seating_stress,
        operating_stress,
        seating,
        operating,
        checks,
        refs,
    )


@dataclass(frozen=True)
class TighteningPass:
    target_fraction: float
    bolt_order: tuple[int, ...]
    repeat_until_stable: bool = False
    citations: tuple[Citation, ...] = ()


def tightening_passes(bolt_count: int) -> tuple[TighteningPass, ...]:
    """Symmetric cross pattern, bolts numbered clockwise from 1.

    ESA/FSA 009/98 (1998) 6.7: hand snug, 30%, 60%, full torque, circular
    full-torque pass. Generated opposite/quadrant ordering is limited to
    4, 8, 12, 16 and 32 bolts; it is not a transcription of PCC-1 figures.
    PCC-1-2019 sec. 11 procedure qualification and Appendix F pattern
    selection still apply. Final circular passes repeat until no further
    nut rotation under the qualified procedure. No hot retorque implied.
    """
    refs = citations("bolting")
    if type(bolt_count) is not int or bolt_count not in (4, 8, 12, 16, 32):
        raise ValueError("supported bolt counts are 4, 8, 12, 16 and 32")
    quarter = bolt_count // 4
    if quarter == 3:
        starts = (0, 1, 2)
    else:
        bits = (quarter - 1).bit_length()
        starts = tuple(
            sorted(range(quarter), key=lambda k: int(f"{k:0{bits}b}"[::-1], 2))
        )
    cross = tuple(
        k + offset + 1
        for k in starts
        for offset in (0, 2 * quarter, quarter, 3 * quarter)
    )
    return tuple(
        TighteningPass(fraction, cross, citations=refs) for fraction in (0, 0.3, 0.6, 1)
    ) + (TighteningPass(1, tuple(range(1, bolt_count + 1)), True, refs),)


def joint_leak_checks() -> tuple[tuple[str, str], ...]:
    """Ranked diagnostic priorities, NOT a measured frequency ranking.

    PCC-1-2019 Appendix P troubleshooting and ESA/FSA 009/98 sections
    6.1-6.9 motivate this engineering triage order. No population data is
    available to establish a universal leading cause of startup leaks.
    """
    citations("bolting")
    return (
        (
            "Insufficient or uneven gasket compression",
            "Check target bolt stress, calibrated tool, lubrication, cross passes and residual load.",
        ),
        (
            "Wrong, damaged or reused gasket",
            "Verify RF spiral-wound or RTJ type, ring number, material, dimensions and new-gasket condition.",
        ),
        (
            "Damaged or contaminated sealing faces",
            "Inspect finish, flatness, scratches and RTJ grooves; remove deposits.",
        ),
        (
            "Flange misalignment or external piping load",
            "Check parallelism, gap, supports and nozzle loads before bolt-up; do not draw piping into alignment with bolts.",
        ),
        (
            "Thermal relaxation or differential expansion",
            "Check operating temperature, retained gasket load and qualified startup procedure.",
        ),
        (
            "Wrong rating or pressure-test setup",
            "Verify pressure-temperature ratings, weakest component, local static head and test records.",
        ),
        (
            "Weld or threaded-joint defect",
            "Locate the leak, verify fabrication/NDE records and applicable repair/retest procedure.",
        ),
    )
