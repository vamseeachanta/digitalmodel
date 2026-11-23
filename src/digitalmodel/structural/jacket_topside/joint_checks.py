"""
Tubular Joint Classification and Punching Shear Checks
API RP 2A WSD (21st Edition), Section 4.3

Implements:
  - Tubular joint classification: T/Y, K, X
  - Punching shear unity check (simplified API RP 2A formulation)

Unit system: US customary (kips, inches, ksi).

References
----------
API RP 2A-WSD, 21st Edition, Section 4.3 (Tubular Joints).
"""

import math
from dataclasses import dataclass, field
from enum import Enum
from typing import List, Dict


# ---------------------------------------------------------------------------
# Joint type enumeration
# ---------------------------------------------------------------------------

class JointType(Enum):
    """API RP 2A tubular joint classification."""

    T_Y = "T/Y"   # Single brace (T) or single inclined brace (Y)
    K = "K"        # Two braces on same chord face, loads balance in chord
    X = "X"        # Two braces on opposite chord faces (cross joint)


# ---------------------------------------------------------------------------
# Data structures
# ---------------------------------------------------------------------------

@dataclass
class JointGeometry:
    """
    Geometry of a tubular joint (all dimensions in inches / degrees).

    Parameters
    ----------
    chord_od : float
        Chord outside diameter D (inches).
    chord_wt : float
        Chord wall thickness T (inches).
    brace_angles : list of float
        Angle of each brace measured from the chord axis (degrees).
        Length determines number of braces.
    brace_ods : list of float
        Outside diameter of each brace d (inches).
    brace_wts : list of float
        Wall thickness of each brace t_b (inches).
    gap : float
        Gap g between brace footprints on chord (inches).  Used for K joints.
    braces_on_same_side : bool
        True when both braces are on the same side of the chord (K joint).
        False when braces are on opposite sides (X joint).
        Ignored for single-brace joints.
    """

    chord_od: float
    chord_wt: float
    brace_angles: List[float]
    brace_ods: List[float]
    brace_wts: List[float]
    gap: float = 0.0
    braces_on_same_side: bool = True

    # ------------------------------------------------------------------
    # Derived parameters
    # ------------------------------------------------------------------

    @property
    def gamma(self) -> float:
        """Chord radius-to-thickness ratio: gamma = R/T = (D/2)/T."""
        return (self.chord_od / 2.0) / self.chord_wt

    @property
    def beta(self) -> List[float]:
        """
        Brace-to-chord diameter ratios: beta_i = d_i / D.
        Returns a list, one entry per brace.
        """
        return [d / self.chord_od for d in self.brace_ods]

    @property
    def tau(self) -> List[float]:
        """
        Brace-to-chord wall thickness ratios: tau_i = t_b_i / T.
        Returns a list, one entry per brace.
        """
        return [tb / self.chord_wt for tb in self.brace_wts]

    @property
    def num_braces(self) -> int:
        """Number of braces at this joint."""
        return len(self.brace_angles)


@dataclass
class JointLoads:
    """
    Applied loads on a single brace at the joint (kips / kip-inches).

    Parameters
    ----------
    axial_load : float
        Brace axial load (kips), positive = compression.
    ipb_moment : float
        In-plane bending moment at brace-chord intersection (kip-in).
    opb_moment : float
        Out-of-plane bending moment at brace-chord intersection (kip-in).
    """

    axial_load: float = 0.0
    ipb_moment: float = 0.0
    opb_moment: float = 0.0


@dataclass
class JointCheckResult:
    """Result of a single joint unity check."""

    check_type: str
    applied_vp: float       # Applied punching shear stress (ksi)
    allowable_vp: float     # Allowable punching shear stress (ksi)
    unity_check: float
    passes: bool
    joint_type: JointType = JointType.T_Y
    details: Dict = field(default_factory=dict)


# ---------------------------------------------------------------------------
# Joint classification
# ---------------------------------------------------------------------------

def classify_joint(geometry: JointGeometry) -> JointType:
    """
    Classify a tubular joint per API RP 2A WSD Sec. 4.3.1.

    Classification rules (simplified):
    - Single brace                 → T/Y
    - Two braces, same chord face  → K   (loads balance in chord)
    - Two braces, opposite faces   → X   (load passes through chord)

    Parameters
    ----------
    geometry : JointGeometry

    Returns
    -------
    JointType
    """
    n = geometry.num_braces

    if n == 1:
        return JointType.T_Y

    if n == 2:
        if geometry.braces_on_same_side:
            return JointType.K
        else:
            return JointType.X

    # Multi-brace joints: classify by dominant load path (simplified → K)
    return JointType.K


# ---------------------------------------------------------------------------
# Punching shear check
# ---------------------------------------------------------------------------

def _qq_factor(joint_type: JointType, beta: float, theta_deg: float) -> float:
    """
    Geometry factor Qq per API RP 2A WSD Sec. 4.3.2.

    Simplified formulas (conservative approximation):
      T/Y joint:  Qq = (0.3 / (beta*(1 - 0.833*beta))) * sin(theta)
      K joint:    Qq = same as T/Y (conservative)
      X joint:    Qq = sin(theta)

    where theta is the brace-to-chord included angle.
    """
    theta_rad = math.radians(theta_deg)
    sin_theta = math.sin(theta_rad)

    if joint_type == JointType.X:
        return sin_theta

    denom = beta * (1.0 - 0.833 * beta)
    if abs(denom) < 1e-12:
        denom = 1e-12
    return (0.3 / denom) * sin_theta


def _qf_factor(chord_utilization: float = 0.0) -> float:
    """
    Chord load factor Qf per API RP 2A WSD Sec. 4.3.2.

    Qf accounts for the reduction in joint capacity due to chord loading.
    Simplified form:
        Qf = 1.0 - 0.030 * gamma * A^2
    where A = chord utilization ratio (0..1).
    For a = 0 (no chord loading): Qf = 1.0.
    """
    # chord_utilization is not computed from loads in this simplified version;
    # caller passes 0.0 if chord loading is not considered.
    return max(0.0, 1.0 - 0.030 * chord_utilization)


def _applied_punching_shear(
    brace_od: float,
    brace_wt: float,
    axial_load: float,
    ipb_moment: float,
    opb_moment: float,
) -> float:
    """
    Resultant applied punching shear stress in the chord wall (ksi).

    API RP 2A Eq. (4.3-2) simplified:
        vp = f * sin(theta)
    where f is the brace nominal stress.

    For a tubular brace section:
        A_b = pi/4*(d^2 - d_i^2)
        S_b = I_b / (d/2)

    Resultant:
        vp = sqrt( (fa*sin)^2 + (fb_ipb*sin)^2 + (fb_opb*sin)^2 )

    Here sin(theta) is baked into Qq, so we compute the brace stresses
    directly and the caller multiples by sin(theta) via Qq.
    """
    d = brace_od
    t_b = brace_wt
    d_i = d - 2.0 * t_b
    A_b = (math.pi / 4.0) * (d ** 2 - d_i ** 2)
    I_b = (math.pi / 64.0) * (d ** 4 - d_i ** 4)
    S_b = I_b / (d / 2.0)

    fa_b = axial_load / A_b if A_b > 0.0 else 0.0
    fb_ipb = ipb_moment / S_b if S_b > 0.0 else 0.0
    fb_opb = opb_moment / S_b if S_b > 0.0 else 0.0

    return math.sqrt(fa_b ** 2 + fb_ipb ** 2 + fb_opb ** 2)


def punching_shear_uc(
    geometry: JointGeometry,
    loads: JointLoads,
    chord_fy: float,
    brace_index: int = 0,
    chord_utilization: float = 0.0,
) -> JointCheckResult:
    """
    Punching shear unity check per API RP 2A WSD Sec. 4.3.2 (simplified).

    Allowable punching shear stress:
        Vp = Fy * Qf * Qq / (0.6 * sqrt(gamma))

    Applied punching shear:
        vp = resultant brace stress projected onto chord wall

    Unity check:
        UC = vp / Vp

    Parameters
    ----------
    geometry : JointGeometry
    loads : JointLoads
        Applied loads for the brace indicated by brace_index.
    chord_fy : float
        Chord yield strength (ksi).
    brace_index : int
        Index into geometry.brace_angles/brace_ods/brace_wts.
    chord_utilization : float
        Chord axial utilization ratio (0..1) for Qf computation.
        Default 0.0 (no chord load reduction).

    Returns
    -------
    JointCheckResult
    """
    if brace_index < 0 or brace_index >= geometry.num_braces:
        raise IndexError(
            f"brace_index {brace_index} out of range for joint with "
            f"{geometry.num_braces} brace(s)."
        )

    joint_type = classify_joint(geometry)
    gamma = geometry.gamma
    beta_i = geometry.beta[brace_index]
    theta_deg = geometry.brace_angles[brace_index]

    Qq = _qq_factor(joint_type, beta_i, theta_deg)
    Qf = _qf_factor(chord_utilization)

    allowable_vp = chord_fy * Qf * Qq / (0.6 * math.sqrt(gamma))

    applied_vp = _applied_punching_shear(
        brace_od=geometry.brace_ods[brace_index],
        brace_wt=geometry.brace_wts[brace_index],
        axial_load=loads.axial_load,
        ipb_moment=loads.ipb_moment,
        opb_moment=loads.opb_moment,
    )

    uc = applied_vp / allowable_vp if allowable_vp > 0.0 else float("inf")

    return JointCheckResult(
        check_type="punching_shear",
        applied_vp=applied_vp,
        allowable_vp=allowable_vp,
        unity_check=uc,
        passes=uc <= 1.0,
        joint_type=joint_type,
        details={
            "Qq": Qq,
            "Qf": Qf,
            "gamma": gamma,
            "beta": beta_i,
            "theta_deg": theta_deg,
            "joint_type": joint_type.value,
        },
    )
