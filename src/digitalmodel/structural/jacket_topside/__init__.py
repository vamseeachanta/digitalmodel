"""
Jacket and Topside Structural Analysis

API RP 2A WSD (21st Ed.) member capacity checks and tubular joint
classification and punching shear checks per API RP 2A Section 4.

Unit system: US customary (kips, inches, ksi).
"""

from .member_checks import (
    TubularSection,
    MemberLoads,
    MemberCheckResult,
    axial_tension_uc,
    axial_compression_uc,
    bending_uc,
    combined_axial_bending_uc,
    member_checks,
)

from .joint_checks import (
    JointGeometry,
    JointLoads,
    JointCheckResult,
    JointType,
    classify_joint,
    punching_shear_uc,
)

__all__ = [
    # Member checks
    "TubularSection",
    "MemberLoads",
    "MemberCheckResult",
    "axial_tension_uc",
    "axial_compression_uc",
    "bending_uc",
    "combined_axial_bending_uc",
    "member_checks",
    # Joint checks
    "JointGeometry",
    "JointLoads",
    "JointCheckResult",
    "JointType",
    "classify_joint",
    "punching_shear_uc",
]

__version__ = "1.0.0"
