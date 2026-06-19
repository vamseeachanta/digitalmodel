"""
ABOUTME: Turn a vessel record into the rigid-body mass properties a diffraction
run needs (mass, centre of mass, inertia tensor) — the diffraction-side consumer
of the vessel DB, parallel to the installation go/no-go consumer.

A diffraction setup (OrcaWave / AQWA) needs, per body: mass, centre of mass, and
the mass moment of inertia about the CoG. The vessel DB carries displacement
(-> mass) and radii of gyration (cited or estimated via gyradii.py); inertia is
then ``I = m * k^2`` per axis. Every output field is tagged with how it was
obtained (cited / estimated / default), so nothing is silently invented.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Optional

from digitalmodel.marine_ops.vessel_db.gyradii import estimate_gyradii
from digitalmodel.marine_ops.vessel_db.loader import Record

# DOF order matches the repo convention: surge, sway, heave, roll, pitch, yaw.
_AXES = ("kxx", "kyy", "kzz")


@dataclass
class MassProperties:
    """Rigid-body mass properties for a diffraction setup (SI-ish: te, m, te·m²)."""

    vessel_name: str
    mass_te: float
    cog_m: tuple[float, float, float]            # (x fwd, y port, z up) about chosen datum
    gyradii_m: dict[str, float]                  # kxx/kyy/kzz
    inertia_te_m2: dict[str, float]              # Ixx/Iyy/Izz about CoG
    provenance: dict[str, str] = field(default_factory=dict)  # field -> cited/estimated:.../default
    gaps: list[str] = field(default_factory=list)

    def orcawave_body_block(self) -> dict:
        """OrcaWave body mass block (matches the repo's L0x example keys).

        Diagonal inertia tensor about the centre of mass (principal-axis
        assumption — off-diagonal terms need a full weight breakdown we don't have).
        """
        Ixx = self.inertia_te_m2["Ixx"]
        Iyy = self.inertia_te_m2["Iyy"]
        Izz = self.inertia_te_m2["Izz"]
        return {
            "BodyName": self.vessel_name,
            "BodyMass": round(self.mass_te, 3),
            "BodyCentreOfMass": [round(c, 3) for c in self.cog_m],
            "BodyInertiaSpecifiedBy": "Matrix (for a general body)",
            "BodyInertiaTensorOriginType": "Centre of mass",
            "BodyInertiaTensorRx": [round(Ixx, 1), 0.0, 0.0],
            "BodyInertiaTensorRy": [0.0, round(Iyy, 1), 0.0],
            "BodyInertiaTensorRz": [0.0, 0.0, round(Izz, 1)],
        }


def _gyradius(rec: Record, canon: str, estimates: dict) -> tuple[Optional[float], str]:
    """(value, basis) for a radius of gyration: cited if present, else estimated."""
    num, marker, _ = rec.dimension(canon)
    if marker == "number":
        return num, "cited"
    key = {"kxx": "kxx_roll", "kyy": "kyy_pitch", "kzz": "kzz_yaw"}[canon]
    est = estimates.get(key)
    if est is not None:
        return est.value, est.basis
    return None, "gap"


def from_record(rec: Record) -> Optional[MassProperties]:
    """Build MassProperties from a vessel DB particulars record.

    Returns None if there is no usable mass (displacement). CoG defaults to the
    waterplane centroid datum (x=0 amidships, y=0 centreline) when LCG/TCG are
    absent; VCG is used if present, else left as a flagged gap (0.0 placeholder).
    """
    dims = rec.canonical_dimensions()
    mass = dims.get("displacement")
    if not mass or mass <= 0:
        return None

    beam = dims.get("beam")
    lbp = dims.get("lbp") or dims.get("loa")
    estimates = estimate_gyradii(beam, lbp, rec.vessel_type)

    prov: dict[str, str] = {"mass_te": "cited"}
    gaps: list[str] = []

    gyr: dict[str, float] = {}
    inertia: dict[str, float] = {}
    for canon, iname in (("kxx", "Ixx"), ("kyy", "Iyy"), ("kzz", "Izz")):
        val, basis = _gyradius(rec, canon, estimates)
        if val is None:
            gaps.append(canon)
            continue
        gyr[canon] = val
        inertia[iname] = mass * val * val  # I = m k^2  [te·m²]
        prov[iname] = basis

    # Centre of mass.
    lcg, lcg_m, _ = rec.dimension("lcg")
    vcg, vcg_m, _ = rec.dimension("vcg")
    tcg, tcg_m, _ = rec.dimension("tcg")
    cx = lcg if lcg_m == "number" else 0.0
    cy = tcg if tcg_m == "number" else 0.0
    cz = vcg if vcg_m == "number" else 0.0
    prov["cog_x"] = "cited" if lcg_m == "number" else "default:amidships(0)"
    prov["cog_y"] = "cited" if tcg_m == "number" else "default:centreline(0)"
    if vcg_m == "number":
        prov["cog_z"] = "cited"
    else:
        prov["cog_z"] = "default:keel-datum(0)"
        gaps.append("vcg")

    return MassProperties(
        vessel_name=rec.name,
        mass_te=mass,
        cog_m=(cx, cy, cz),
        gyradii_m=gyr,
        inertia_te_m2=inertia,
        provenance=prov,
        gaps=gaps,
    )
