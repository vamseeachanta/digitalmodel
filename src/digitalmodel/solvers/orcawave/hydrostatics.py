# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Explicit SI hydrostatic inputs for the offline result adapter.

Source: WAMIT User Manual v7.5, section 3.1, hydrostatic and gravitational
restoring coefficients (public manual):
https://www.wamit.com/manual7.x/html/wamit_v75manualch3.html
No density, gravity, centre or waterplane property is inferred from solver data.
"""

from dataclasses import dataclass, asdict
import math


@dataclass(frozen=True)
class HydrostaticProperties:
    """Centered symmetric waterplane, at equilibrium mass rho*V.

    SI inputs: density kg/m³, gravity m/s², volume m³, area m², waterplane
    second moments m⁴, and vertical centres m positive upward. The reference
    is on the waterplane centroid, with x forward, y transverse, z upward.
    xB=xG=yB=yG=0 and Ixy=0 are required applicability assumptions. Ixx is
    integral(y² dA), governing roll; Iyy is integral(x² dA), governing pitch.

    The resisting load is -C*q; translations q are m and rotations rad.
    C33=rho*g*A; C44=rho*g*(Ixx+V*(zB-zG)); C55 likewise with Iyy.
    The buoyancy and gravity terms both remain. A negative rotational entry
    indicates static instability and is preserved for downstream assessment.
    Off-centre bodies, heel, free-surface corrections and other couplings are
    NOT_EVALUATED by this centered-waterplane model.
    """

    density: float
    gravity: float
    volume: float
    waterplane_area: float
    waterplane_ixx: float
    waterplane_iyy: float
    buoyancy_z: float
    gravity_z: float

    def to_mapping(self):
        """Serialize dimensional coefficients without native-solver claims."""
        inputs = asdict(self)
        if not all(math.isfinite(value) for value in inputs.values()):
            raise ValueError("hydrostatic properties must be finite")
        for name in ("density", "gravity", "volume", "waterplane_area"):
            if inputs[name] <= 0:
                raise ValueError(f"{name} must be positive")
        if min(self.waterplane_ixx, self.waterplane_iyy) < 0:
            raise ValueError("waterplane second moments must be nonnegative")
        weight_density = self.density * self.gravity
        centre_term = self.volume * (self.buoyancy_z - self.gravity_z)
        diagonal = [0.0, 0.0, weight_density * self.waterplane_area,
                    weight_density * (self.waterplane_ixx + centre_term),
                    weight_density * (self.waterplane_iyy + centre_term), 0.0]
        if not all(math.isfinite(value) for value in diagonal):
            raise ValueError("restoring coefficients must be finite")
        return {
            "Status": "EVALUATED",
            "Basis": "centered symmetric waterplane, mass equals density times volume",
            "Inputs": inputs,
            "Density": self.density,
            "Gravity": self.gravity,
            "DOFOrder": ["surge", "sway", "heave", "roll", "pitch", "yaw"],
            "TranslationUnits": "m", "RotationUnits": "rad",
            "ForceUnits": "N", "MomentUnits": "N m",
            "RestoringMatrix": [[value if i == j else 0.0 for j in range(6)]
                                for i, value in enumerate(diagonal)],
            "NotEvaluated": ["off-centre coupling", "free-surface correction", "finite heel"],
        }
