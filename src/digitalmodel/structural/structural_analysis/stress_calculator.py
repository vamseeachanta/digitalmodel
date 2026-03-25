"""
Stress Calculator

Calculates stresses in structural members including:
- Beam bending stress
- Shear stress
- Torsional stress
- Hoop and longitudinal stress (pressure vessels)
"""

from .models import MaterialProperties


class StressCalculator:
    """Calculate stresses in structural members."""

    def __init__(self, material: MaterialProperties):
        self.material = material

    def beam_stress(
        self,
        axial_force: float,
        moment_y: float,
        moment_z: float,
        area: float,
        I_y: float,
        I_z: float,
        y: float,
        z: float
    ) -> float:
        """
        Calculate combined bending stress in a beam.

        Args:
            axial_force: Axial force (N)
            moment_y: Moment about y-axis (N·m)
            moment_z: Moment about z-axis (N·m)
            area: Cross-sectional area (m²)
            I_y: Moment of inertia about y (m⁴)
            I_z: Moment of inertia about z (m⁴)
            y: Distance from neutral axis in y (m)
            z: Distance from neutral axis in z (m)

        Returns:
            Normal stress (MPa)
        """
        sigma_axial = axial_force / area / 1e6  # Convert to MPa
        sigma_bending_y = moment_y * z / I_y / 1e6
        sigma_bending_z = moment_z * y / I_z / 1e6

        return sigma_axial + sigma_bending_y + sigma_bending_z

    def shear_stress(
        self,
        shear_force: float,
        Q: float,
        I: float,
        t: float
    ) -> float:
        """
        Calculate shear stress using VQ/It formula.

        Args:
            shear_force: Shear force (N)
            Q: First moment of area (m³)
            I: Moment of inertia (m⁴)
            t: Thickness at section (m)

        Returns:
            Shear stress (MPa)
        """
        return shear_force * Q / (I * t) / 1e6

    def torsional_stress(
        self,
        torque: float,
        r: float,
        J: float
    ) -> float:
        """
        Calculate torsional shear stress.

        Args:
            torque: Applied torque (N·m)
            r: Radial distance from center (m)
            J: Polar moment of inertia (m⁴)

        Returns:
            Shear stress (MPa)
        """
        return torque * r / J / 1e6

    def hoop_stress(
        self,
        pressure: float,
        radius: float,
        thickness: float
    ) -> float:
        """
        Calculate hoop stress in thin-walled cylinder.

        Args:
            pressure: Internal pressure (MPa)
            radius: Inner radius (m)
            thickness: Wall thickness (m)

        Returns:
            Hoop stress (MPa)
        """
        return pressure * radius / thickness

    def longitudinal_stress(
        self,
        pressure: float,
        radius: float,
        thickness: float
    ) -> float:
        """
        Calculate longitudinal stress in thin-walled cylinder.

        Args:
            pressure: Internal pressure (MPa)
            radius: Inner radius (m)
            thickness: Wall thickness (m)

        Returns:
            Longitudinal stress (MPa)
        """
        return pressure * radius / (2 * thickness)
