"""Builder for LineTypes section of OrcaFlex riser models.

This builder generates LineType definitions for riser line types,
using the General category format for flexible risers.
"""

from typing import Any

from .base import BaseBuilder
from .registry import BuilderRegistry


@BuilderRegistry.register("05_riser_line_types.yml", order=41)
class RiserLineTypeBuilder(BaseBuilder):
    """Builds the LineTypes section for riser models.

    Generates LineType entries for each riser line type defined in the spec.
    Uses the OrcaFlex 'General' category for flexible riser properties.

    Reference: OrcaFlex LineTypes documentation for General category.
    """

    def should_generate(self) -> bool:
        """Only generate for riser models."""
        return self.spec.is_riser()

    def build(self) -> dict[str, Any]:
        """Build the LineTypes section from riser line type definitions.

        Returns:
            Dictionary with 'LineTypes' key containing list of line type
            definitions for each riser line type.
        """
        riser = self.spec.riser
        line_types_list = []
        line_type_names = []

        for lt in riser.line_types:
            line_type = self._build_riser_line_type(lt)
            line_types_list.append(line_type)
            line_type_names.append(lt.name)

        # Register line type names for cross-builder reference
        self._register_entity("riser_line_type_names", line_type_names)
        self._register_entity("line_type_names", line_type_names)

        return {"LineTypes": line_types_list}

    def _build_riser_line_type(self, lt) -> dict[str, Any]:
        """Build a single riser LineType entry.

        Uses the General category format for flexible riser properties.

        Args:
            lt: RiserLineType object from spec.

        Returns:
            Dictionary representing an OrcaFlex LineType entry.
        """
        # Calculate mass per unit length if using structural properties
        mass = lt.mass_per_length

        # Handle drag coefficient - can be float or string (table reference)
        if isinstance(lt.drag_coefficient, str):
            # Table reference
            cd_values = [lt.drag_coefficient, None, lt.axial_drag_coefficient]
        else:
            # Numeric value
            cd_values = [lt.drag_coefficient, None, lt.axial_drag_coefficient]

        line_type_def = {
            "Name": lt.name,
            "Category": "General",
            "OD": lt.outer_diameter,
            "ID": lt.inner_diameter,
            "CentreOfMass": [0, 0],
            "BulkModulus": "Infinity",
            "MassPerUnitLength": mass,
            "CompressionIsLimited": "No",
            "MinRadius": [lt.min_bend_radius, None] if lt.min_bend_radius else [None, None],
            "EI": [lt.bending_stiffness, None],
            "EA": lt.axial_stiffness,
            "PoissonRatio": lt.poisson_ratio,
            "ExpansionTable": "None",
            "GJ": lt.torsional_stiffness,
            "TensionTorqueCoupling": 0,
        }

        # Add contact diameter if specified
        if lt.contact_diameter:
            line_type_def["OuterContactDiameter"] = lt.contact_diameter
        else:
            line_type_def["OuterContactDiameter"] = None

        line_type_def["InnerContactDiameter"] = None
        line_type_def["ClashStiffness"] = 0

        # Hydrodynamic coefficients
        line_type_def["Ca"] = [lt.added_mass_coefficient, None, 0]
        line_type_def["Cm"] = [None, None, None]
        line_type_def["Cs"] = 0
        line_type_def["Ce"] = 0
        line_type_def["Cd"] = cd_values
        line_type_def["Cl"] = 0
        line_type_def["NormalDragLiftDiameter"] = None
        line_type_def["AxialDragLiftDiameter"] = None

        # Stress calculation diameters
        line_type_def["StressOD"] = lt.stress_od
        line_type_def["StressID"] = lt.stress_id

        # Allowable stress/tension
        if lt.allowable_tension:
            line_type_def["AllowableTension"] = lt.allowable_tension
        line_type_def["AllowableStress"] = None

        # Load factors
        line_type_def["TensileStressLoadingFactor"] = 1
        line_type_def["BendingStressLoadingFactor"] = 1
        line_type_def["ShearStressLoadingFactor"] = 1
        line_type_def["TorsionalStressLoadingFactor"] = 1

        # Seabed friction
        line_type_def["SeabedLateralFrictionCoefficient"] = 0.5
        line_type_def["SeabedAxialFrictionCoefficient"] = None

        # Damping
        line_type_def["RayleighDampingCoefficients"] = "(no damping)"

        return line_type_def
