"""Builder for the LineTypes section of OrcaFlex models.

This builder generates LineType definitions for homogeneous pipe segments,
referencing coating names from the VarDataBuilder via context.
"""

from typing import Any

from .base import BaseBuilder
from .registry import BuilderRegistry


# Default material properties by grade
MATERIAL_PROPERTIES = {
    "X65": {
        "MaterialDensity": 7.85,  # te/m3
        "E": 207e6,  # kN/m2
        "PoissonRatio": 0.293,
        "DNVSTF101Fy": 450e3,  # kN/m2
    },
    "X70": {
        "MaterialDensity": 7.85,
        "E": 207e6,
        "PoissonRatio": 0.293,
        "DNVSTF101Fy": 485e3,
    },
    "X52": {
        "MaterialDensity": 7.85,
        "E": 207e6,
        "PoissonRatio": 0.293,
        "DNVSTF101Fy": 360e3,
    },
    "X60": {
        "MaterialDensity": 7.85,
        "E": 207e6,
        "PoissonRatio": 0.293,
        "DNVSTF101Fy": 415e3,
    },
}

# Default hydrodynamic coefficients for pipes
DEFAULT_HYDRO_COEFFS = {
    "Cdn": 1.18,  # Normal drag coefficient
    "Cdz": 0.008,  # Axial drag coefficient
    "Cl": 0,  # Lift coefficient
    "Can": 1,  # Normal added mass coefficient
    "Caz": 0,  # Axial added mass coefficient
    "Cs": 0,  # Slam coefficient
    "Ce": 0,  # Exit coefficient
}


@BuilderRegistry.register("05_line_types.yml", order=40)
class LineTypeBuilder(BaseBuilder):
    """Builds the LineTypes section of the OrcaFlex model.

    Generates LineType entries for each unique segment type in the pipeline
    definition. Each LineType includes:
    - Pipe dimensions (OD, ID)
    - Material properties (density, elastic modulus)
    - Hydrodynamic coefficients
    - Coating reference (from VarData)
    - DNV-ST-F101 parameters for code checks

    Reference: 05_line_types.yml in modular include format.
    """

    def build(self) -> dict[str, Any]:
        """Build the LineTypes section from pipeline segments.

        Returns:
            Dictionary with 'LineTypes' key containing list of line type
            definitions for each unique segment type.
        """
        pipeline = self.spec.pipeline
        dimensions = pipeline.dimensions

        # Calculate inner diameter
        od = dimensions.outer_diameter
        id_value = od - 2 * dimensions.wall_thickness

        # Get material properties
        material_grade = pipeline.material.upper()
        material_props = MATERIAL_PROPERTIES.get(
            material_grade, MATERIAL_PROPERTIES["X65"]
        )

        # Override with spec values if provided
        youngs_modulus = (
            pipeline.youngs_modulus
            if pipeline.youngs_modulus
            else material_props["E"]
        )
        poissons_ratio = (
            pipeline.poissons_ratio
            if pipeline.poissons_ratio
            else material_props["PoissonRatio"]
        )

        # Get unique segment types
        unique_types = []
        seen = set()
        for segment in pipeline.segments:
            if segment.type not in seen:
                unique_types.append(segment.type)
                seen.add(segment.type)

        line_types_list = []
        line_type_names = []

        for segment_type in unique_types:
            # Parse segment type to extract coating name
            # Expected format: "X65+coating+CWC120" or similar
            parts = segment_type.split("+")
            if len(parts) >= 2:
                # Reconstruct coating name from parts after material
                coating_name = "+".join(parts[1:])
            else:
                coating_name = "coating"

            line_type = self._build_line_type_entry(
                name=segment_type,
                od=od,
                id_value=id_value,
                material_density=material_props["MaterialDensity"],
                youngs_modulus=youngs_modulus,
                poissons_ratio=poissons_ratio,
                coating_name=coating_name,
                dnv_fy=material_props.get("DNVSTF101Fy", 450e3),
            )
            line_types_list.append(line_type)
            line_type_names.append(segment_type)

        # Optionally add winch wire line type if needed for installation models
        if self.spec.equipment.tugs or self.spec.equipment.tensioner:
            winch_wire = self._build_winch_wire_line_type()
            line_types_list.append(winch_wire)
            line_type_names.append("Winch wire_LT")

        # Register line type names for cross-builder reference
        self._register_entity("line_type_names", line_type_names)

        return {"LineTypes": line_types_list}

    def _build_line_type_entry(
        self,
        name: str,
        od: float,
        id_value: float,
        material_density: float,
        youngs_modulus: float,
        poissons_ratio: float,
        coating_name: str,
        dnv_fy: float,
    ) -> dict[str, Any]:
        """Build a single homogeneous pipe LineType entry.

        Args:
            name: LineType name (segment type identifier)
            od: Outer diameter (m)
            id_value: Inner diameter (m)
            material_density: Steel density (te/m3)
            youngs_modulus: Young's modulus (kN/m2)
            poissons_ratio: Poisson's ratio
            coating_name: Reference name for VarData coating
            dnv_fy: DNV-ST-F101 yield strength (kN/m2)

        Returns:
            Dictionary representing an OrcaFlex LineType entry.
        """
        return {
            "Name": name,
            "Category": "Homogeneous pipe",
            "OD": od,
            "ID": id_value,
            "MaterialDensity": material_density,
            "E": youngs_modulus,
            "PoissonRatio": poissons_ratio,
            "ExpansionTable": "None",
            # Hydrodynamic coefficients
            "Cdn": DEFAULT_HYDRO_COEFFS["Cdn"],
            "Cdz": DEFAULT_HYDRO_COEFFS["Cdz"],
            "Cl": DEFAULT_HYDRO_COEFFS["Cl"],
            "Can": DEFAULT_HYDRO_COEFFS["Can"],
            "Caz": DEFAULT_HYDRO_COEFFS["Caz"],
            "Cs": DEFAULT_HYDRO_COEFFS["Cs"],
            "Ce": DEFAULT_HYDRO_COEFFS["Ce"],
            # Seabed interaction
            "SeabedLateralFrictionCoefficient": 0.5,
            "SeabedAxialFrictionCoefficient": None,
            "ClashStiffness": 0,
            "AllowableStress": None,
            # Damping
            "RayleighDampingCoefficients": "(no damping)",
            # Coating reference (from VarData)
            "CoatingThickness": coating_name,
            "LiningThickness": 0,
            "AdditionalEI": 0,
            # DNV-ST-F101 parameters
            "DNVSTF101Pmin": None,
            "DNVSTF101T2": None,
            "DNVSTF101Fy": dnv_fy,
            "DNVSTF101Fu": 0,
            "DNVSTF101E": 0,
            "DNVSTF101AlphaH": 0,
            "DNVSTF101O0": 0,
            "DNVSTF101SimplifiedStrainLimit": 0.25,
            "DNVSTF101GammaSCLB": 0,
            "DNVSTF101GammaSCDC": 0,
            "DNVSTF101GammaM": 0,
            "DNVSTF101AlphaFab": 0,
            "DNVSTF101AlphaGW": 0,
            "DNVSTF101AlphaPm": 0,
            "DNVSTF101AlphaEpsilonC": 0,
            "DNVSTF101AlphaMat": None,
            "DNVSTF101TCRA": 0,
            "DNVSTF101FyCRA": 0,
            "DNVSTF101FuCRA": 0,
        }

    def _build_winch_wire_line_type(self) -> dict[str, Any]:
        """Build a winch wire LineType for installation models.

        Returns:
            Dictionary representing a general (wire) LineType entry.
        """
        return {
            "Name": "Winch wire_LT",
            "Category": "General",
            "OD": 0.08,
            "ID": 0,
            "CentreOfMass": [0, 0],
            "BulkModulus": "Infinity",
            "MassPerUnitLength": 0.0399,
            "CompressionIsLimited": "Yes",
            "AllowableTension": 6300,
            "MinRadius": [None, None],
            "EI": [0, None],
            "EA": 404e3,
            "PoissonRatio": 0.5,
            "ExpansionTable": "None",
            "GJ": 80,
            "TensionTorqueCoupling": 0,
            "OuterContactDiameter": None,
            "InnerContactDiameter": None,
            "ClashStiffness": 0,
            "Ca": [1, None, 0],
            "Cm": [None, None, None],
            "Cs": 0,
            "Ce": 0,
            "Cd": [1.2, None, 0.008],
            "Cl": 0,
            "NormalDragLiftDiameter": None,
            "AxialDragLiftDiameter": None,
            "StressOD": None,
            "StressID": None,
            "AllowableStress": None,
            "TensileStressLoadingFactor": 1,
            "BendingStressLoadingFactor": 1,
            "ShearStressLoadingFactor": 1,
            "TorsionalStressLoadingFactor": 1,
            "SeabedLateralFrictionCoefficient": 0.5,
            "SeabedAxialFrictionCoefficient": None,
            "RayleighDampingCoefficients": "(no damping)",
            "Pen": [1, "Solid", "Lime"],
        }
