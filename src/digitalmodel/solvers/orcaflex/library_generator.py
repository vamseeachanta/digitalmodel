"""
OrcaFlex Library Component Generator

Generates library component YAML files from equipment CSV catalogs.
Library components use object-level properties only (no section headers)
for use with OrcaFlex's IncludeFile directive.

Usage:
    from digitalmodel.solvers.orcaflex.library_generator import LibraryGenerator

    generator = LibraryGenerator(library_path="docs/domains/orcaflex/library")
    generator.generate_from_csv("equipment/buoys.csv", "buoy_types")
"""

import csv
import yaml
from pathlib import Path
from typing import Dict, Any, List, Optional
from datetime import datetime

from digitalmodel.solvers.orcaflex.yaml_utils import OrcaFlexDumper

# Backward-compat alias
OrcaFlexLibraryDumper = OrcaFlexDumper


def csv_row_to_line_type(row: Dict[str, Any]) -> Dict[str, Any]:
    """
    Convert a CSV row to line type properties.

    Supports multiple CSV column naming conventions:
    - OD_m, ID_m, MassPerUnitLength_kg_m (standard)
    - OD, ID, Mass (risers.csv format)
    - Diameter_m, Mass_kg_per_m (alternative)

    Args:
        row: Dictionary with CSV column values

    Returns:
        Dictionary of OrcaFlex line type properties (no section header)
    """
    # Handle different column naming conventions for outer diameter
    od = float(row.get("OD_m", row.get("OD", row.get("Diameter_m", 0.1))))

    # Handle different column naming conventions for inner diameter
    # Note: "ID" in risers.csv is inner diameter, not identifier
    id_val = float(row.get("ID_m", row.get("ID", 0)) if row.get("ID_m") or row.get("ID") else 0)

    # Handle different column naming conventions for mass
    mass = float(row.get("MassPerUnitLength_kg_m", row.get("Mass_kg_per_m", row.get("Mass", 0.1))))

    # Handle EA (axial stiffness) - may be in kN or N
    ea_raw = row.get("EA_kN", row.get("EA", 100000))
    ea = float(ea_raw)

    # Handle EI (bending stiffness) if provided
    ei_raw = row.get("EI_kNm2", row.get("EI"))
    if ei_raw:
        ei = [float(ei_raw), None]
    else:
        ei = [0, None]

    # Handle Poisson ratio
    poisson = float(row.get("PoissonRatio", row.get("Poisson", 0.5)))

    props = {
        "OD": od,
        "ID": id_val,
        "MassPerUnitLength": mass,
        "EA": ea,
        "CompressionIsLimited": True,
        "EI": ei,
        "GJ": 0,
        "PoissonRatio": poisson,
    }

    # Optional properties
    if "Cd" in row:
        props["Cd"] = [float(row["Cd"]), None, 0.4]
    if "Ca" in row:
        props["Ca"] = [float(row["Ca"]), None, 0.07]
    if "SeabedFriction" in row:
        props["SeabedLateralFrictionCoefficient"] = float(row["SeabedFriction"])

    return props


def csv_row_to_buoy_type(row: Dict[str, Any]) -> Dict[str, Any]:
    """
    Convert a CSV row to 6D buoy type properties.

    Args:
        row: Dictionary with CSV column values

    Returns:
        Dictionary of OrcaFlex 6D buoy properties (no section header)
    """
    # Handle mass in tonnes (te) or kg
    if "Mass_te" in row:
        mass = float(row["Mass_te"]) * 1000  # Convert tonnes to kg
    else:
        mass = float(row.get("Mass_kg", 95000))

    diameter = float(row.get("Diameter_m", 12.0))
    height = float(row.get("Height_m", 4.5))

    # Estimate moments of inertia for cylinder
    radius = diameter / 2
    ixx = iyy = mass * (3 * radius**2 + height**2) / 12
    izz = mass * radius**2 / 2

    props = {
        "BuoyType": "Spar buoy",
        "Connection": "Free",
        "DampingRelativeTo": "Earth",
        "Mass": mass,
        "MomentsOfInertia": [round(ixx), round(iyy), round(izz)],
        "CentreOfMass": [0, 0, 0],
        "StackBaseCentre": [0, 0, -height/2],
        "BulkModulus": "Infinity",
        "NormalDragAreaCalculatedFromGeometry": True,
    }

    # Add cylinder data if we have diameter/height
    if diameter and height:
        cd_normal = float(row.get("Cd_Normal", row.get("Cd", 0.8)))
        cd_axial = float(row.get("Cd_Axial", 1.13))
        ca_normal = float(row.get("Ca_Normal", row.get("Cm", 1.0)))
        ca_axial = float(row.get("Ca_Axial", 0.92))

        props["Cylinders"] = [{
            "CylinderOuterDiameter": diameter,
            "CylinderInnerDiameter": 0,
            "CylinderLength": height,
            "CylinderAxialDragArea": 0,
            "DragForceCoefficient": [cd_normal, cd_axial],
            "AddedMassForceCoefficient": [ca_normal, ca_axial],
        }]

    return props


def csv_row_to_vessel_type(row: Dict[str, Any]) -> Dict[str, Any]:
    """
    Convert a CSV row to vessel type properties.

    Note: This creates a basic vessel type. RAO data should be imported
    separately from hydrodynamic analysis tools.

    Args:
        row: Dictionary with CSV column values

    Returns:
        Dictionary of OrcaFlex vessel type properties (no section header)
    """
    props = {
        "Length": float(row.get("Length_m", 250)),
        "RAOResponseUnits": "degrees",
        "RAOWaveUnit": "amplitude",
        "WavesReferredToBy": "period (s)",
        "RAOPhaseConvention": "lags",
        "RAOPhaseUnitsConvention": "degrees",
        "SurgePositive": "forward",
        "SwayPositive": "port",
        "HeavePositive": "up",
        "Symmetry": "xz plane",
    }

    return props


class LibraryGenerator:
    """
    Generates OrcaFlex library component files from CSV equipment catalogs.
    """

    def __init__(self, library_path: str):
        """
        Initialize the library generator.

        Args:
            library_path: Path to the library root directory
        """
        self.library_path = Path(library_path)
        self.library_path.mkdir(parents=True, exist_ok=True)

        # Conversion functions for each category
        self.converters = {
            "line_types": csv_row_to_line_type,
            "buoy_types": csv_row_to_buoy_type,
            "vessel_types": csv_row_to_vessel_type,
        }

    def generate_component(
        self,
        name: str,
        props: Dict[str, Any],
        category: str,
        description: str = ""
    ) -> Path:
        """
        Generate a single library component file.

        Args:
            name: Component name (used for filename)
            props: Dictionary of OrcaFlex properties
            category: Library category (e.g., "line_types")
            description: Optional description comment

        Returns:
            Path to the generated file
        """
        # Create category directory
        category_dir = self.library_path / category
        category_dir.mkdir(parents=True, exist_ok=True)

        # Generate filename (lowercase, underscores)
        filename = name.lower().replace(" ", "_").replace("-", "_") + ".yml"
        file_path = category_dir / filename

        # Build file content
        header = f"""# {name} - Library Component
# Category: {category}
# Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
"""
        if description:
            header += f"# Description: {description}\n"
        header += "# NOTE: No section header - properties only for IncludeFile\n"

        # Dump properties
        yaml_content = yaml.dump(
            props,
            Dumper=OrcaFlexLibraryDumper,
            default_flow_style=False,
            allow_unicode=True,
            sort_keys=False,
            width=1000,
        )

        # Write file
        file_path.write_text(header + yaml_content)

        return file_path

    def generate_from_csv(
        self,
        csv_path: str,
        category: str,
        name_column: str = None
    ) -> List[Path]:
        """
        Generate library components from a CSV file.

        Args:
            csv_path: Path to CSV file
            category: Library category to generate
            name_column: Column to use for component name (auto-detected if None)

        Returns:
            List of generated file paths
        """
        csv_file = Path(csv_path)
        if not csv_file.exists():
            raise FileNotFoundError(f"CSV file not found: {csv_path}")

        converter = self.converters.get(category)
        if not converter:
            raise ValueError(f"Unknown category: {category}")

        generated = []

        with open(csv_file, 'r', encoding='utf-8') as f:
            reader = csv.DictReader(f)

            for row in reader:
                # Determine name column
                if name_column:
                    name = row[name_column]
                else:
                    # Auto-detect name column - prioritize specific ID columns over generic "ID"
                    # (generic "ID" may be inner diameter in line type CSVs)
                    name_columns = [
                        "Name", "RiserID", "RiserName", "PipelineID", "PipelineName",
                        "UmbilicalID", "UmbilicalName", "LineTypeID", "LineName",
                        "BuoyID", "BuoyName", "VesselID", "VesselName",
                        "ChainID", "ChainName", "ComponentID", "ComponentName"
                    ]
                    for col in name_columns:
                        if col in row and row[col]:
                            name = row[col]
                            break
                    else:
                        name = f"Component_{len(generated)+1}"

                # Get description if available
                description = row.get("Description", "")

                # Convert row to properties
                props = converter(row)

                # Generate component file
                file_path = self.generate_component(
                    name=name,
                    props=props,
                    category=category,
                    description=description
                )
                generated.append(file_path)

        return generated

    def generate_index(self) -> Dict[str, List[Dict]]:
        """
        Generate an index of all library components.

        Returns:
            Dictionary mapping categories to component lists
        """
        index = {}

        for category_dir in self.library_path.iterdir():
            if category_dir.is_dir():
                category = category_dir.name
                index[category] = []

                for component_file in sorted(category_dir.glob("*.yml")):
                    try:
                        data = yaml.safe_load(component_file.read_text())
                        index[category].append({
                            "file": component_file.name,
                            "path": str(component_file.relative_to(self.library_path)),
                            "properties": list(data.keys()) if data else []
                        })
                    except yaml.YAMLError:
                        pass

        return index

    def write_index(self) -> Path:
        """
        Write the library index to a YAML file.

        Returns:
            Path to the index file
        """
        index = self.generate_index()
        index_path = self.library_path / "index.yml"

        content = f"""# OrcaFlex Library Index
# Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
#
# Usage in model files:
#   LineTypes:
#     - Name: Chain_84mm
#       IncludeFile: library/line_types/chain_84mm_r4.yml
#
"""
        yaml_content = yaml.dump(index, default_flow_style=False, sort_keys=False)
        index_path.write_text(content + yaml_content)

        return index_path


def main():
    """CLI entry point for library generation."""
    import argparse

    parser = argparse.ArgumentParser(
        description="Generate OrcaFlex library components from CSV"
    )
    parser.add_argument("csv_file", help="Path to CSV equipment catalog")
    parser.add_argument("category", choices=["line_types", "buoy_types", "vessel_types"],
                        help="Library category to generate")
    parser.add_argument("-o", "--output", default="docs/domains/orcaflex/library",
                        help="Library output directory")
    parser.add_argument("--name-column", help="CSV column for component names")

    args = parser.parse_args()

    generator = LibraryGenerator(args.output)
    generated = generator.generate_from_csv(
        args.csv_file,
        args.category,
        args.name_column
    )

    print(f"Generated {len(generated)} components in {args.category}/")
    for path in generated:
        print(f"  - {path.name}")

    # Update index
    index_path = generator.write_index()
    print(f"\nUpdated index: {index_path}")


if __name__ == "__main__":
    main()
