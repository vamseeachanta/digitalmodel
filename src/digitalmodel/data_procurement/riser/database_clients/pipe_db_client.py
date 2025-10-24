# ABOUTME: Pipe specification database for API 5L and API 5CT pipes
# ABOUTME: Embedded database with property calculations and OrcaFlex export

"""
Pipe Specification Database Client
====================================

Repository-based pipe database with complete specifications.

Data Coverage:
- API 5L line pipe (production risers)
- API 5CT casing and tubing (drilling risers)
- Diameters: 2" to 36" (standard sizes)
- Wall thicknesses: SCH 20, 40, 60, 80, 100, 120, 140, 160, STD, XS, XXS
- Grades: X42, X52, X60, X65, X70, X80 (API 5L)

Source: API standards and industry specifications
Authentication: None (repository data)

Example:
    client = PipeSpecificationClient()

    # Find pipes by diameter
    pipes = client.find_by_diameter(10, schedule='SCH 80')

    # Get specific pipe
    pipe = client.get_pipe(diameter=10, schedule='SCH 80', grade='X52')

    # Calculate properties
    props = client.calculate_properties(pipe)

    # Export to OrcaFlex
    yaml_output = client.to_orcaflex_line_type(props)
"""

import logging
from typing import Dict, Any, List, Optional
import numpy as np

logger = logging.getLogger(__name__)


class PipeSpecificationClient:
    """
    Pipe specification database client.

    Embedded database of pipe specifications with complete properties.
    """

    # Schedule to wall thickness mapping (inches)
    SCHEDULE_WALL_THICKNESS = {
        # Common schedules
        'SCH 20': {},  # Populated below
        'SCH 40': {},
        'SCH 60': {},
        'SCH 80': {},
        'SCH 100': {},
        'SCH 120': {},
        'SCH 140': {},
        'SCH 160': {},
        'STD': {},  # Standard (same as SCH 40 for most sizes)
        'XS': {},   # Extra strong (same as SCH 80 for most sizes)
        'XXS': {},  # Double extra strong
    }

    # Standard pipe diameters (inches)
    STANDARD_DIAMETERS = [2, 3, 4, 6, 8, 10, 12, 14, 16, 18, 20, 24, 30, 36]

    # API 5L grades
    API_5L_GRADES = {
        'X42': {'yield_strength': 289, 'tensile_strength': 414},  # MPa
        'X52': {'yield_strength': 359, 'tensile_strength': 455},
        'X60': {'yield_strength': 414, 'tensile_strength': 517},
        'X65': {'yield_strength': 448, 'tensile_strength': 531},
        'X70': {'yield_strength': 483, 'tensile_strength': 565},
        'X80': {'yield_strength': 552, 'tensile_strength': 621},
    }

    # Material properties (steel)
    STEEL_PROPERTIES = {
        'youngs_modulus': 207,  # GPa
        'poissons_ratio': 0.30,
        'density': 7850,  # kg/m³
        'shear_modulus': 80,  # GPa
    }

    def __init__(self):
        """Initialize Pipe Specification Client."""
        # Build schedule-diameter-wall thickness mapping
        self._initialize_schedule_mapping()

        # Build pipe database
        self.pipe_database = self._initialize_pipe_database()

        # Build indexes for fast lookup
        self._build_indexes()

        logger.info(f"Initialized PipeSpecificationClient with {len(self.pipe_database)} pipe specifications")

    def _initialize_schedule_mapping(self):
        """Initialize schedule to wall thickness mapping."""
        # Wall thickness data from API 5L standard (inches)
        # Format: diameter: {schedule: wall_thickness}

        wall_data = {
            2: {'SCH 40': 0.154, 'SCH 80': 0.218, 'STD': 0.154, 'XS': 0.218, 'XXS': 0.436},
            3: {'SCH 40': 0.216, 'SCH 80': 0.300, 'STD': 0.216, 'XS': 0.300, 'XXS': 0.600},
            4: {'SCH 40': 0.237, 'SCH 80': 0.337, 'STD': 0.237, 'XS': 0.337, 'XXS': 0.674},
            6: {'SCH 40': 0.280, 'SCH 80': 0.432, 'STD': 0.280, 'XS': 0.432, 'XXS': 0.864},
            8: {'SCH 20': 0.250, 'SCH 40': 0.322, 'SCH 60': 0.406, 'SCH 80': 0.500, 'SCH 100': 0.594,
                'SCH 120': 0.719, 'SCH 140': 0.812, 'SCH 160': 0.906, 'STD': 0.322, 'XS': 0.500, 'XXS': 0.875},
            10: {'SCH 20': 0.250, 'SCH 40': 0.365, 'SCH 60': 0.500, 'SCH 80': 0.594, 'SCH 100': 0.719,
                 'SCH 120': 0.844, 'SCH 140': 1.000, 'SCH 160': 1.125, 'STD': 0.365, 'XS': 0.500, 'XXS': 1.000},
            12: {'SCH 20': 0.250, 'SCH 40': 0.375, 'SCH 60': 0.562, 'SCH 80': 0.688, 'SCH 100': 0.844,
                 'SCH 120': 1.000, 'SCH 140': 1.125, 'SCH 160': 1.312, 'STD': 0.375, 'XS': 0.500, 'XXS': 1.000},
            14: {'SCH 20': 0.250, 'SCH 40': 0.375, 'SCH 60': 0.594, 'SCH 80': 0.750, 'SCH 100': 0.938,
                 'SCH 120': 1.094, 'SCH 140': 1.250, 'SCH 160': 1.406, 'STD': 0.375, 'XS': 0.500},
            16: {'SCH 20': 0.250, 'SCH 40': 0.375, 'SCH 60': 0.656, 'SCH 80': 0.844, 'SCH 100': 1.031,
                 'SCH 120': 1.219, 'SCH 140': 1.438, 'SCH 160': 1.594, 'STD': 0.375, 'XS': 0.500},
            18: {'SCH 20': 0.250, 'SCH 40': 0.375, 'SCH 60': 0.750, 'SCH 80': 0.938, 'SCH 100': 1.156,
                 'SCH 120': 1.375, 'SCH 140': 1.562, 'SCH 160': 1.781, 'STD': 0.375, 'XS': 0.500},
            20: {'SCH 20': 0.250, 'SCH 40': 0.375, 'SCH 60': 0.812, 'SCH 80': 1.031, 'SCH 100': 1.281,
                 'SCH 120': 1.500, 'SCH 140': 1.750, 'SCH 160': 1.969, 'STD': 0.375, 'XS': 0.500},
            24: {'SCH 20': 0.250, 'SCH 40': 0.375, 'SCH 60': 0.969, 'SCH 80': 1.219, 'SCH 100': 1.531,
                 'SCH 120': 1.812, 'SCH 140': 2.062, 'SCH 160': 2.344, 'STD': 0.375, 'XS': 0.500},
            30: {'SCH 20': 0.312, 'SCH 40': 0.375, 'SCH 60': 1.000, 'SCH 80': 1.250, 'STD': 0.375, 'XS': 0.500},
            36: {'SCH 20': 0.312, 'SCH 40': 0.375, 'SCH 60': 1.000, 'SCH 80': 1.250, 'STD': 0.375, 'XS': 0.500},
        }

        # Populate schedule mapping
        for diameter, schedules in wall_data.items():
            for schedule, wall_thickness in schedules.items():
                self.SCHEDULE_WALL_THICKNESS[schedule][diameter] = wall_thickness

    def _initialize_pipe_database(self) -> List[Dict[str, Any]]:
        """
        Initialize pipe database with standard specifications.

        Returns:
            List of pipe specifications
        """
        pipes = []

        # Generate pipes for all combinations
        for diameter in self.STANDARD_DIAMETERS:
            for schedule in self.SCHEDULE_WALL_THICKNESS.keys():
                # Check if this diameter has this schedule
                if diameter not in self.SCHEDULE_WALL_THICKNESS[schedule]:
                    continue

                wall_thickness = self.SCHEDULE_WALL_THICKNESS[schedule][diameter]

                # Create pipes for each grade
                for grade in self.API_5L_GRADES.keys():
                    pipe = self._generate_pipe_spec(diameter, wall_thickness, schedule, grade)
                    pipes.append(pipe)

        return pipes

    def _generate_pipe_spec(self, diameter: float, wall_thickness: float,
                           schedule: str, grade: str) -> Dict[str, Any]:
        """
        Generate complete pipe specification.

        Args:
            diameter: Nominal diameter in inches
            wall_thickness: Wall thickness in inches
            schedule: Schedule designation
            grade: API 5L grade

        Returns:
            Complete pipe specification dict
        """
        # Convert to metric
        od_mm = diameter * 25.4
        wall_mm = wall_thickness * 25.4
        id_mm = od_mm - 2 * wall_mm

        od_m = od_mm / 1000
        wall_m = wall_mm / 1000
        id_m = id_mm / 1000

        # Material properties
        steel = self.STEEL_PROPERTIES
        grade_props = self.API_5L_GRADES[grade]

        # Cross-sectional area (steel only)
        area_m2 = np.pi / 4 * (od_m**2 - id_m**2)

        # Mass per meter (kg/m)
        mass_per_meter = area_m2 * steel['density']

        # Axial stiffness EA (N)
        ea = area_m2 * steel['youngs_modulus'] * 1e9  # Convert GPa to Pa

        # Moment of inertia I (m⁴)
        i = np.pi / 64 * (od_m**4 - id_m**4)

        # Bending stiffness EI (N·m²)
        ei = steel['youngs_modulus'] * 1e9 * i

        # Polar moment of inertia J (m⁴)
        j = np.pi / 32 * (od_m**4 - id_m**4)

        # Torsional stiffness GJ (N·m²/rad)
        gj = steel['shear_modulus'] * 1e9 * j

        # Section modulus Z (m³)
        z = i / (od_m / 2)

        # Burst pressure (Barlow's formula) - MPa
        # P = 2 * S * t / D where S = SMYS, t = wall thickness, D = OD
        burst_pressure = 2 * grade_props['yield_strength'] * wall_m / od_m

        # Collapse pressure (API RP 2RD simplified)
        # Elastic collapse: P = 2E * (t/D)^3
        # Simplified for initial estimate
        collapse_pressure = 2 * steel['youngs_modulus'] * 1000 * (wall_m / od_m)**3

        return {
            # Identification
            'nominal_diameter': diameter,  # inches
            'schedule': schedule,
            'grade': grade,
            'standard': 'API 5L',

            # Dimensions (metric)
            'outer_diameter_mm': round(od_mm, 2),
            'inner_diameter_mm': round(id_mm, 2),
            'wall_thickness_mm': round(wall_mm, 2),

            # Dimensions (imperial)
            'outer_diameter_in': diameter,
            'inner_diameter_in': round(diameter - 2 * wall_thickness, 3),
            'wall_thickness_in': wall_thickness,

            # Mechanical properties
            'yield_strength_mpa': grade_props['yield_strength'],
            'tensile_strength_mpa': grade_props['tensile_strength'],
            'youngs_modulus_gpa': steel['youngs_modulus'],
            'poissons_ratio': steel['poissons_ratio'],
            'shear_modulus_gpa': steel['shear_modulus'],

            # Physical properties
            'density_kg_m3': steel['density'],
            'mass_per_meter_kg': round(mass_per_meter, 2),

            # Calculated properties
            'cross_sectional_area_m2': area_m2,
            'axial_stiffness_ea_n': ea,
            'bending_stiffness_ei_nm2': ei,
            'torsional_stiffness_gj_nm2': gj,
            'moment_of_inertia_m4': i,
            'section_modulus_m3': z,

            # Pressure ratings
            'burst_pressure_mpa': round(burst_pressure, 1),
            'collapse_pressure_mpa': round(collapse_pressure, 1),
        }

    def _build_indexes(self):
        """Build indexes for fast lookup."""
        # Index by diameter
        self.diameter_index = {}
        for pipe in self.pipe_database:
            diameter = pipe['nominal_diameter']
            if diameter not in self.diameter_index:
                self.diameter_index[diameter] = []
            self.diameter_index[diameter].append(pipe)

        # Index by diameter + schedule
        self.diameter_schedule_index = {}
        for pipe in self.pipe_database:
            key = (pipe['nominal_diameter'], pipe['schedule'])
            if key not in self.diameter_schedule_index:
                self.diameter_schedule_index[key] = []
            self.diameter_schedule_index[key].append(pipe)

        logger.info(f"Built indexes: {len(self.diameter_index)} diameters, "
                   f"{len(self.diameter_schedule_index)} diameter-schedule combinations")

    def find_by_diameter(self, diameter: float, schedule: str = None,
                        grade: str = None) -> List[Dict[str, Any]]:
        """
        Find pipes matching diameter criteria.

        Args:
            diameter: Nominal diameter in inches
            schedule: Optional schedule filter
            grade: Optional grade filter

        Returns:
            List of matching pipes

        Example:
            # Find all 10" pipes
            pipes = client.find_by_diameter(10)

            # Find 10" SCH 80 pipes
            pipes = client.find_by_diameter(10, schedule='SCH 80')

            # Find 10" SCH 80 X52 pipe
            pipes = client.find_by_diameter(10, schedule='SCH 80', grade='X52')
        """
        # Use index for fast lookup
        if schedule:
            key = (diameter, schedule)
            pipes = self.diameter_schedule_index.get(key, [])
        else:
            pipes = self.diameter_index.get(diameter, [])

        # Apply grade filter if specified
        if grade:
            pipes = [p for p in pipes if p['grade'] == grade]

        return [p.copy() for p in pipes]

    def find_by_pressure_rating(self, internal_pressure: float, diameter: float,
                               safety_factor: float = 1.5) -> List[Dict[str, Any]]:
        """
        Find pipes by pressure rating (Barlow's formula).

        Args:
            internal_pressure: Required internal pressure in MPa
            diameter: Nominal diameter in inches
            safety_factor: Safety factor (default 1.5)

        Returns:
            List of suitable pipes sorted by wall thickness

        Example:
            # Find pipes for 10 MPa internal pressure
            pipes = client.find_by_pressure_rating(10.0, diameter=10)
        """
        required_pressure = internal_pressure * safety_factor

        # Get all pipes of this diameter
        all_pipes = self.diameter_index.get(diameter, [])

        # Filter by burst pressure
        suitable_pipes = [
            p for p in all_pipes
            if p['burst_pressure_mpa'] >= required_pressure
        ]

        # Sort by wall thickness (thinnest first for optimization)
        suitable_pipes.sort(key=lambda p: p['wall_thickness_mm'])

        return [p.copy() for p in suitable_pipes]

    def calculate_properties(self, pipe: Dict[str, Any],
                            coatings: List[Dict[str, Any]] = None,
                            contents_density: float = 0) -> Dict[str, Any]:
        """
        Calculate complete riser properties with coatings and contents.

        Args:
            pipe: Base pipe specification
            coatings: List of coating dicts with 'type', 'thickness' (mm), 'density' (kg/m³)
            contents_density: Contents density in kg/m³ (default 0 for empty)

        Returns:
            Complete riser properties dict

        Example:
            pipe = client.find_by_diameter(10, schedule='SCH 80', grade='X52')[0]
            props = client.calculate_properties(
                pipe=pipe,
                coatings=[
                    {'type': '3LPE', 'thickness': 3.2, 'density': 940},
                    {'type': 'insulation', 'thickness': 50, 'density': 500}
                ],
                contents_density=850  # oil
            )
        """
        od_m = pipe['outer_diameter_mm'] / 1000
        id_m = pipe['inner_diameter_mm'] / 1000

        # Start with base pipe properties
        mass_dry = pipe['mass_per_meter_kg']

        # Add coating mass
        coating_mass = 0
        coating_od = od_m

        if coatings:
            for coating in coatings:
                thickness_m = coating['thickness'] / 1000

                # Coating volume per meter
                coating_volume = np.pi / 4 * ((coating_od + 2 * thickness_m)**2 - coating_od**2)
                coating_mass += coating_volume * coating['density']

                # Update OD for next layer
                coating_od += 2 * thickness_m

        # Add contents mass
        contents_volume = np.pi / 4 * id_m**2
        contents_mass = contents_volume * contents_density

        # Total masses
        mass_per_meter_dry = mass_dry + coating_mass
        mass_per_meter_wet = mass_per_meter_dry + contents_mass

        # Submerged mass (in seawater, density 1025 kg/m³)
        # Buoyancy based on outer diameter with coatings
        displaced_volume = np.pi / 4 * coating_od**2
        buoyancy_force = displaced_volume * 1025
        mass_per_meter_submerged = mass_per_meter_wet - buoyancy_force

        # Hydrodynamic coefficients (bare pipe, conservatively)
        cd_normal = 1.2  # Drag coefficient normal to axis
        ca_normal = 1.0  # Added mass coefficient normal to axis
        cd_axial = 0.01  # Drag coefficient along axis
        ca_axial = 0.0   # Added mass coefficient along axis

        return {
            **pipe,  # Include all pipe properties

            # Mass properties
            'mass_per_meter_dry_kg': round(mass_per_meter_dry, 2),
            'mass_per_meter_wet_kg': round(mass_per_meter_wet, 2),
            'mass_per_meter_submerged_kg': round(mass_per_meter_submerged, 2),

            # Coating properties
            'outer_diameter_with_coatings_mm': round(coating_od * 1000, 2),
            'coating_mass_per_meter_kg': round(coating_mass, 2),
            'coatings': coatings or [],

            # Contents
            'contents_density_kg_m3': contents_density,
            'contents_mass_per_meter_kg': round(contents_mass, 2),

            # Hydrodynamic coefficients
            'cd_normal': cd_normal,
            'ca_normal': ca_normal,
            'cd_axial': cd_axial,
            'ca_axial': ca_axial,
        }

    def to_orcaflex_line_type(self, pipe_props: Dict[str, Any],
                              name: str = None) -> str:
        """
        Convert pipe properties to OrcaFlex line type YAML.

        Args:
            pipe_props: Pipe properties dict (from calculate_properties)
            name: Line type name (default: Pipe_{diameter}_{schedule}_{grade})

        Returns:
            OrcaFlex line type YAML string

        Example:
            pipe = client.find_by_diameter(10, schedule='SCH 80', grade='X52')[0]
            props = client.calculate_properties(pipe, coatings=[...])
            yaml_output = client.to_orcaflex_line_type(props)
        """
        import yaml

        # Generate name
        if name is None:
            name = f"Pipe_{pipe_props['nominal_diameter']}in_{pipe_props['schedule']}_{pipe_props['grade']}"

        # Build OrcaFlex line type
        od_m = pipe_props.get('outer_diameter_with_coatings_mm',
                             pipe_props['outer_diameter_mm']) / 1000
        id_m = pipe_props['inner_diameter_mm'] / 1000

        line_type = {
            'Name': name,
            'Category': 'GeneralLineType',

            # Geometric properties
            'OD': round(od_m, 5),  # m
            'ID': round(id_m, 5),  # m

            # Mass properties (use submerged mass for OrcaFlex)
            'Mass per unit length': round(pipe_props['mass_per_meter_submerged_kg'], 2),

            # Stiffness properties
            'EA': round(pipe_props['axial_stiffness_ea_n'], 0),  # N
            'EI': round(pipe_props['bending_stiffness_ei_nm2'], 0),  # N·m²
            'GJ': round(pipe_props['torsional_stiffness_gj_nm2'], 0),  # N·m²/rad

            # Hydrodynamic properties
            'Cd (Normal)': pipe_props.get('cd_normal', 1.2),
            'Ca (Normal)': pipe_props.get('ca_normal', 1.0),
            'Cd (Axial)': pipe_props.get('cd_axial', 0.01),
            'Ca (Axial)': pipe_props.get('ca_axial', 0.0),

            # Metadata
            'Grade': pipe_props['grade'],
            'Standard': pipe_props['standard'],
            'Description': (f"API {pipe_props['standard']} pipe "
                          f"{pipe_props['nominal_diameter']}\" "
                          f"{pipe_props['schedule']} {pipe_props['grade']}")
        }

        return yaml.dump({'LineType': line_type},
                        default_flow_style=False,
                        sort_keys=False)
