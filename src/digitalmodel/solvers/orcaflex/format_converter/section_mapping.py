"""OrcaFlex section-to-file mappings.

Canonical source for SECTION_MAPPING (section name -> include filename)
and INPUT_PARAMETERS (OrcaFlex paths -> simplified parameter names).

Extracted from scripts/conversion/yaml_to_include.py.
"""

# Maps OrcaFlex YAML section names to numbered include filenames.
# Numbered prefixes (01-19) define the standard include order.
SECTION_MAPPING: dict[str, str] = {
    'General': '01_general.yml',
    'VariableData': '02_var_data.yml',
    'Environment': '03_environment.yml',
    'VesselTypes': '04_vessel_types.yml',
    'LineTypes': '05_line_types.yml',
    'Vessels': '06_vessels.yml',
    'Lines': '07_lines.yml',
    '6DBuoys': '08_buoys.yml',
    'Buoys': '08_buoys.yml',
    'BuoyTypes': '08a_buoy_types.yml',
    'Shapes': '09_shapes.yml',
    'Constraints': '10_constraints.yml',
    'Links': '11_links.yml',
    'Winches': '12_winches.yml',
    'Supports': '13_supports.yml',
    'SupportTypes': '13_supports.yml',
    'MorisonElementTypes': '14_morison.yml',
    '3DBuoys': '08_buoys.yml',
    'Groups': '10_groups.yml',
    'TurbineTypes': '14_turbine_types.yml',
    'Turbines': '15_turbines.yml',
    'DragChainTypes': '16_drag_chain_types.yml',
    'DragChains': '17_drag_chains.yml',
    'FlexJoints': '18_flex_joints.yml',
    'SolidFrictionCoefficients': '19_friction.yml',
}

# Reverse mapping: include filename -> list of section names
REVERSE_SECTION_MAPPING: dict[str, list[str]] = {}
for _section, _file in SECTION_MAPPING.items():
    REVERSE_SECTION_MAPPING.setdefault(_file, []).append(_section)

# Maps OrcaFlex parameter paths to simplified input parameter names.
# Used for extracting key parameters for parametric analysis.
INPUT_PARAMETERS: dict[str, dict[str, str]] = {
    'Environment': {
        'SeabedOriginDepth': 'water_depth',
        'WaterDepth': 'water_depth',
        'WaveHeight': 'hs',
        'WavePeriod': 'tp',
        'RefCurrentSpeed': 'current_speed',
        'RefCurrentDirection': 'current_direction',
        'WindSpeed': 'wind_speed',
        'Density': 'water_density',
    },
    'General': {
        'StageDuration': 'stage_durations',
        'ImplicitConstantTimeStep': 'time_step',
    },
}
