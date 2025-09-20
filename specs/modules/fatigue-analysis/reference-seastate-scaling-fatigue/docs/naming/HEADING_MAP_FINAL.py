#!/usr/bin/env python3
"""
Heading mapping for fatigue analysis output files
Uses 000deg format (3-digit with leading zeros)
"""

# Actual heading mapping from reference metadata
# Format: 000deg (3-digit with leading zeros)
HEADING_MAP = {
    # Wave cases - 6 directions with 3 Tp values each
    'wave01': '045deg', 'wave02': '045deg', 'wave03': '045deg',  # 45° direction
    'wave04': '070deg', 'wave05': '070deg', 'wave06': '070deg',  # 70° direction  
    'wave07': '090deg', 'wave08': '090deg', 'wave09': '090deg',  # 90° direction
    'wave10': '125deg', 'wave11': '125deg', 'wave12': '125deg',  # 125° direction
    'wave13': '150deg', 'wave14': '150deg', 'wave15': '150deg',  # 150° direction
    'wave16': '310deg', 'wave17': '310deg', 'wave18': '310deg',  # 310° direction
    
    # Wind cases - 16 compass directions (22.5° increments)
    'wind01': '000deg',  # 0° North
    'wind02': '023deg',  # 22.5° NNE (rounded from 022.5)
    'wind03': '045deg',  # 45° NE
    'wind04': '068deg',  # 67.5° ENE (rounded from 067.5)
    'wind05': '090deg',  # 90° East
    'wind06': '113deg',  # 112.5° ESE (rounded from 112.5)
    'wind07': '135deg',  # 135° SE
    'wind08': '158deg',  # 157.5° SSE (rounded from 157.5)
    'wind09': '180deg',  # 180° South
    'wind10': '203deg',  # 202.5° SSW (rounded from 202.5)
    'wind11': '225deg',  # 225° SW
    'wind12': '248deg',  # 247.5° WSW (rounded from 247.5)
    'wind13': '270deg',  # 270° West
    'wind14': '293deg',  # 292.5° WNW (rounded from 292.5)
    'wind15': '315deg',  # 315° NW
    'wind16': '338deg'   # 337.5° NNW (rounded from 337.5)
}


def format_heading(degrees):
    """
    Format heading as 3-digit string with 'deg' suffix
    
    Args:
        degrees: Heading in degrees (float or int)
        
    Returns:
        String in format '000deg' with leading zeros
    """
    # Round to nearest integer for fractional degrees
    deg_int = round(degrees)
    return f"{deg_int:03d}deg"


def add_heading_to_filename(input_filename, output_type='rainflow'):
    """
    Add heading information to output filename
    
    Args:
        input_filename: Original filename (e.g., 'fsts_l015_mwl_wave01_Strut1.csv')
        output_type: Type of output ('rainflow', 'damage', 'effective_tension', etc.)
        
    Returns:
        Output filename with heading (e.g., 'fsts_l015_mwl_wave01_045deg_Strut1_rainflow.csv')
    
    Examples:
        >>> add_heading_to_filename('fsts_l015_mwl_wave01_Strut1.csv', 'rainflow')
        'fsts_l015_mwl_wave01_045deg_Strut1_rainflow.csv'
        
        >>> add_heading_to_filename('fsts_l095_125km3_l000_pb_mwl_wind09_Strut5.csv', 'damage')
        'fsts_l095_125km3_l000_pb_mwl_wind09_180deg_Strut5_damage.csv'
    """
    import re
    
    # Remove .csv extension if present
    base = input_filename.replace('.csv', '')
    
    # Find the case type (wave## or wind##)
    pattern = r'(wave\d{2}|wind\d{2})'
    match = re.search(pattern, base)
    
    if match:
        case = match.group(1)
        if case in HEADING_MAP:
            heading = HEADING_MAP[case]
            # Insert heading after the case
            base = base.replace(case, f"{case}_{heading}")
        else:
            print(f"Warning: No heading mapping for {case}")
    
    # Add output type
    return f"{base}_{output_type}.csv"


def get_heading_from_case(case_name):
    """
    Get heading for a given case name
    
    Args:
        case_name: Case identifier (e.g., 'wave01', 'wind16')
        
    Returns:
        Heading string (e.g., '045deg', '338deg')
    """
    return HEADING_MAP.get(case_name, None)


def extract_heading_from_filename(filename):
    """
    Extract heading from a filename that already contains it
    
    Args:
        filename: Filename with heading (e.g., 'fsts_l015_mwl_wave01_045deg_Strut1_rainflow.csv')
        
    Returns:
        Heading string (e.g., '045deg') or None if not found
    """
    import re
    pattern = r'(\d{3}deg)'
    match = re.search(pattern, filename)
    return match.group(1) if match else None


# Example usage and testing
if __name__ == "__main__":
    # Test cases
    test_files = [
        'fsts_l015_mwl_wave01_Strut1.csv',
        'fsts_l095_mwl_wind09_Strut5.csv',
        'fsts_l015_125km3_l100_pb_mwl_wave16_Strut8.csv',
        'fsts_l095_125km3_l000_pb_mwl_wind01_Strut1.csv'
    ]
    
    print("Heading Mapping Examples:\n")
    for test_file in test_files:
        rainflow = add_heading_to_filename(test_file, 'rainflow')
        damage = add_heading_to_filename(test_file, 'damage')
        print(f"Input:    {test_file}")
        print(f"Rainflow: {rainflow}")
        print(f"Damage:   {damage}")
        print()
    
    print("\nHeading Extraction Test:")
    test_output = 'fsts_l015_mwl_wave01_045deg_Strut1_rainflow.csv'
    heading = extract_heading_from_filename(test_output)
    print(f"File: {test_output}")
    print(f"Extracted heading: {heading}")