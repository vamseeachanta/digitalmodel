"""
CSV Parser Module for Mooring Tension Iteration System
Handles parsing of target tension and fender property CSV files
"""

import csv
import json
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Union
from dataclasses import dataclass, field


@dataclass
class MooringLineTarget:
    """Data class for mooring line target specifications"""
    name: str
    target_tension: Optional[float] = None  # kN
    target_length: Optional[float] = None   # m
    line_lengths: List[float] = field(default_factory=list)  # m
    line_ea: List[float] = field(default_factory=list)  # kN
    section_to_modify: int = 1  # Which section to adjust
    
    def has_tension_target(self) -> bool:
        """Check if this line has a tension target"""
        return self.target_tension is not None and self.target_tension > 0
    
    def has_length_target(self) -> bool:
        """Check if this line has a length target"""
        return self.target_length is not None and self.target_length > 0
    
    def get_governing_ea(self) -> float:
        """Get the major governing stiffness"""
        if not self.line_ea:
            raise ValueError(f"No EA values defined for line {self.name}")
        # Return the EA of the section to be modified, or minimum EA if not specified
        if self.section_to_modify <= len(self.line_ea):
            return self.line_ea[self.section_to_modify - 1]
        return min(self.line_ea)  # Conservative approach


@dataclass
class FenderProperties:
    """Data class for fender force-deflection properties"""
    name: str
    target_force: Optional[float] = None  # kN
    force_deflection_curve: List[Tuple[float, float]] = field(default_factory=list)
    
    def get_force_at_deflection(self, deflection: float) -> float:
        """Interpolate force at given deflection"""
        if not self.force_deflection_curve:
            return 0.0
        
        # Find surrounding points for interpolation
        for i in range(len(self.force_deflection_curve) - 1):
            d1, f1 = self.force_deflection_curve[i]
            d2, f2 = self.force_deflection_curve[i + 1]
            
            if d1 <= deflection <= d2:
                # Linear interpolation
                ratio = (deflection - d1) / (d2 - d1) if d2 != d1 else 0
                return f1 + ratio * (f2 - f1)
        
        # Outside range - return boundary values
        if deflection < self.force_deflection_curve[0][0]:
            return self.force_deflection_curve[0][1]
        return self.force_deflection_curve[-1][1]


class CSVParser:
    """Parser for mooring tension and fender property CSV files"""
    
    def __init__(self, file_path: Union[str, Path]):
        """Initialize parser with file path"""
        self.file_path = Path(file_path)
        if not self.file_path.exists():
            raise FileNotFoundError(f"CSV file not found: {self.file_path}")
    
    def parse_mooring_targets(self) -> Dict[str, MooringLineTarget]:
        """Parse mooring line target tensions/lengths from CSV"""
        targets = {}
        
        with open(self.file_path, 'r') as f:
            reader = csv.DictReader(f)
            
            for row in reader:
                # Parse line name
                line_name = row.get('ObjectName', '').strip()
                if not line_name:
                    continue
                
                # Create target object
                target = MooringLineTarget(name=line_name)
                
                # Parse target tension (can be number or NULL)
                tension_str = row.get('target_tension', '').strip()
                if tension_str and tension_str.upper() != 'NULL':
                    try:
                        target.target_tension = float(tension_str)
                    except ValueError:
                        print(f"Warning: Invalid tension value for {line_name}: {tension_str}")
                
                # Parse line lengths (JSON array or comma-separated)
                length_str = row.get('line_length', '').strip()
                if length_str:
                    try:
                        # Replace None with null for proper JSON parsing
                        length_str_json = length_str.replace('None', 'null')
                        lengths = json.loads(length_str_json)
                        if isinstance(lengths, list):
                            # Filter out None/null values
                            target.line_lengths = [l for l in lengths if l is not None]
                            # Check if first element is target length
                            if lengths and isinstance(lengths[0], (int, float)):
                                target.target_length = lengths[0]
                    except (json.JSONDecodeError, ValueError) as e:
                        print(f"Warning: Could not parse line length for {line_name}: {length_str}")
                
                # Parse EA values (JSON array)
                ea_str = row.get('line_EA', '').strip()
                if ea_str:
                    try:
                        # Handle scientific notation (e.g., 1.29e3)
                        ea_str_json = ea_str.replace('e', 'e')  # Keep as is
                        ea_values = json.loads(ea_str_json)
                        if isinstance(ea_values, list):
                            target.line_ea = [float(e) for e in ea_values if e is not None]
                    except (json.JSONDecodeError, ValueError) as e:
                        print(f"Warning: Invalid EA format for {line_name}: {ea_str}")
                
                # Parse section to modify
                section_str = row.get('section_to_be_modified', '').strip()
                if section_str:
                    try:
                        target.section_to_modify = int(section_str)
                    except ValueError:
                        print(f"Warning: Invalid section number for {line_name}: {section_str}")
                
                targets[line_name] = target
        
        return targets
    
    def parse_fender_properties(self) -> Dict[str, FenderProperties]:
        """Parse fender force-deflection properties from CSV"""
        fenders = {}
        
        with open(self.file_path, 'r') as f:
            reader = csv.DictReader(f)
            
            for row in reader:
                # Parse fender name
                fender_name = row.get('ObjectName', '').strip()
                if not fender_name:
                    continue
                
                # Create fender object
                fender = FenderProperties(name=fender_name)
                
                # Parse target force
                force_str = row.get('target_force', '').strip()
                if force_str and force_str.upper() != 'NULL':
                    try:
                        fender.target_force = float(force_str)
                    except ValueError:
                        pass
                
                # Parse force-deflection curve (JSON array of pairs)
                curve_str = row.get('fender_properties', '').strip()
                if curve_str:
                    try:
                        curve_data = json.loads(curve_str)
                        if isinstance(curve_data, list):
                            fender.force_deflection_curve = [
                                (float(pair[0]), float(pair[1])) 
                                for pair in curve_data 
                                if isinstance(pair, list) and len(pair) == 2
                            ]
                    except (json.JSONDecodeError, ValueError) as e:
                        print(f"Warning: Invalid curve format for {fender_name}: {e}")
                
                fenders[fender_name] = fender
        
        return fenders
    
    def validate_targets(self, targets: Dict[str, MooringLineTarget]) -> List[str]:
        """Validate parsed targets and return list of warnings"""
        warnings = []
        
        for name, target in targets.items():
            # Check if either tension or length target is specified
            if not target.has_tension_target() and not target.has_length_target():
                warnings.append(f"{name}: No target tension or length specified")
            
            # Check if both are specified (potential conflict)
            if target.has_tension_target() and target.has_length_target():
                warnings.append(f"{name}: Both tension and length targets specified - tension will be used")
            
            # Check EA values
            if not target.line_ea:
                warnings.append(f"{name}: No EA values specified - will need to extract from model")
            
            # Check section to modify is valid
            if target.line_ea and target.section_to_modify > len(target.line_ea):
                warnings.append(f"{name}: Section {target.section_to_modify} exceeds available sections")
        
        return warnings


def main():
    """Test the CSV parser with sample data"""
    # Test with sample file
    sample_file = Path(__file__).parent / "data" / "sample_target_tensions.csv"
    
    if sample_file.exists():
        print(f"Parsing: {sample_file}")
        parser = CSVParser(sample_file)
        
        # Parse mooring targets
        targets = parser.parse_mooring_targets()
        print(f"\nFound {len(targets)} mooring lines:")
        
        for name, target in targets.items():
            print(f"\n{name}:")
            if target.has_tension_target():
                print(f"  Target tension: {target.target_tension} kN")
            if target.has_length_target():
                print(f"  Target length: {target.target_length} m")
            if target.line_ea:
                print(f"  EA values: {target.line_ea} kN")
                print(f"  Governing EA: {target.get_governing_ea()} kN")
            print(f"  Section to modify: {target.section_to_modify}")
        
        # Validate
        warnings = parser.validate_targets(targets)
        if warnings:
            print("\nValidation warnings:")
            for warning in warnings:
                print(f"  - {warning}")
    else:
        print(f"Sample file not found: {sample_file}")


if __name__ == "__main__":
    main()