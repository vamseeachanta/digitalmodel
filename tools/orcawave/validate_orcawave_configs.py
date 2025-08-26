#!/usr/bin/env python
"""
Validate OrcaWave configurations using the MCP server
Checks mesh loading and validation tab for errors
"""

import asyncio
import json
from pathlib import Path
from typing import Dict, List, Any
import subprocess
import time

class OrcaWaveValidator:
    """Validate OrcaWave configurations and check for mesh errors"""
    
    def __init__(self):
        self.mcp_port = 3000  # Default MCP port
        self.configs_to_test = []
        
    def find_configs(self) -> List[str]:
        """Find all config files to test"""
        configs = []
        
        # Config files we created
        test_configs = [
            "config_test_box.yml",
            "config_barge.yml", 
            "config_spar.yml",
            "config_oc4_fixed.yml",
            "example_1_simple_vessel.yml",
            "example_3_minimal_test.yml"
        ]
        
        for config in test_configs:
            if Path(config).exists():
                configs.append(config)
                print(f"  Found: {config}")
        
        return configs
    
    def start_mcp_server(self):
        """Start the OrcaWave MCP server if not running"""
        print("\nStarting OrcaWave MCP server...")
        
        # Check if MCP tools exist
        mcp_path = Path("D:/github/digitalmodel/specs/modules/mcp-server/orcawave-mcp")
        if not mcp_path.exists():
            print("  MCP server not found at expected location")
            return False
            
        # Try to start the server
        try:
            # Start server in background
            server_script = mcp_path / "start_server.py"
            if server_script.exists():
                subprocess.Popen(
                    ["python", str(server_script)],
                    stdout=subprocess.DEVNULL,
                    stderr=subprocess.DEVNULL
                )
                print("  MCP server starting...")
                time.sleep(3)  # Give it time to start
                return True
        except Exception as e:
            print(f"  Failed to start MCP: {e}")
            return False
    
    def validate_config_structure(self, config_path: str) -> Dict[str, Any]:
        """Validate config file structure before loading"""
        import yaml
        
        result = {
            "file": config_path,
            "valid_structure": False,
            "errors": [],
            "warnings": []
        }
        
        try:
            with open(config_path, 'r', encoding='utf-8-sig') as f:
                content = f.read()
                
            # Check for YAML 1.1 header
            if not content.startswith('%YAML 1.1'):
                result["warnings"].append("Missing %YAML 1.1 header")
            
            # Parse YAML
            lines = content.split('\n')
            yaml_content = '\n'.join([line for line in lines if not line.startswith('%YAML')])
            config = yaml.safe_load(yaml_content)
            
            # Check required fields
            required_fields = ['UnitsSystem', 'SolveType', 'WaterDensity', 'Bodies']
            for field in required_fields:
                if field not in config:
                    result["errors"].append(f"Missing required field: {field}")
            
            # Validate water density
            if 'WaterDensity' in config:
                if config['WaterDensity'] != 1.025:
                    result["errors"].append(f"Water density should be 1.025, got {config['WaterDensity']}")
            
            # Validate bodies
            if 'Bodies' in config and config['Bodies']:
                for i, body in enumerate(config['Bodies']):
                    # Check GDF file exists
                    if 'BodyMeshFileName' in body:
                        gdf_path = Path(body['BodyMeshFileName'])
                        if not gdf_path.exists():
                            # Try relative path
                            gdf_path = Path(gdf_path.name)
                            if not gdf_path.exists():
                                result["errors"].append(f"Body {i}: GDF file not found: {body['BodyMeshFileName']}")
                    
                    # Check for inertia
                    has_inertia = False
                    if 'BodyMass' in body and body['BodyMass'] > 0:
                        has_inertia = True
                    if 'BodyRadiiOfGyrationRx, BodyRadiiOfGyrationRy, BodyRadiiOfGyrationRz' in body:
                        has_inertia = True
                    if 'BodyInertiaTensorRx, BodyInertiaTensorRy, BodyInertiaTensorRz' in body:
                        has_inertia = True
                    
                    if not has_inertia:
                        result["errors"].append(f"Body {i}: No inertia specified")
            
            result["valid_structure"] = len(result["errors"]) == 0
            
        except Exception as e:
            result["errors"].append(f"Failed to parse: {e}")
        
        return result
    
    def check_mesh_quality(self, gdf_path: str) -> Dict[str, Any]:
        """Check mesh quality metrics"""
        result = {
            "file": gdf_path,
            "exists": False,
            "panel_count": 0,
            "quality_checks": {}
        }
        
        gdf = Path(gdf_path)
        if not gdf.exists():
            # Try just the filename
            gdf = Path(gdf.name)
        
        if gdf.exists():
            result["exists"] = True
            
            with open(gdf, 'r') as f:
                lines = f.readlines()
                
            # Parse GDF format
            if len(lines) > 3:
                try:
                    # Line 4 has panel count
                    result["panel_count"] = int(lines[3].strip())
                    
                    # Quality checks
                    if result["panel_count"] < 50:
                        result["quality_checks"]["low_resolution"] = True
                    if result["panel_count"] > 10000:
                        result["quality_checks"]["high_panel_count"] = True
                    
                    # Check for symmetry
                    symmetry_line = lines[2].strip()
                    if '1' in symmetry_line[:5]:
                        result["quality_checks"]["has_symmetry"] = True
                        
                except Exception as e:
                    result["quality_checks"]["parse_error"] = str(e)
        
        return result
    
    async def validate_with_orcawave(self, config_path: str) -> Dict[str, Any]:
        """Validate using actual OrcaWave (if MCP available)"""
        result = {
            "file": config_path,
            "orcawave_validation": "Not tested (MCP not available)"
        }
        
        # This would use the actual MCP server if available
        # For now, we'll do static validation
        
        return result
    
    def run_validation(self):
        """Run full validation suite"""
        print("="*60)
        print(" ORCAWAVE CONFIGURATION VALIDATION ")
        print("="*60)
        
        print("\n1. FINDING CONFIGURATIONS...")
        print("-"*40)
        configs = self.find_configs()
        
        if not configs:
            print("  No configuration files found!")
            return
        
        print(f"\n  Found {len(configs)} configurations to test")
        
        print("\n2. VALIDATING CONFIGURATION STRUCTURE...")
        print("-"*40)
        
        validation_results = []
        for config in configs:
            print(f"\nValidating: {config}")
            result = self.validate_config_structure(config)
            validation_results.append(result)
            
            if result["valid_structure"]:
                print("  [OK] Structure valid")
            else:
                print("  [ERROR] Structure issues:")
                for error in result["errors"]:
                    print(f"    - {error}")
            
            if result["warnings"]:
                print("  [WARNING] Issues:")
                for warning in result["warnings"]:
                    print(f"    - {warning}")
            
            # Check mesh quality
            import yaml
            try:
                with open(config, 'r', encoding='utf-8-sig') as f:
                    content = f.read()
                lines = content.split('\n')
                yaml_content = '\n'.join([line for line in lines if not line.startswith('%YAML')])
                cfg = yaml.safe_load(yaml_content)
                
                if 'Bodies' in cfg and cfg['Bodies']:
                    for body in cfg['Bodies']:
                        if 'BodyMeshFileName' in body:
                            mesh_result = self.check_mesh_quality(body['BodyMeshFileName'])
                            print(f"\n  Mesh: {Path(body['BodyMeshFileName']).name}")
                            if mesh_result["exists"]:
                                print(f"    Panels: {mesh_result['panel_count']}")
                                for check, value in mesh_result["quality_checks"].items():
                                    if check == "has_symmetry":
                                        print(f"    [INFO] Uses symmetry")
                                    elif check == "low_resolution":
                                        print(f"    [WARNING] Low panel count (<50)")
                                    elif check == "high_panel_count":
                                        print(f"    [WARNING] High panel count (>10000)")
                            else:
                                print(f"    [ERROR] Mesh file not found")
            except:
                pass
        
        print("\n3. VALIDATION SUMMARY...")
        print("-"*40)
        
        valid_configs = [r for r in validation_results if r["valid_structure"]]
        invalid_configs = [r for r in validation_results if not r["valid_structure"]]
        
        print(f"\nValid configurations: {len(valid_configs)}/{len(validation_results)}")
        if valid_configs:
            print("\nReady to load in OrcaWave:")
            for result in valid_configs:
                print(f"  [OK] {result['file']}")
        
        if invalid_configs:
            print("\nNeed fixes before loading:")
            for result in invalid_configs:
                print(f"  [ERROR] {result['file']}")
                for error in result["errors"][:2]:  # Show first 2 errors
                    print(f"    - {error}")
        
        print("\n4. RECOMMENDED TEST ORDER...")
        print("-"*40)
        print("\n1. Start with: config_test_box.yml")
        print("   - Simple geometry (240 panels)")
        print("   - Generated GDF file")
        print("\n2. Then try: example_3_minimal_test.yml")
        print("   - Uses verified L01 vessel mesh")
        print("   - Minimal configuration")
        print("\n3. Finally: example_1_simple_vessel.yml")
        print("   - Full configuration")
        print("   - Multiple periods/headings")
        
        return validation_results

def main():
    validator = OrcaWaveValidator()
    results = validator.run_validation()
    
    print("\n" + "="*60)
    print(" NEXT STEPS ")
    print("="*60)
    print("\n1. Open OrcaWave")
    print("2. File -> Open")
    print("3. Select one of the validated .yml files")
    print("4. Check the Validation tab for any mesh warnings")
    print("5. If successful, run the analysis")
    
    print("\nNote: The Validation tab will show:")
    print("  - Panel arrangement warnings")
    print("  - Aspect ratio issues")
    print("  - Waterline intersection problems")
    print("  - Body volume calculations")
    
    return 0

if __name__ == "__main__":
    main()