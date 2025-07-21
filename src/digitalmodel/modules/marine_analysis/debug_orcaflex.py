"""Debug OrcaFlex parsing step by step."""

import yaml
from src.digitalmodel.modules.marine_analysis.orcaflex_reader import OrcaFlexReader

def debug_orcaflex():
    """Debug OrcaFlex parsing step by step."""
    
    file_path = "tests/modules/rao_analysis/SS_Off_0_8P.yml"
    
    # Load YAML manually first
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
            print(f"File loaded successfully, length: {len(content)}")
        
        # Parse with yaml
        data = list(yaml.safe_load_all(content))
        print(f"YAML documents found: {len(data)}")
        
        # Check first document
        if data:
            first_doc = data[0]
            print(f"First document keys: {list(first_doc.keys())}")
            
            if 'VesselTypes' in first_doc:
                vessel_types = first_doc['VesselTypes']
                print(f"VesselTypes found: {len(vessel_types)} vessels")
                
                if vessel_types:
                    first_vessel = vessel_types[0]
                    print(f"First vessel keys: {list(first_vessel.keys())}")
                    
                    if 'Draughts' in first_vessel:
                        draughts = first_vessel['Draughts']
                        print(f"Draughts found: {len(draughts)} draughts")
                        
                        if draughts:
                            first_draught = draughts[0]
                            print(f"First draught keys: {list(first_draught.keys())}")
                            
                            if 'DisplacementRAOs' in first_draught:
                                rao_data = first_draught['DisplacementRAOs']
                                print(f"DisplacementRAOs found: {list(rao_data.keys())}")
                                
                                if 'RAOs' in rao_data:
                                    raos = rao_data['RAOs']
                                    print(f"RAOs found: {len(raos)} directions")
                                    
                                    if raos:
                                        first_rao = raos[0]
                                        print(f"First RAO: {first_rao}")
                                        
                                        # Check the data structure
                                        if 'RAOPeriodOrFreq, RAOSurgeAmp, RAOSurgePhase, RAOSwayAmp, RAOSwayPhase, RAOHeaveAmp, RAOHeavePhase, RAORollAmp, RAORollPhase, RAOPitchAmp, RAOPitchPhase, RAOYawAmp, RAOYawPhase' in first_rao:
                                            data_rows = first_rao['RAOPeriodOrFreq, RAOSurgeAmp, RAOSurgePhase, RAOSwayAmp, RAOSwayPhase, RAOHeaveAmp, RAOHeavePhase, RAORollAmp, RAORollPhase, RAOPitchAmp, RAOPitchPhase, RAOYawAmp, RAOYawPhase']
                                            print(f"Data rows: {len(data_rows)}")
                                            print(f"First row: {data_rows[0]}")
        
        # Now test the parser
        print(f"\nTesting OrcaFlex parser...")
        parser = OrcaFlexReader()
        
        try:
            result = parser.parse_yml_file(file_path)
            print(f"Parser succeeded!")
        except Exception as e:
            print(f"Parser failed: {e}")
            
            # Let's check the internal method
            try:
                with open(file_path, 'r', encoding='utf-8') as f:
                    content = f.read()
                documents = list(yaml.safe_load_all(content))
                combined_data = {}
                for doc in documents:
                    if isinstance(doc, dict):
                        combined_data.update(doc)
                
                print(f"Combined data keys: {list(combined_data.keys())}")
                
                if parser.vessel_types_key in combined_data:
                    vessel_types = combined_data[parser.vessel_types_key]
                    vessel_data = parser._find_vessel_type(vessel_types)
                    if vessel_data:
                        print(f"Found vessel: {vessel_data.get('Name', 'Unknown')}")
                        
                        if parser.draughts_key in vessel_data:
                            draughts = vessel_data[parser.draughts_key]
                            draught_data = parser._find_draught_with_raos(draughts)
                            if draught_data:
                                print(f"Found draught with RAOs")
                                
                                if parser.displacement_raos_key in draught_data:
                                    rao_data_raw = draught_data[parser.displacement_raos_key]
                                    print(f"RAO data keys: {list(rao_data_raw.keys())}")
                                    
                                    try:
                                        frequencies, headings, raos = parser._parse_orcaflex_rao_data(rao_data_raw)
                                        print(f"Parsing succeeded: {len(frequencies)} frequencies, {len(headings)} headings")
                                    except Exception as parse_error:
                                        print(f"RAO parsing failed: {parse_error}")
                                else:
                                    print(f"No {parser.displacement_raos_key} in draught data")
                            else:
                                print("No draught with RAOs found")
                        else:
                            print(f"No {parser.draughts_key} in vessel data")
                    else:
                        print("No vessel data found")
                else:
                    print(f"No {parser.vessel_types_key} in combined data")
                    
            except Exception as debug_error:
                print(f"Debug failed: {debug_error}")
                import traceback
                traceback.print_exc()
    
    except Exception as e:
        print(f"File loading failed: {e}")

if __name__ == "__main__":
    debug_orcaflex()