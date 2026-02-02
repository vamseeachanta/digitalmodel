"""
Automated test suite for reference seastate scaling fatigue analysis.
Converted from manual verification steps to automated tests.
"""

import pytest
import numpy as np
import pandas as pd
from pathlib import Path
import sys
import json

# Add the module path to sys.path
module_path = Path(__file__).parent.parent.parent.parent / "specs" / "modules" / "fatigue-analysis" / "reference-seastate-scaling-fatigue"
sys.path.insert(0, str(module_path))

from production_data_handler import ProductionDataHandler
from load_scaler import LoadScaler


class TestReferenceSeaStateScaling:
    """Test suite for reference seastate scaling verification."""
    
    @pytest.fixture
    def base_path(self):
        """Provide base path to sample data."""
        return module_path / "sample_data"
    
    @pytest.fixture
    def handler(self, base_path):
        """Create ProductionDataHandler instance."""
        return ProductionDataHandler(base_path)
    
    @pytest.fixture
    def scaler(self, handler):
        """Create LoadScaler instance."""
        return LoadScaler(handler)
    
    def test_step1_directory_structure(self, base_path):
        """Test Step 1: Verify directory structure is flat."""
        assert base_path.exists(), f"Directory not found: {base_path}"
        
        # Check for flat structure
        subdirs = [d for d in base_path.iterdir() if d.is_dir()]
        assert len(subdirs) == 0, f"Expected flat structure but found subdirectories: {subdirs}"
        
        # Check for CSV files
        csv_files = list(base_path.glob("*.csv"))
        assert len(csv_files) == 64, f"Expected 64 CSV files, found {len(csv_files)}"
    
    def test_step2_naming_convention(self, base_path):
        """Test Step 2: Verify naming convention."""
        import re
        pattern = r'^(.+)_mwl_(wind01|wave01)_Strut(\d)\.csv$'
        
        csv_files = list(base_path.glob("*.csv"))
        configs_found = set()
        references_found = set()
        struts_found = set()
        
        for file in csv_files:
            match = re.match(pattern, file.name)
            assert match, f"File {file.name} doesn't match expected pattern"
            
            config, reference, strut = match.groups()
            configs_found.add(config)
            references_found.add(reference)
            struts_found.add(strut)
        
        # Verify completeness
        expected_configs = {'fsts_l015', 'fsts_l095', 'fsts_l015_125km3_l100_pb', 'fsts_l095_125km3_l000_pb'}
        expected_refs = {'wind01', 'wave01'}
        expected_struts = {'1', '2', '3', '4', '5', '6', '7', '8'}
        
        assert configs_found == expected_configs, f"Missing configs: {expected_configs - configs_found}"
        assert references_found == expected_refs, f"Missing references: {expected_refs - references_found}"
        assert struts_found == expected_struts, f"Missing struts: {expected_struts - struts_found}"


if __name__ == "__main__":
    # Run tests with pytest
    pytest.main([__file__, "-v", "--tb=short"])
