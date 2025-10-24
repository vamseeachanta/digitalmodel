# ABOUTME: Integration tests for complete data procurement workflow
# ABOUTME: Tests metocean + vessel + mooring integration with OrcaFlex export

"""
Complete Workflow Integration Tests
====================================

Integration tests for the complete data procurement framework.

Tests:
1. Metocean + Vessel + Mooring integration
2. OrcaFlex model generation
3. Cross-module data flow
4. Zero storage verification

Critical: NO MOCKS - uses real component databases and calculations.
"""

import pytest
from datetime import datetime, timedelta
from pathlib import Path
import shutil
import numpy as np

from digitalmodel.data_procurement import MetoceanClient, VesselClient, MooringClient


class TestCompleteWorkflow:
    """Integration tests for complete workflow."""

    @pytest.fixture
    def metocean_client(self):
        """Create MetoceanClient for testing."""
        config_path = Path(__file__).parent.parent.parent / \
                     'specs/modules/data-procurement/metocean-data/configs/example_config.yml'

        return MetoceanClient.from_config(str(config_path))

    @pytest.fixture
    def vessel_client(self):
        """Create VesselClient for testing."""
        config_path = Path(__file__).parent.parent.parent / \
                     'specs/modules/data-procurement/vessel-systems/configs/example_config.yml'

        return VesselClient.from_config(str(config_path))

    @pytest.fixture
    def mooring_client(self):
        """Create MooringClient for testing."""
        return MooringClient()

    def test_fpso_mooring_integration(self, metocean_client, vessel_client, mooring_client):
        """
        Test complete FPSO mooring design workflow.

        Tests:
        - Metocean data query
        - Vessel RAOs and hydrostatics
        - Mooring line design
        - OrcaFlex export
        """
        # Project parameters
        location = {'latitude': 25.0, 'longitude': -90.0}
        water_depth = 1500  # m
        design_load = 5000  # kN

        # Step 1: Query metocean data
        start_date = datetime(2020, 1, 1)
        end_date = datetime(2020, 1, 7)  # 1 week for test

        metocean_records = []
        for record in metocean_client.query_metocean(
            start_date=start_date,
            end_date=end_date,
            location=location,
            parameters=['significant_wave_height']
        ):
            metocean_records.append(record)
            if len(metocean_records) >= 10:  # Limit for test
                break

        assert len(metocean_records) > 0, "Should retrieve metocean data"

        # Verify metocean data structure
        for record in metocean_records:
            assert 'timestamp' in record
            assert 'location' in record

        # Step 2: Get vessel with RAOs
        vessel_type = 'VLCC'
        draft = 20.0

        from digitalmodel.data_procurement.vessel.api_clients import GenericRAOClient
        rao_client = GenericRAOClient()
        vessel_particulars = rao_client.get_vessel_particulars(vessel_type)

        vessel = {
            'name': 'Generic VLCC',
            'length': vessel_particulars['loa'],
            'beam': vessel_particulars['beam'],
            'depth': vessel_particulars['depth'],
            'draft': draft,
            'dwt': vessel_particulars['dwt']
        }

        # Get RAOs
        raos = vessel_client.get_vessel_raos(vessel_type=vessel_type, draft=draft)

        assert 'raos' in raos
        assert len(raos['raos']) == 6  # 6 DOF

        # Calculate hydrostatics
        hydro = vessel_client.calculate_hydrostatics(vessel, draft=draft)

        assert 'displacement' in hydro
        assert 'gm_metacentric_height' in hydro
        assert hydro['gm_metacentric_height'] > 0  # Positive GM (stable)

        # Step 3: Design mooring line
        mooring_line = mooring_client.design_mooring_line(
            design_load=design_load,
            water_depth=water_depth,
            mooring_type='catenary',
            soil_type='clay'
        )

        assert mooring_line['mooring_type'] == 'catenary'
        assert len(mooring_line['segments']) > 0
        assert 'anchor' in mooring_line
        assert mooring_line['validation']['valid'] == True

        # Step 4: Generate OrcaFlex YAML
        vessel_yaml = vessel_client.to_orcaflex_vessel(vessel, raos, hydro, draft=draft)
        mooring_yaml = mooring_client.to_orcaflex_mooring(mooring_line)

        assert len(vessel_yaml) > 0
        assert len(mooring_yaml) > 0
        assert 'Vessel' in vessel_yaml or 'LineType' in mooring_yaml

    def test_taut_leg_mooring_integration(self, mooring_client):
        """
        Test taut-leg mooring design with synthetic rope.

        Tests:
        - Taut-leg specific design
        - HMPE rope selection
        - Nonlinear stiffness
        """
        design_load = 8000  # kN
        water_depth = 2000  # m (deepwater)

        # Design taut-leg mooring
        mooring_line = mooring_client.design_mooring_line(
            design_load=design_load,
            water_depth=water_depth,
            mooring_type='taut_leg',
            soil_type='sand'
        )

        assert mooring_line['mooring_type'] == 'taut_leg'

        # Verify synthetic rope is used
        has_synthetic = any(seg['type'] == 'synthetic_rope' for seg in mooring_line['segments'])
        assert has_synthetic, "Taut-leg should use synthetic rope"

        # Get synthetic rope segment
        synthetic_seg = [seg for seg in mooring_line['segments'] if seg['type'] == 'synthetic_rope'][0]
        rope = synthetic_seg['component']

        # For deepwater, should use HMPE
        if water_depth > 1000:
            assert rope['fiber_type'] == 'hmpe', "Deepwater should use HMPE"
            assert rope['floats'] == True, "HMPE should float"

        # Generate OrcaFlex with nonlinear stiffness
        orcaflex_yaml = mooring_client.to_orcaflex_mooring(mooring_line)

        assert 'NonlinearStiffness' in orcaflex_yaml, "Should include nonlinear stiffness"

    def test_zero_storage_verification(self, mooring_client):
        """
        Critical test: Verify zero storage footprint.

        Tests:
        - No data files created
        - No large files anywhere
        - Repository data only
        """
        # Clean any existing data directories
        test_dirs = [Path('./data'), Path('./mooring_data'), Path('./output')]
        for dir_path in test_dirs:
            if dir_path.exists():
                shutil.rmtree(dir_path)

        # Design mooring (should not create files)
        mooring_line = mooring_client.design_mooring_line(
            design_load=5000,
            water_depth=1500,
            mooring_type='catenary'
        )

        # Generate OrcaFlex YAML (in-memory)
        orcaflex_yaml = mooring_client.to_orcaflex_mooring(mooring_line)

        # Verify NO data directories created
        for dir_path in test_dirs:
            assert not dir_path.exists(), f"Should not create {dir_path}"

        # Verify NO large files created anywhere in test directory
        test_root = Path(__file__).parent
        all_files = list(test_root.rglob('*'))

        for file_path in all_files:
            if file_path.is_file():
                size_mb = file_path.stat().st_size / (1024 * 1024)
                assert size_mb < 1, f"No data files > 1MB should be created: {file_path}"

    def test_component_database_integration(self, mooring_client):
        """
        Test integration across all component databases.

        Tests:
        - Chain + wire rope + anchor selection
        - Connector compatibility
        - Standards validation
        """
        design_load = 5000  # kN

        # Get chain
        chain = mooring_client.chain_client.find_by_design_load(design_load, grade='R4S')[0]

        # Get wire rope
        wire_rope = mooring_client.wire_rope_client.find_by_design_load(design_load, construction='6x36 IWRC')[0]

        # Get anchor
        anchor = mooring_client.anchor_client.find_by_holding_capacity(design_load * 2, soil='clay')[0]

        # Get connector for chain
        connectors = mooring_client.connector_client.find_for_chain(chain['grade'], chain['diameter'])

        assert len(connectors) > 0, "Should find compatible connectors"

        connector = connectors[0]

        # Verify connector capacity exceeds chain capacity
        chain_mbl = chain['minimum_breaking_load']
        connector_wll = connector['working_load_limit']

        # Connector WLL should be ~50% of chain MBL (typical design)
        assert connector_wll >= chain_mbl / 2.5, "Connector WLL should be adequate for chain"

    def test_standards_validation(self, mooring_client):
        """
        Test API RP 2SK standards validation.

        Tests:
        - Safety factor compliance
        - Component capacity checks
        - Anchor holding capacity
        """
        design_load = 5000  # kN
        safety_factor = 1.67  # API RP 2SK intact condition

        # Design mooring
        mooring_line = mooring_client.design_mooring_line(
            design_load=design_load,
            water_depth=1500,
            mooring_type='catenary',
            safety_factor=safety_factor
        )

        validation = mooring_line['validation']

        # Should be valid
        assert validation['valid'] == True, "Design should pass API RP 2SK validation"
        assert validation['standard'] == 'API RP 2SK'
        assert len(validation['issues']) == 0, "Should have no validation issues"

        # Verify all components meet capacity requirement
        for segment in mooring_line['segments']:
            component = segment['component']

            if segment['type'] in ['chain', 'wire_rope', 'synthetic_rope']:
                mbl = component['minimum_breaking_load']
                required_mbl = design_load * safety_factor

                assert mbl >= required_mbl, f"Component MBL {mbl} should exceed required {required_mbl}"

    def test_orcaflex_yaml_structure(self, vessel_client, mooring_client):
        """
        Test OrcaFlex YAML structure and completeness.

        Tests:
        - Valid YAML format
        - Required fields present
        - Multi-segment line configuration
        """
        import yaml

        # Create vessel YAML
        vessel_type = 'VLCC'
        draft = 20.0

        from digitalmodel.data_procurement.vessel.api_clients import GenericRAOClient
        rao_client = GenericRAOClient()
        vessel_particulars = rao_client.get_vessel_particulars(vessel_type)

        vessel = {
            'name': 'Test VLCC',
            'length': vessel_particulars['loa'],
            'beam': vessel_particulars['beam'],
            'depth': vessel_particulars['depth'],
            'draft': draft,
            'dwt': vessel_particulars['dwt']
        }

        raos = vessel_client.get_vessel_raos(vessel_type=vessel_type, draft=draft)
        hydro = vessel_client.calculate_hydrostatics(vessel, draft=draft)

        vessel_yaml = vessel_client.to_orcaflex_vessel(vessel, raos, hydro, draft=draft)

        # Parse YAML (should not raise exception)
        vessel_data = yaml.safe_load(vessel_yaml)

        assert 'Vessel' in vessel_data
        assert 'Name' in vessel_data['Vessel']
        assert 'Length' in vessel_data['Vessel']
        assert 'GM' in vessel_data['Vessel']

        # Create mooring YAML
        mooring_line = mooring_client.design_mooring_line(
            design_load=5000,
            water_depth=1500,
            mooring_type='catenary'
        )

        mooring_yaml = mooring_client.to_orcaflex_mooring(mooring_line)

        # Should contain multiple line types (for each segment)
        assert mooring_yaml.count('LineType:') >= len(mooring_line['segments'])

    def test_performance_metrics(self, mooring_client):
        """
        Test performance of component selection.

        Tests:
        - Component search speed
        - Design calculation speed
        """
        import time

        # Test chain selection performance
        start = time.time()

        for _ in range(100):
            chains = mooring_client.chain_client.find_by_design_load(5000, grade='R4S')

        elapsed = time.time() - start

        # Should be fast (< 100ms for 100 searches = <1ms per search)
        assert elapsed < 0.1, f"Chain selection should be fast: {elapsed:.3f}s for 100 searches"

        # Test mooring design performance
        start = time.time()

        mooring_line = mooring_client.design_mooring_line(
            design_load=5000,
            water_depth=1500,
            mooring_type='catenary'
        )

        elapsed = time.time() - start

        # Should complete in < 1 second
        assert elapsed < 1.0, f"Mooring design should be fast: {elapsed:.3f}s"


if __name__ == "__main__":
    # Run tests with pytest
    pytest.main([__file__, "-v", "--tb=short"])
