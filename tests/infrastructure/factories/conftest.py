"""
Pytest fixtures for test data factories.

Provides centralized access to test data factories and realistic
test datasets for use across all test modules.
"""
import pytest
import tempfile
import shutil
from pathlib import Path
from typing import Dict, List, Any

from .simulation_factories import (
    TestDataManager,
    SimulationResultFactory,
    MaterialTestDataFactory,
    LoadCaseFactory,
    AnalysisJobFactory,
    create_test_simulation,
    create_test_material,
    create_test_load_case,
    SimulationStatus,
    SimulationType
)


@pytest.fixture
def test_data_manager():
    """Fixture providing test data manager."""
    return TestDataManager()


@pytest.fixture
def simulation_factory():
    """Fixture providing simulation factory."""
    return SimulationResultFactory


@pytest.fixture
def material_factory():
    """Fixture providing material factory."""
    return MaterialTestDataFactory


@pytest.fixture
def load_case_factory():
    """Fixture providing load case factory."""
    return LoadCaseFactory


@pytest.fixture
def analysis_job_factory():
    """Fixture providing analysis job factory."""
    return AnalysisJobFactory


@pytest.fixture
def sample_completed_simulation():
    """Fixture providing a completed simulation with output data."""
    return create_test_simulation(status=SimulationStatus.COMPLETED)


@pytest.fixture
def sample_failed_simulation():
    """Fixture providing a failed simulation."""
    return create_test_simulation(status=SimulationStatus.FAILED)


@pytest.fixture
def sample_running_simulation():
    """Fixture providing a running simulation."""
    return create_test_simulation(status=SimulationStatus.RUNNING)


@pytest.fixture
def sample_material_tensile():
    """Fixture providing tensile test material data."""
    return create_test_material(test_type='tensile')


@pytest.fixture
def sample_material_fatigue():
    """Fixture providing fatigue test material data."""
    return create_test_material(test_type='fatigue')


@pytest.fixture
def sample_load_cases(load_case_factory):
    """Fixture providing a set of sample load cases."""
    return [load_case_factory() for _ in range(5)]


@pytest.fixture
def small_simulation_dataset(test_data_manager):
    """Fixture providing small dataset for quick tests."""
    return test_data_manager.generate_simulation_dataset(n_simulations=3)


@pytest.fixture
def medium_simulation_dataset(test_data_manager):
    """Fixture providing medium dataset for integration tests."""
    return test_data_manager.generate_simulation_dataset(n_simulations=10)


@pytest.fixture
def large_simulation_dataset(test_data_manager):
    """Fixture providing large dataset for performance tests."""
    return test_data_manager.generate_simulation_dataset(n_simulations=100)


@pytest.fixture
def realistic_project(test_data_manager):
    """Fixture providing realistic engineering project."""
    return test_data_manager.create_realistic_project()


@pytest.fixture
def temp_data_directory():
    """Fixture providing temporary directory for test data files."""
    temp_dir = tempfile.mkdtemp()
    yield temp_dir
    shutil.rmtree(temp_dir)


@pytest.fixture
def exported_test_dataset(test_data_manager, temp_data_directory):
    """Fixture providing exported test dataset files."""
    project = test_data_manager.create_realistic_project()
    test_data_manager.export_dataset_to_files(project, temp_data_directory)
    return {
        "project": project,
        "directory": temp_data_directory,
        "files": list(Path(temp_data_directory).glob("**/*"))
    }


@pytest.fixture(scope="session")
def stress_test_dataset():
    """Fixture providing large dataset for stress testing (session scope)."""
    manager = TestDataManager()
    return {
        "simulations": manager.generate_simulation_dataset(n_simulations=1000),
        "materials": manager.generate_material_dataset(n_materials=50),
        "load_cases": manager.generate_load_cases(n_cases=200)
    }


@pytest.fixture(params=[
    SimulationType.STATIC,
    SimulationType.DYNAMIC,
    SimulationType.FATIGUE,
    SimulationType.FREQUENCY
])
def simulation_by_type(request, simulation_factory):
    """Parametrized fixture providing simulations of different types."""
    # Note: This would need to be adapted based on actual simulation factory
    # that supports simulation type parameter
    return simulation_factory(status=SimulationStatus.COMPLETED)


@pytest.fixture(params=[
    'tensile', 'compression', 'fatigue', 'impact'
])
def material_by_test_type(request, material_factory):
    """Parametrized fixture providing materials with different test types."""
    return material_factory(test_type=request.param)


@pytest.fixture
def edge_case_simulations():
    """Fixture providing edge case simulations for robustness testing."""
    return [
        # Very short simulation
        create_test_simulation(
            status=SimulationStatus.COMPLETED,
            duration=0.001,
            time_step=0.0001
        ),
        # Very long simulation
        create_test_simulation(
            status=SimulationStatus.COMPLETED,
            duration=86400.0,  # 24 hours
            time_step=1.0
        ),
        # High precision simulation
        create_test_simulation(
            status=SimulationStatus.COMPLETED,
            convergence_criteria=1e-12
        ),
        # Low precision simulation
        create_test_simulation(
            status=SimulationStatus.COMPLETED,
            convergence_criteria=1e-2
        )
    ]


@pytest.fixture
def corrupted_data_scenarios():
    """Fixture providing scenarios with corrupted or invalid data."""
    import numpy as np
    import pandas as pd

    scenarios = []

    # Simulation with NaN values
    sim_with_nan = create_test_simulation(status=SimulationStatus.COMPLETED)
    if sim_with_nan.output_data is not None:
        sim_with_nan.output_data.loc[10:20, 'response'] = np.nan
    scenarios.append(("nan_values", sim_with_nan))

    # Simulation with infinite values
    sim_with_inf = create_test_simulation(status=SimulationStatus.COMPLETED)
    if sim_with_inf.output_data is not None:
        sim_with_inf.output_data.loc[5:15, 'response'] = np.inf
    scenarios.append(("inf_values", sim_with_inf))

    # Simulation with extremely large values
    sim_with_large = create_test_simulation(status=SimulationStatus.COMPLETED)
    if sim_with_large.output_data is not None:
        sim_with_large.output_data.loc[0:10, 'response'] = 1e50
    scenarios.append(("large_values", sim_with_large))

    # Empty simulation data
    sim_empty = create_test_simulation(status=SimulationStatus.COMPLETED)
    sim_empty.output_data = pd.DataFrame()
    scenarios.append(("empty_data", sim_empty))

    return scenarios


@pytest.fixture
def performance_test_data():
    """Fixture providing data specifically for performance testing."""
    return {
        "small": TestDataManager().generate_simulation_dataset(n_simulations=10),
        "medium": TestDataManager().generate_simulation_dataset(n_simulations=100),
        "large": TestDataManager().generate_simulation_dataset(n_simulations=1000),
        "xlarge": TestDataManager().generate_simulation_dataset(n_simulations=5000)
    }


@pytest.fixture
def concurrent_test_data():
    """Fixture providing data for concurrent access testing."""
    manager = TestDataManager()

    # Generate multiple independent datasets
    datasets = []
    for i in range(5):
        dataset = {
            "id": f"dataset_{i}",
            "simulations": manager.generate_simulation_dataset(n_simulations=20),
            "materials": manager.generate_material_dataset(n_materials=5)
        }
        datasets.append(dataset)

    return datasets


@pytest.fixture
def memory_test_simulations():
    """Fixture providing simulations with large memory footprints."""
    import numpy as np
    import pandas as pd

    simulations = []

    for size in ['small', 'medium', 'large']:
        if size == 'small':
            n_points = 1000
        elif size == 'medium':
            n_points = 10000
        else:  # large
            n_points = 100000

        # Create simulation with large output data
        sim = create_test_simulation(status=SimulationStatus.COMPLETED)

        # Generate large time series data
        time = np.linspace(0, 100, n_points)
        data = {
            'time': time,
            'response': np.random.randn(n_points),
            'velocity': np.random.randn(n_points),
            'acceleration': np.random.randn(n_points)
        }

        # Add multiple channels for larger memory footprint
        for i in range(10):
            data[f'channel_{i}'] = np.random.randn(n_points)

        sim.output_data = pd.DataFrame(data)
        simulations.append((size, sim))

    return simulations


# Parametrized fixtures for cross-testing different data combinations
@pytest.fixture(params=[
    {"n_simulations": 5, "status": SimulationStatus.COMPLETED},
    {"n_simulations": 3, "status": SimulationStatus.FAILED},
    {"n_simulations": 8, "status": SimulationStatus.RUNNING}
])
def simulation_dataset_by_status(request, test_data_manager):
    """Parametrized fixture for datasets with different status distributions."""
    params = request.param
    return test_data_manager.generate_simulation_dataset(**params)


@pytest.fixture(params=[5, 10, 25, 50])
def varying_dataset_sizes(request, test_data_manager):
    """Parametrized fixture for datasets of different sizes."""
    return test_data_manager.generate_simulation_dataset(n_simulations=request.param)


# Mock external service fixtures
@pytest.fixture
def mock_file_system(tmp_path):
    """Fixture providing mock file system structure."""
    # Create realistic directory structure
    (tmp_path / "projects").mkdir()
    (tmp_path / "projects" / "project_001").mkdir()
    (tmp_path / "projects" / "project_001" / "simulations").mkdir()
    (tmp_path / "projects" / "project_001" / "materials").mkdir()
    (tmp_path / "projects" / "project_001" / "results").mkdir()

    # Create some mock files
    (tmp_path / "projects" / "project_001" / "project.json").write_text('{"name": "Test Project"}')
    (tmp_path / "projects" / "project_001" / "simulations" / "sim_001.dat").write_text("simulation data")
    (tmp_path / "projects" / "project_001" / "materials" / "steel.mat").write_text("material properties")

    return tmp_path


@pytest.fixture
def mock_database_data():
    """Fixture providing mock database data structure."""
    return {
        "users": [
            {"id": 1, "username": "engineer1", "role": "engineer"},
            {"id": 2, "username": "analyst1", "role": "analyst"},
            {"id": 3, "username": "admin1", "role": "admin"}
        ],
        "projects": [
            {"id": 1, "name": "Bridge Analysis", "owner_id": 1},
            {"id": 2, "name": "Building Design", "owner_id": 2}
        ],
        "simulations": [
            {"id": 1, "project_id": 1, "status": "completed", "type": "static"},
            {"id": 2, "project_id": 1, "status": "running", "type": "dynamic"},
            {"id": 3, "project_id": 2, "status": "failed", "type": "fatigue"}
        ]
    }