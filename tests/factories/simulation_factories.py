"""
Factory Boy factories for simulation-related test data.

Provides realistic test data generation for engineering simulations,
analysis results, and related domain objects.
"""
import factory
import faker
import numpy as np
import pandas as pd
from datetime import datetime, timedelta
from typing import Dict, List, Any, Optional
import random
import uuid
from dataclasses import dataclass
from enum import Enum


# Domain-specific enums and data classes
class SimulationType(Enum):
    STATIC = "static"
    DYNAMIC = "dynamic"
    FATIGUE = "fatigue"
    FREQUENCY = "frequency"
    MODAL = "modal"
    BUCKLING = "buckling"


class SimulationStatus(Enum):
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


@dataclass
class SimulationParameters:
    """Parameters for engineering simulation."""
    time_step: float
    duration: float
    solver_type: str
    convergence_criteria: float
    damping_ratio: Optional[float] = None
    material_properties: Optional[Dict[str, float]] = None


@dataclass
class SimulationResult:
    """Result from engineering simulation."""
    simulation_id: str
    status: SimulationStatus
    start_time: datetime
    end_time: Optional[datetime]
    parameters: SimulationParameters
    output_data: Optional[pd.DataFrame] = None
    metadata: Optional[Dict[str, Any]] = None


# Custom Faker providers for engineering data
class EngineeringProvider(faker.providers.BaseProvider):
    """Custom Faker provider for engineering-specific data."""

    def simulation_id(self) -> str:
        """Generate realistic simulation ID."""
        return f"SIM_{uuid.uuid4().hex[:8].upper()}"

    def material_name(self) -> str:
        """Generate realistic material name."""
        materials = [
            "Steel Grade 50", "Aluminum 6061-T6", "Titanium Ti-6Al-4V",
            "Carbon Steel A36", "Stainless Steel 316L", "Inconel 718",
            "Composite CFRP", "Concrete C30", "Rubber EPDM"
        ]
        return self.random.choice(materials)

    def engineering_units(self, quantity_type: str) -> str:
        """Generate appropriate engineering units."""
        unit_map = {
            "force": ["N", "kN", "MN", "lbf", "kip"],
            "pressure": ["Pa", "kPa", "MPa", "GPa", "psi", "bar"],
            "length": ["m", "mm", "cm", "km", "ft", "in"],
            "time": ["s", "ms", "min", "hr"],
            "frequency": ["Hz", "kHz", "MHz", "rpm"],
            "temperature": ["°C", "°F", "K"],
            "velocity": ["m/s", "km/h", "ft/s", "mph"],
            "acceleration": ["m/s²", "g", "ft/s²"]
        }
        return self.random.choice(unit_map.get(quantity_type, ["unit"]))

    def coordinate_system(self) -> str:
        """Generate coordinate system name."""
        systems = ["Global", "Local", "Cylindrical", "Spherical", "User-Defined"]
        return self.random.choice(systems)

    def load_case_name(self) -> str:
        """Generate realistic load case name."""
        load_types = ["Dead Load", "Live Load", "Wind Load", "Seismic Load", "Thermal Load"]
        conditions = ["Operating", "Extreme", "Fatigue", "Test", "Design"]
        return f"{self.random.choice(conditions)} {self.random.choice(load_types)}"


# Register the custom provider
faker.Faker.add_provider(EngineeringProvider)


class SimulationParametersFactory(factory.Factory):
    """Factory for simulation parameters."""

    class Meta:
        model = SimulationParameters

    time_step = factory.LazyFunction(lambda: round(random.uniform(0.001, 1.0), 6))
    duration = factory.LazyFunction(lambda: round(random.uniform(1.0, 3600.0), 2))
    solver_type = factory.Faker('random_element', elements=['euler', 'runge_kutta', 'implicit', 'newmark'])
    convergence_criteria = factory.LazyFunction(lambda: random.uniform(1e-8, 1e-3))
    damping_ratio = factory.LazyFunction(lambda: random.uniform(0.01, 0.10) if random.random() > 0.3 else None)

    @factory.lazy_attribute
    def material_properties(self):
        """Generate realistic material properties."""
        if random.random() > 0.5:
            return None

        return {
            "youngs_modulus": random.uniform(200e9, 210e9),  # Steel range
            "poissons_ratio": random.uniform(0.25, 0.35),
            "density": random.uniform(7800, 7900),  # kg/m³
            "yield_strength": random.uniform(250e6, 400e6),  # Pa
            "ultimate_strength": random.uniform(400e6, 600e6)  # Pa
        }


class SimulationResultFactory(factory.Factory):
    """Factory for simulation results."""

    class Meta:
        model = SimulationResult

    simulation_id = factory.Faker('simulation_id')
    status = factory.Faker('random_element', elements=list(SimulationStatus))
    start_time = factory.Faker('date_time_this_year')
    parameters = factory.SubFactory(SimulationParametersFactory)

    @factory.lazy_attribute
    def end_time(self):
        """Generate end time based on start time and status."""
        if self.status in [SimulationStatus.COMPLETED, SimulationStatus.FAILED, SimulationStatus.CANCELLED]:
            duration_minutes = random.uniform(1, 120)  # 1 to 120 minutes
            return self.start_time + timedelta(minutes=duration_minutes)
        return None

    @factory.lazy_attribute
    def output_data(self):
        """Generate realistic simulation output data."""
        if self.status != SimulationStatus.COMPLETED:
            return None

        # Generate time series data
        n_points = int(self.parameters.duration / self.parameters.time_step)
        n_points = min(n_points, 10000)  # Limit for performance

        time_array = np.linspace(0, self.parameters.duration, n_points)

        # Generate realistic engineering data
        if random.random() > 0.5:
            # Oscillatory response (common in dynamics)
            frequency = random.uniform(0.1, 10.0)  # Hz
            amplitude = random.uniform(1.0, 100.0)
            phase = random.uniform(0, 2 * np.pi)
            damping = random.uniform(0.01, 0.1)

            response = amplitude * np.exp(-damping * time_array) * np.sin(2 * np.pi * frequency * time_array + phase)
            # Add noise
            noise = np.random.normal(0, amplitude * 0.05, len(response))
            response += noise
        else:
            # Monotonic response (common in static analysis)
            final_value = random.uniform(10.0, 1000.0)
            response = final_value * (1 - np.exp(-time_array / (self.parameters.duration / 4)))

        return pd.DataFrame({
            'time': time_array,
            'response': response,
            'velocity': np.gradient(response, time_array),
            'acceleration': np.gradient(np.gradient(response, time_array), time_array)
        })

    @factory.lazy_attribute
    def metadata(self):
        """Generate realistic metadata."""
        return {
            "solver_iterations": random.randint(10, 1000),
            "convergence_achieved": self.status == SimulationStatus.COMPLETED,
            "peak_memory_mb": random.uniform(100, 8000),
            "cpu_time_seconds": random.uniform(1, 7200),
            "node_count": random.randint(100, 100000),
            "element_count": random.randint(50, 50000),
            "coordinate_system": faker.Faker().coordinate_system(),
            "units": {
                "length": "m",
                "force": "N",
                "time": "s",
                "pressure": "Pa"
            }
        }


class AnalysisJobFactory(factory.Factory):
    """Factory for analysis job data."""

    class Meta:
        model = dict

    id = factory.Faker('simulation_id')
    name = factory.Faker('catch_phrase')
    analysis_type = factory.Faker('random_element', elements=[t.value for t in SimulationType])
    created_by = factory.Faker('user_name')
    created_at = factory.Faker('date_time_this_month')
    priority = factory.Faker('random_element', elements=['low', 'medium', 'high', 'critical'])
    estimated_duration_minutes = factory.LazyFunction(lambda: random.randint(5, 480))

    @factory.lazy_attribute
    def input_files(self):
        """Generate list of input files."""
        file_count = random.randint(1, 5)
        files = []
        for _ in range(file_count):
            files.append({
                "filename": faker.Faker().file_name(extension="dat"),
                "size_mb": round(random.uniform(0.1, 500.0), 2),
                "checksum": faker.Faker().sha256()
            })
        return files

    @factory.lazy_attribute
    def configuration(self):
        """Generate analysis configuration."""
        return {
            "mesh_size": random.uniform(0.01, 1.0),
            "boundary_conditions": random.randint(5, 50),
            "load_cases": random.randint(1, 10),
            "output_frequency": random.randint(1, 100),
            "save_intermediate": random.choice([True, False]),
            "parallel_cores": random.choice([1, 2, 4, 8, 16])
        }


class MaterialTestDataFactory(factory.Factory):
    """Factory for material test data."""

    class Meta:
        model = dict

    material_id = factory.Faker('uuid4')
    material_name = factory.Faker('material_name')
    test_date = factory.Faker('date_this_year')
    test_type = factory.Faker('random_element', elements=[
        'tensile', 'compression', 'fatigue', 'impact', 'creep', 'hardness'
    ])
    specimen_id = factory.LazyFunction(lambda: f"SPEC_{random.randint(1000, 9999)}")
    temperature_celsius = factory.LazyFunction(lambda: random.uniform(-40, 200))

    @factory.lazy_attribute
    def test_results(self):
        """Generate realistic material test results."""
        if self.test_type == 'tensile':
            return {
                "yield_strength_mpa": random.uniform(200, 800),
                "ultimate_strength_mpa": random.uniform(400, 1200),
                "elongation_percent": random.uniform(5, 40),
                "elastic_modulus_gpa": random.uniform(200, 220),
                "poissons_ratio": random.uniform(0.25, 0.35)
            }
        elif self.test_type == 'fatigue':
            return {
                "cycles_to_failure": random.randint(10000, 10000000),
                "stress_amplitude_mpa": random.uniform(50, 300),
                "stress_ratio": random.uniform(-1, 0.8),
                "frequency_hz": random.uniform(1, 100)
            }
        elif self.test_type == 'impact':
            return {
                "impact_energy_j": random.uniform(10, 300),
                "absorbed_energy_j": random.uniform(5, 250),
                "fracture_appearance": random.choice(['ductile', 'brittle', 'mixed'])
            }
        else:
            return {
                "peak_load_n": random.uniform(1000, 100000),
                "displacement_mm": random.uniform(0.1, 50),
                "test_duration_s": random.uniform(60, 3600)
            }

    @factory.lazy_attribute
    def stress_strain_data(self):
        """Generate stress-strain curve data."""
        if self.test_type not in ['tensile', 'compression']:
            return None

        n_points = random.randint(100, 1000)
        max_strain = random.uniform(0.005, 0.20)  # 0.5% to 20%
        strain = np.linspace(0, max_strain, n_points)

        # Generate realistic stress-strain curve
        elastic_modulus = random.uniform(200e9, 220e9)  # Pa
        yield_stress = random.uniform(250e6, 400e6)  # Pa
        ultimate_stress = random.uniform(400e6, 600e6)  # Pa

        stress = np.zeros_like(strain)
        elastic_limit = yield_stress / elastic_modulus

        for i, e in enumerate(strain):
            if e <= elastic_limit:
                # Elastic region
                stress[i] = elastic_modulus * e
            else:
                # Plastic region - simplified hardening
                plastic_strain = e - elastic_limit
                hardening_modulus = elastic_modulus * 0.1
                stress[i] = yield_stress + hardening_modulus * plastic_strain

                # Cap at ultimate stress
                if stress[i] > ultimate_stress:
                    stress[i] = ultimate_stress * (1 - 0.1 * (e - strain[np.argmax(stress <= ultimate_stress)]))

        # Add some noise
        noise_level = np.max(stress) * 0.01
        stress += np.random.normal(0, noise_level, len(stress))

        return pd.DataFrame({
            'strain': strain,
            'stress_pa': stress,
            'stress_mpa': stress / 1e6
        })


class LoadCaseFactory(factory.Factory):
    """Factory for load case definitions."""

    class Meta:
        model = dict

    load_case_id = factory.Faker('uuid4')
    name = factory.Faker('load_case_name')
    load_type = factory.Faker('random_element', elements=[
        'point_load', 'distributed_load', 'pressure', 'thermal', 'displacement', 'acceleration'
    ])
    magnitude = factory.LazyFunction(lambda: random.uniform(1, 10000))
    units = factory.LazyAttribute(lambda obj: faker.Faker().engineering_units('force'))
    direction = factory.Faker('random_element', elements=['x', 'y', 'z', 'normal', 'tangential'])

    @factory.lazy_attribute
    def application_points(self):
        """Generate load application points."""
        n_points = random.randint(1, 10)
        points = []
        for _ in range(n_points):
            points.append({
                "x": random.uniform(-10, 10),
                "y": random.uniform(-10, 10),
                "z": random.uniform(-10, 10),
                "node_id": random.randint(1, 10000)
            })
        return points

    @factory.lazy_attribute
    def time_history(self):
        """Generate time history for dynamic loads."""
        if random.random() > 0.6:  # 40% of loads have time history
            return None

        duration = random.uniform(1, 100)
        n_points = random.randint(10, 500)
        time = np.linspace(0, duration, n_points)

        # Generate different types of time histories
        load_type = random.choice(['step', 'ramp', 'sine', 'random'])

        if load_type == 'step':
            magnitude = np.ones_like(time)
            magnitude[time < duration * 0.1] = 0  # Step at 10% of duration
        elif load_type == 'ramp':
            magnitude = time / duration  # Linear ramp
        elif load_type == 'sine':
            frequency = random.uniform(0.1, 5.0)
            magnitude = np.sin(2 * np.pi * frequency * time)
        else:  # random
            magnitude = np.random.normal(0, 1, len(time))

        return pd.DataFrame({
            'time': time,
            'magnitude_factor': magnitude
        })


class TestDataManager:
    """Manager for generating and managing test data sets."""

    def __init__(self):
        self.faker = faker.Faker()
        self.faker.add_provider(EngineeringProvider)

    def generate_simulation_dataset(self, n_simulations: int = 10, **kwargs) -> List[SimulationResult]:
        """Generate a dataset of simulation results."""
        return [SimulationResultFactory(**kwargs) for _ in range(n_simulations)]

    def generate_material_dataset(self, n_materials: int = 5, **kwargs) -> List[Dict]:
        """Generate a dataset of material test data."""
        return [MaterialTestDataFactory(**kwargs) for _ in range(n_materials)]

    def generate_load_cases(self, n_cases: int = 8, **kwargs) -> List[Dict]:
        """Generate a set of load cases."""
        return [LoadCaseFactory(**kwargs) for _ in range(n_cases)]

    def generate_analysis_jobs(self, n_jobs: int = 15, **kwargs) -> List[Dict]:
        """Generate a set of analysis jobs."""
        return [AnalysisJobFactory(**kwargs) for _ in range(n_jobs)]

    def create_realistic_project(self, project_name: str = None) -> Dict[str, Any]:
        """Create a realistic engineering project with all components."""
        if not project_name:
            project_name = self.faker.catch_phrase()

        return {
            "project_info": {
                "name": project_name,
                "id": self.faker.uuid4(),
                "created_date": self.faker.date_this_year(),
                "description": self.faker.text(max_nb_chars=200),
                "client": self.faker.company(),
                "engineer": self.faker.name(),
                "status": self.faker.random_element(['active', 'on_hold', 'completed', 'cancelled'])
            },
            "materials": self.generate_material_dataset(n_materials=random.randint(3, 8)),
            "load_cases": self.generate_load_cases(n_cases=random.randint(5, 15)),
            "simulations": self.generate_simulation_dataset(n_simulations=random.randint(10, 25)),
            "analysis_jobs": self.generate_analysis_jobs(n_jobs=random.randint(8, 20))
        }

    def export_dataset_to_files(self, dataset: Dict[str, Any], output_dir: str):
        """Export dataset to files for testing file I/O."""
        import json
        from pathlib import Path

        output_path = Path(output_dir)
        output_path.mkdir(parents=True, exist_ok=True)

        # Export project info
        with open(output_path / "project_info.json", 'w') as f:
            json.dump(dataset["project_info"], f, indent=2, default=str)

        # Export materials
        materials_df = pd.DataFrame(dataset["materials"])
        materials_df.to_csv(output_path / "materials.csv", index=False)

        # Export load cases
        with open(output_path / "load_cases.json", 'w') as f:
            json.dump(dataset["load_cases"], f, indent=2, default=str)

        # Export simulation summary
        sim_summary = []
        for sim in dataset["simulations"]:
            summary = {
                "simulation_id": sim.simulation_id,
                "status": sim.status.value,
                "start_time": sim.start_time.isoformat(),
                "end_time": sim.end_time.isoformat() if sim.end_time else None,
                "solver_type": sim.parameters.solver_type,
                "duration": sim.parameters.duration
            }
            sim_summary.append(summary)

        sim_df = pd.DataFrame(sim_summary)
        sim_df.to_csv(output_path / "simulations_summary.csv", index=False)

        # Export detailed simulation results
        results_dir = output_path / "simulation_results"
        results_dir.mkdir(exist_ok=True)

        for sim in dataset["simulations"]:
            if sim.output_data is not None:
                filename = f"{sim.simulation_id}_results.csv"
                sim.output_data.to_csv(results_dir / filename, index=False)

        print(f"Dataset exported to {output_path}")


# Convenience functions for quick data generation
def create_test_simulation(**kwargs) -> SimulationResult:
    """Quick function to create a single test simulation."""
    return SimulationResultFactory(**kwargs)


def create_test_material(**kwargs) -> Dict:
    """Quick function to create a single test material."""
    return MaterialTestDataFactory(**kwargs)


def create_test_load_case(**kwargs) -> Dict:
    """Quick function to create a single test load case."""
    return LoadCaseFactory(**kwargs)


if __name__ == "__main__":
    # Demo the factories
    print("Generating test data with factories...")

    # Create test data manager
    manager = TestDataManager()

    # Generate a single simulation
    sim = create_test_simulation(status=SimulationStatus.COMPLETED)
    print(f"Generated simulation: {sim.simulation_id}")
    print(f"Status: {sim.status}")
    print(f"Parameters: {sim.parameters}")

    if sim.output_data is not None:
        print(f"Output data shape: {sim.output_data.shape}")
        print(f"Output columns: {list(sim.output_data.columns)}")

    # Generate a material
    material = create_test_material(test_type='tensile')
    print(f"\nGenerated material: {material['material_name']}")
    print(f"Test type: {material['test_type']}")
    print(f"Test results: {material['test_results']}")

    # Generate a complete project
    project = manager.create_realistic_project("Demo Engineering Project")
    print(f"\nGenerated project: {project['project_info']['name']}")
    print(f"Materials: {len(project['materials'])}")
    print(f"Load cases: {len(project['load_cases'])}")
    print(f"Simulations: {len(project['simulations'])}")

    print("\nTest data generation demo completed!")