"""
Property-based testing for state machines and workflow invariants.

Tests state transitions, invariants, and system behavior using
Hypothesis state machine testing capabilities.
"""
import pytest
from hypothesis import given, strategies as st, settings, assume
from hypothesis.stateful import RuleBasedStateMachine, rule, precondition, invariant
import enum
from typing import Dict, List, Any, Optional
from dataclasses import dataclass
import time


class SimulationState(enum.Enum):
    """States for engineering simulation workflow."""
    UNINITIALIZED = "uninitialized"
    INITIALIZED = "initialized"
    CONFIGURED = "configured"
    RUNNING = "running"
    PAUSED = "paused"
    COMPLETED = "completed"
    ERROR = "error"


@dataclass
class SimulationConfig:
    """Configuration for engineering simulation."""
    time_step: float
    duration: float
    solver_type: str
    convergence_criteria: float

    def __post_init__(self):
        if self.time_step <= 0:
            raise ValueError("Time step must be positive")
        if self.duration <= 0:
            raise ValueError("Duration must be positive")
        if self.convergence_criteria <= 0:
            raise ValueError("Convergence criteria must be positive")


class EngineeringSimulation:
    """Simplified engineering simulation for testing state machine properties."""

    def __init__(self):
        self.state = SimulationState.UNINITIALIZED
        self.config: Optional[SimulationConfig] = None
        self.current_time = 0.0
        self.results: List[Dict] = []
        self.error_message: Optional[str] = None

    def initialize(self) -> bool:
        """Initialize the simulation."""
        if self.state != SimulationState.UNINITIALIZED:
            return False

        self.state = SimulationState.INITIALIZED
        self.current_time = 0.0
        self.results = []
        self.error_message = None
        return True

    def configure(self, config: SimulationConfig) -> bool:
        """Configure the simulation."""
        if self.state != SimulationState.INITIALIZED:
            return False

        try:
            self.config = config
            self.state = SimulationState.CONFIGURED
            return True
        except Exception as e:
            self.state = SimulationState.ERROR
            self.error_message = str(e)
            return False

    def start(self) -> bool:
        """Start the simulation."""
        if self.state != SimulationState.CONFIGURED:
            return False

        self.state = SimulationState.RUNNING
        return True

    def pause(self) -> bool:
        """Pause the simulation."""
        if self.state != SimulationState.RUNNING:
            return False

        self.state = SimulationState.PAUSED
        return True

    def resume(self) -> bool:
        """Resume the simulation."""
        if self.state != SimulationState.PAUSED:
            return False

        self.state = SimulationState.RUNNING
        return True

    def step(self) -> bool:
        """Execute one simulation step."""
        if self.state != SimulationState.RUNNING:
            return False

        if not self.config:
            self.state = SimulationState.ERROR
            self.error_message = "No configuration"
            return False

        # Simulate computation
        self.current_time += self.config.time_step

        # Add some results
        self.results.append({
            'time': self.current_time,
            'value': self.current_time * 2.0  # Dummy calculation
        })

        # Check if simulation is complete
        if self.current_time >= self.config.duration:
            self.state = SimulationState.COMPLETED

        return True

    def reset(self) -> bool:
        """Reset the simulation to uninitialized state."""
        self.state = SimulationState.UNINITIALIZED
        self.config = None
        self.current_time = 0.0
        self.results = []
        self.error_message = None
        return True

    def get_progress(self) -> float:
        """Get simulation progress as percentage."""
        if not self.config or self.config.duration <= 0:
            return 0.0
        return min(100.0, (self.current_time / self.config.duration) * 100.0)


class SimulationStateMachine(RuleBasedStateMachine):
    """State machine for testing simulation workflow properties."""

    def __init__(self):
        super().__init__()
        self.simulation = EngineeringSimulation()

    @rule()
    def initialize(self):
        """Initialize the simulation."""
        old_state = self.simulation.state
        result = self.simulation.initialize()

        if old_state == SimulationState.UNINITIALIZED:
            assert result is True
            assert self.simulation.state == SimulationState.INITIALIZED
        else:
            assert result is False
            assert self.simulation.state == old_state

    @rule(
        time_step=st.floats(min_value=0.001, max_value=1.0),
        duration=st.floats(min_value=1.0, max_value=100.0),
        solver_type=st.sampled_from(['euler', 'runge_kutta', 'implicit']),
        convergence=st.floats(min_value=1e-6, max_value=1e-2)
    )
    def configure(self, time_step, duration, solver_type, convergence):
        """Configure the simulation."""
        old_state = self.simulation.state

        try:
            config = SimulationConfig(
                time_step=time_step,
                duration=duration,
                solver_type=solver_type,
                convergence_criteria=convergence
            )
            result = self.simulation.configure(config)

            if old_state == SimulationState.INITIALIZED:
                assert result is True
                assert self.simulation.state == SimulationState.CONFIGURED
                assert self.simulation.config == config
            else:
                assert result is False
                assert self.simulation.state == old_state

        except ValueError:
            # Invalid configuration should not change state if we're not in INITIALIZED
            if old_state == SimulationState.INITIALIZED:
                assert self.simulation.state == SimulationState.ERROR
            else:
                assert self.simulation.state == old_state

    @rule()
    @precondition(lambda self: self.simulation.state == SimulationState.CONFIGURED)
    def start(self):
        """Start the simulation."""
        result = self.simulation.start()
        assert result is True
        assert self.simulation.state == SimulationState.RUNNING

    @rule()
    @precondition(lambda self: self.simulation.state == SimulationState.RUNNING)
    def pause(self):
        """Pause the simulation."""
        old_time = self.simulation.current_time
        result = self.simulation.pause()
        assert result is True
        assert self.simulation.state == SimulationState.PAUSED
        # Time shouldn't change when pausing
        assert self.simulation.current_time == old_time

    @rule()
    @precondition(lambda self: self.simulation.state == SimulationState.PAUSED)
    def resume(self):
        """Resume the simulation."""
        old_time = self.simulation.current_time
        result = self.simulation.resume()
        assert result is True
        assert self.simulation.state == SimulationState.RUNNING
        # Time shouldn't change when resuming
        assert self.simulation.current_time == old_time

    @rule()
    @precondition(lambda self: self.simulation.state == SimulationState.RUNNING)
    def step(self):
        """Execute simulation step."""
        old_time = self.simulation.current_time
        old_results_count = len(self.simulation.results)

        result = self.simulation.step()
        assert result is True

        # Time should advance
        if self.simulation.state == SimulationState.RUNNING:
            assert self.simulation.current_time > old_time
        elif self.simulation.state == SimulationState.COMPLETED:
            assert self.simulation.current_time >= old_time

        # Should have more results
        assert len(self.simulation.results) == old_results_count + 1

    @rule()
    def reset(self):
        """Reset the simulation."""
        result = self.simulation.reset()
        assert result is True
        assert self.simulation.state == SimulationState.UNINITIALIZED
        assert self.simulation.config is None
        assert self.simulation.current_time == 0.0
        assert len(self.simulation.results) == 0
        assert self.simulation.error_message is None

    @invariant()
    def time_is_non_negative(self):
        """Time should never be negative."""
        assert self.simulation.current_time >= 0.0

    @invariant()
    def progress_is_valid(self):
        """Progress should be between 0 and 100."""
        progress = self.simulation.get_progress()
        assert 0.0 <= progress <= 100.0

    @invariant()
    def results_are_chronological(self):
        """Results should be in chronological order."""
        if len(self.simulation.results) > 1:
            for i in range(1, len(self.simulation.results)):
                assert self.simulation.results[i]['time'] >= self.simulation.results[i-1]['time']

    @invariant()
    def state_configuration_consistency(self):
        """State and configuration should be consistent."""
        if self.simulation.state in [SimulationState.CONFIGURED, SimulationState.RUNNING,
                                   SimulationState.PAUSED, SimulationState.COMPLETED]:
            assert self.simulation.config is not None

        if self.simulation.state == SimulationState.UNINITIALIZED:
            assert self.simulation.config is None

    @invariant()
    def completed_state_properties(self):
        """Properties that should hold when simulation is completed."""
        if self.simulation.state == SimulationState.COMPLETED:
            assert self.simulation.config is not None
            assert self.simulation.current_time >= self.simulation.config.duration
            assert len(self.simulation.results) > 0


class DataProcessingWorkflow:
    """Simplified data processing workflow for testing."""

    def __init__(self):
        self.data: List[float] = []
        self.processed_data: List[float] = []
        self.is_sorted = False
        self.stats: Optional[Dict[str, float]] = None

    def add_data(self, value: float) -> None:
        """Add data point."""
        self.data.append(value)
        self.is_sorted = False  # Adding data breaks sort order
        self.stats = None       # Invalidate cached statistics

    def sort_data(self) -> None:
        """Sort the data."""
        self.data.sort()
        self.is_sorted = True

    def process_data(self, operation: str) -> bool:
        """Process data with given operation."""
        if not self.data:
            return False

        if operation == "square":
            self.processed_data = [x ** 2 for x in self.data]
        elif operation == "sqrt":
            if any(x < 0 for x in self.data):
                return False
            self.processed_data = [x ** 0.5 for x in self.data]
        elif operation == "log":
            if any(x <= 0 for x in self.data):
                return False
            import math
            self.processed_data = [math.log(x) for x in self.data]
        else:
            return False

        return True

    def calculate_statistics(self) -> Dict[str, float]:
        """Calculate basic statistics."""
        if not self.data:
            return {}

        import statistics
        self.stats = {
            'mean': statistics.mean(self.data),
            'median': statistics.median(self.data),
            'stdev': statistics.stdev(self.data) if len(self.data) > 1 else 0.0,
            'min': min(self.data),
            'max': max(self.data),
            'count': len(self.data)
        }
        return self.stats

    def clear(self) -> None:
        """Clear all data."""
        self.data = []
        self.processed_data = []
        self.is_sorted = False
        self.stats = None


class DataProcessingStateMachine(RuleBasedStateMachine):
    """State machine for testing data processing workflow properties."""

    def __init__(self):
        super().__init__()
        self.workflow = DataProcessingWorkflow()

    @rule(value=st.floats(min_value=-1000.0, max_value=1000.0, allow_nan=False, allow_infinity=False))
    def add_data(self, value):
        """Add data to the workflow."""
        old_count = len(self.workflow.data)
        self.workflow.add_data(value)

        assert len(self.workflow.data) == old_count + 1
        assert self.workflow.data[-1] == value
        assert not self.workflow.is_sorted or old_count == 0  # Should break sort order

    @rule()
    @precondition(lambda self: len(self.workflow.data) > 0)
    def sort_data(self):
        """Sort the data."""
        old_data = self.workflow.data.copy()
        self.workflow.sort_data()

        assert self.workflow.is_sorted
        assert len(self.workflow.data) == len(old_data)
        assert sorted(old_data) == self.workflow.data

    @rule(operation=st.sampled_from(['square', 'sqrt', 'log']))
    @precondition(lambda self: len(self.workflow.data) > 0)
    def process_data(self, operation):
        """Process data with given operation."""
        old_processed_count = len(self.workflow.processed_data)
        result = self.workflow.process_data(operation)

        if result:
            assert len(self.workflow.processed_data) == len(self.workflow.data)
            # Verify operation was applied correctly
            for i, (original, processed) in enumerate(zip(self.workflow.data, self.workflow.processed_data)):
                if operation == "square":
                    assert abs(processed - original ** 2) < 1e-10
                elif operation == "sqrt" and original >= 0:
                    assert abs(processed - original ** 0.5) < 1e-10
                elif operation == "log" and original > 0:
                    import math
                    assert abs(processed - math.log(original)) < 1e-10

    @rule()
    @precondition(lambda self: len(self.workflow.data) > 0)
    def calculate_statistics(self):
        """Calculate statistics."""
        stats = self.workflow.calculate_statistics()

        assert 'mean' in stats
        assert 'median' in stats
        assert 'min' in stats
        assert 'max' in stats
        assert 'count' in stats

        # Verify basic properties
        assert stats['min'] <= stats['mean'] <= stats['max']
        assert stats['min'] <= stats['median'] <= stats['max']
        assert stats['count'] == len(self.workflow.data)

    @rule()
    def clear(self):
        """Clear all data."""
        self.workflow.clear()

        assert len(self.workflow.data) == 0
        assert len(self.workflow.processed_data) == 0
        assert not self.workflow.is_sorted
        assert self.workflow.stats is None

    @invariant()
    def data_count_consistency(self):
        """Data count should be consistent."""
        assert len(self.workflow.data) >= 0

    @invariant()
    def sort_order_consistency(self):
        """If marked as sorted, data should actually be sorted."""
        if self.workflow.is_sorted and len(self.workflow.data) > 1:
            for i in range(1, len(self.workflow.data)):
                assert self.workflow.data[i] >= self.workflow.data[i-1]

    @invariant()
    def statistics_consistency(self):
        """Statistics should be consistent with data."""
        if self.workflow.stats is not None:
            assert self.workflow.stats['count'] == len(self.workflow.data)
            if len(self.workflow.data) > 0:
                assert self.workflow.stats['min'] == min(self.workflow.data)
                assert self.workflow.stats['max'] == max(self.workflow.data)


# Test classes using the state machines
class TestSimulationStateMachine:
    """Test simulation state machine properties."""

    @settings(max_examples=50, stateful_step_count=20)
    def test_simulation_state_machine(self):
        """Test simulation workflow invariants."""
        TestSimulationStateMachine = SimulationStateMachine.TestCase
        TestSimulationStateMachine().runTest()


class TestDataProcessingStateMachine:
    """Test data processing state machine properties."""

    @settings(max_examples=50, stateful_step_count=30)
    def test_data_processing_state_machine(self):
        """Test data processing workflow invariants."""
        TestDataProcessingStateMachine = DataProcessingStateMachine.TestCase
        TestDataProcessingStateMachine().runTest()


# Individual property tests for specific state transitions
class TestStateTransitionProperties:
    """Test specific state transition properties."""

    @given(
        configs=st.lists(
            st.tuples(
                st.floats(min_value=0.001, max_value=1.0),  # time_step
                st.floats(min_value=1.0, max_value=10.0),   # duration
                st.sampled_from(['euler', 'runge_kutta']),   # solver
                st.floats(min_value=1e-6, max_value=1e-2)    # convergence
            ),
            min_size=1,
            max_size=10
        )
    )
    @settings(max_examples=100)
    def test_simulation_reconfiguration_properties(self, configs):
        """Test that simulations can be reconfigured multiple times."""
        sim = EngineeringSimulation()
        sim.initialize()

        for time_step, duration, solver, convergence in configs:
            try:
                config = SimulationConfig(time_step, duration, solver, convergence)
                result = sim.configure(config)

                if sim.state == SimulationState.INITIALIZED:
                    assert result is True
                    assert sim.state == SimulationState.CONFIGURED
                    assert sim.config == config

                    # Reset to allow reconfiguration
                    sim.state = SimulationState.INITIALIZED

            except ValueError:
                # Invalid configurations should result in error state
                assert sim.state == SimulationState.ERROR

    @given(
        operations=st.lists(
            st.sampled_from(['add_positive', 'add_negative', 'add_zero', 'sort', 'clear']),
            min_size=1,
            max_size=20
        )
    )
    @settings(max_examples=50)
    def test_data_workflow_sequences(self, operations):
        """Test various sequences of data operations."""
        workflow = DataProcessingWorkflow()
        import random

        for operation in operations:
            if operation == 'add_positive':
                workflow.add_data(random.uniform(0.1, 100.0))
            elif operation == 'add_negative':
                workflow.add_data(random.uniform(-100.0, -0.1))
            elif operation == 'add_zero':
                workflow.add_data(0.0)
            elif operation == 'sort':
                if workflow.data:
                    old_data = workflow.data.copy()
                    workflow.sort_data()
                    assert workflow.is_sorted
                    assert sorted(old_data) == workflow.data
            elif operation == 'clear':
                workflow.clear()
                assert len(workflow.data) == 0

            # Invariants that should always hold
            assert len(workflow.data) >= 0
            if workflow.is_sorted and len(workflow.data) > 1:
                for i in range(1, len(workflow.data)):
                    assert workflow.data[i] >= workflow.data[i-1]


if __name__ == "__main__":
    # Demo state machine testing
    print("Running state machine property tests...")

    # Quick test of simulation state machine
    sim = EngineeringSimulation()
    print(f"Initial state: {sim.state}")

    sim.initialize()
    print(f"After initialize: {sim.state}")

    config = SimulationConfig(0.1, 5.0, "euler", 1e-4)
    sim.configure(config)
    print(f"After configure: {sim.state}")

    sim.start()
    print(f"After start: {sim.state}")

    for _ in range(3):
        sim.step()
        print(f"After step: {sim.state}, time: {sim.current_time:.1f}, progress: {sim.get_progress():.1f}%")

    print("State machine demo completed!")