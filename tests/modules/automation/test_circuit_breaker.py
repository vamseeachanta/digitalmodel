"""
ABOUTME: Tests for circuit breaker pattern implementation.
Validates state transitions, failure detection, and fallback mechanisms.
"""

import pytest
import time
from pathlib import Path
from unittest.mock import Mock, patch
import tempfile
import shutil

from digitalmodel.modules.automation.circuit_breaker import (
    CircuitBreaker,
    CircuitBreakerConfig,
    CircuitState,
    CircuitOpenError,
    AllAgentsFailedError,
    CircuitBreakerManager,
)


@pytest.fixture
def temp_storage():
    """Create temporary storage directory."""
    temp_dir = Path(tempfile.mkdtemp())
    yield temp_dir
    # Windows-safe cleanup: retry with delay to allow file handles to close
    import time
    max_retries = 3
    for i in range(max_retries):
        try:
            shutil.rmtree(temp_dir)
            break
        except PermissionError:
            if i < max_retries - 1:
                time.sleep(0.5)
            else:
                # Last resort: ignore errors on Windows
                shutil.rmtree(temp_dir, ignore_errors=True)


@pytest.fixture
def config():
    """Create test configuration."""
    return CircuitBreakerConfig(
        failure_threshold=3,
        failure_window=10,
        open_duration=2,
        success_threshold=2,
        timeout=5,
        checkpoint_retention=3
    )


@pytest.fixture
def breaker(config, temp_storage):
    """Create circuit breaker instance."""
    return CircuitBreaker(
        agent_type="test-agent",
        config=config,
        storage_path=temp_storage
    )


class TestCircuitBreakerStateTransitions:
    """Test circuit breaker state transitions."""

    def test_initial_state_is_closed(self, breaker):
        """Test circuit starts in CLOSED state."""
        assert breaker.state == CircuitState.CLOSED

    def test_transition_to_open_on_failures(self, breaker):
        """Test circuit opens after threshold failures."""
        # Mock function that always fails
        def failing_function():
            raise ValueError("Test failure")

        # Execute multiple times to exceed threshold
        for _ in range(3):
            with pytest.raises(ValueError):
                breaker.execute(failing_function)

        # Circuit should be OPEN
        assert breaker.state == CircuitState.OPEN

    def test_transition_to_half_open_after_timeout(self, breaker):
        """Test circuit transitions to HALF_OPEN after timeout."""
        # Force circuit to OPEN
        breaker.state = CircuitState.OPEN
        breaker.state_changed_at = time.time() - 3  # 3 seconds ago

        # Check transition
        breaker._check_half_open_transition()

        assert breaker.state == CircuitState.HALF_OPEN

    def test_transition_to_closed_on_successes(self, breaker):
        """Test circuit closes after successful executions in HALF_OPEN."""
        # Set to HALF_OPEN
        breaker.state = CircuitState.HALF_OPEN

        # Mock successful function
        def successful_function():
            return "success"

        # Execute successful calls
        for _ in range(2):
            result = breaker.execute(successful_function)
            assert result == "success"

        # Circuit should be CLOSED
        assert breaker.state == CircuitState.CLOSED

    def test_half_open_returns_to_open_on_failure(self, breaker):
        """Test HALF_OPEN returns to OPEN on any failure."""
        # Set to HALF_OPEN
        breaker.state = CircuitState.HALF_OPEN

        # Execute failing function
        def failing_function():
            raise ValueError("Test failure")

        with pytest.raises(ValueError):
            breaker.execute(failing_function)

        # Should return to OPEN
        assert breaker.state == CircuitState.OPEN


class TestCircuitBreakerFailureDetection:
    """Test failure detection mechanisms."""

    def test_record_exception_as_failure(self, breaker):
        """Test exceptions are recorded as failures."""
        def failing_function():
            raise RuntimeError("Test error")

        initial_failures = breaker.metrics.failure_count

        with pytest.raises(RuntimeError):
            breaker.execute(failing_function)

        assert breaker.metrics.failure_count == initial_failures + 1

    def test_clean_old_failures(self, breaker):
        """Test old failures are removed from tracking."""
        # Add old failures
        old_time = time.time() - 20  # 20 seconds ago
        breaker.failure_times = [old_time, old_time + 1, old_time + 2]

        # Clean old failures (window is 10 seconds)
        breaker._clean_old_failures()

        # All should be removed
        assert len(breaker.failure_times) == 0

    def test_failure_count_in_window(self, breaker):
        """Test failure counting within time window."""
        # Add failures within window
        current_time = time.time()
        breaker.failure_times = [
            current_time - 5,  # 5 seconds ago
            current_time - 3,  # 3 seconds ago
            current_time - 1,  # 1 second ago
        ]

        breaker._clean_old_failures()

        # All should remain
        assert len(breaker.failure_times) == 3

    def test_timeout_detection(self, breaker):
        """Test timeout is detected as failure."""
        def slow_function():
            time.sleep(10)  # Exceeds 5 second timeout
            return "done"

        # This test may not work on Windows (no SIGALRM)
        # Skip if on Windows
        import platform
        if platform.system() == "Windows":
            pytest.skip("Timeout detection not supported on Windows")

        with pytest.raises(TimeoutError):
            breaker.execute(slow_function)

        assert breaker.metrics.failure_count > 0


class TestCircuitBreakerFallback:
    """Test fallback agent selection."""

    def test_fallback_on_circuit_open(self, config, temp_storage):
        """Test fallback is triggered when circuit is OPEN."""
        # Use agent type with known fallbacks
        breaker = CircuitBreaker(
            agent_type="coder",  # Has fallbacks: backend-dev, sparc-coder
            config=config,
            storage_path=temp_storage
        )

        # Force circuit to OPEN
        breaker.state = CircuitState.OPEN

        def test_function():
            return "result"

        # Should raise CircuitOpenError with fallback
        with pytest.raises(CircuitOpenError) as exc_info:
            breaker.execute(test_function)

        assert exc_info.value.fallback_agent in ["backend-dev", "sparc-coder"]

    def test_no_fallback_when_none_available(self, breaker):
        """Test behavior when no fallbacks are available."""
        # Set agent type with no fallbacks
        breaker.agent_type = "unknown-agent"
        breaker.state = CircuitState.OPEN

        def test_function():
            return "result"

        with pytest.raises(CircuitOpenError) as exc_info:
            breaker.execute(test_function)

        assert "no fallbacks available" in str(exc_info.value)

    def test_custom_fallback_registry(self, config, temp_storage):
        """Test custom fallback agent registry."""
        custom_registry = {
            "my-agent": ["fallback-1", "fallback-2"]
        }

        breaker = CircuitBreaker(
            agent_type="my-agent",
            config=config,
            storage_path=temp_storage,
            fallback_registry=custom_registry
        )

        fallbacks = breaker._get_fallback_agents()
        assert fallbacks == ["fallback-1", "fallback-2"]


class TestCircuitBreakerMetrics:
    """Test metrics tracking."""

    def test_success_rate_calculation(self, breaker):
        """Test success rate is calculated correctly."""
        breaker.metrics.total_executions = 10
        breaker.metrics.success_count = 8

        assert breaker.metrics.success_rate == 80.0

    def test_recovery_time_calculation(self, breaker):
        """Test recovery time is calculated correctly."""
        breaker.metrics.last_failure_time = 100.0
        breaker.metrics.last_recovery_time = 130.0

        assert breaker.metrics.recovery_time == 30.0

    def test_circuit_opens_counter(self, breaker):
        """Test circuit opens are counted."""
        initial_opens = breaker.metrics.circuit_opens

        # Cause circuit to open
        def failing_function():
            raise ValueError("Test failure")

        for _ in range(3):
            with pytest.raises(ValueError):
                breaker.execute(failing_function)

        assert breaker.metrics.circuit_opens == initial_opens + 1

    def test_get_state_returns_metrics(self, breaker):
        """Test get_state includes metrics."""
        state = breaker.get_state()

        assert "metrics" in state
        assert "success_rate" in state["metrics"]
        assert "total_executions" in state["metrics"]
        assert "circuit_opens" in state["metrics"]


class TestCircuitBreakerPersistence:
    """Test state persistence."""

    def test_state_persisted_to_file(self, breaker):
        """Test state is saved to file."""
        # Execute some operations
        def successful_function():
            return "success"

        breaker.execute(successful_function)

        # Check file exists
        state_file = breaker._get_state_file()
        assert state_file.exists()

    def test_state_loaded_on_init(self, config, temp_storage):
        """Test state is loaded when creating new instance."""
        # Create first breaker and execute operations
        breaker1 = CircuitBreaker(
            agent_type="test-agent",
            config=config,
            storage_path=temp_storage
        )

        def successful_function():
            return "success"

        breaker1.execute(successful_function)

        # Create second breaker with same type
        breaker2 = CircuitBreaker(
            agent_type="test-agent",
            config=config,
            storage_path=temp_storage
        )

        # Should have loaded previous state
        assert breaker2.metrics.total_executions == breaker1.metrics.total_executions

    def test_reset_clears_state(self, breaker):
        """Test reset clears circuit state."""
        # Cause some failures
        def failing_function():
            raise ValueError("Test failure")

        for _ in range(2):
            with pytest.raises(ValueError):
                breaker.execute(failing_function)

        # Reset
        breaker.reset()

        assert breaker.state == CircuitState.CLOSED
        assert len(breaker.failure_times) == 0
        assert breaker.success_count_in_half_open == 0


class TestCircuitBreakerManager:
    """Test circuit breaker manager."""

    def test_get_or_create_breaker(self, config, temp_storage):
        """Test manager creates breakers on demand."""
        manager = CircuitBreakerManager(config=config, storage_path=temp_storage)

        breaker1 = manager.get_breaker("agent-1")
        breaker2 = manager.get_breaker("agent-1")

        # Should return same instance
        assert breaker1 is breaker2

    def test_multiple_agent_types(self, config, temp_storage):
        """Test manager handles multiple agent types."""
        manager = CircuitBreakerManager(config=config, storage_path=temp_storage)

        breaker1 = manager.get_breaker("agent-1")
        breaker2 = manager.get_breaker("agent-2")

        assert breaker1 is not breaker2
        assert breaker1.agent_type == "agent-1"
        assert breaker2.agent_type == "agent-2"

    def test_get_all_states(self, config, temp_storage):
        """Test getting all breaker states."""
        manager = CircuitBreakerManager(config=config, storage_path=temp_storage)

        # Create some breakers
        manager.get_breaker("agent-1")
        manager.get_breaker("agent-2")

        states = manager.get_all_states()

        assert "agent-1" in states
        assert "agent-2" in states

    def test_reset_all_breakers(self, config, temp_storage):
        """Test resetting all breakers."""
        manager = CircuitBreakerManager(config=config, storage_path=temp_storage)

        # Create and modify breakers
        breaker1 = manager.get_breaker("agent-1")
        breaker1.state = CircuitState.OPEN

        breaker2 = manager.get_breaker("agent-2")
        breaker2.state = CircuitState.OPEN

        # Reset all
        manager.reset_all()

        assert breaker1.state == CircuitState.CLOSED
        assert breaker2.state == CircuitState.CLOSED


class TestIntegration:
    """Integration tests."""

    def test_full_workflow_with_recovery(self, breaker):
        """Test complete workflow from failure to recovery."""
        call_count = [0]

        def function_that_fails_then_succeeds():
            call_count[0] += 1
            if call_count[0] <= 3:
                raise ValueError("Failing")
            return "success"

        # Fail 3 times to open circuit
        for _ in range(3):
            with pytest.raises(ValueError):
                breaker.execute(function_that_fails_then_succeeds)

        assert breaker.state == CircuitState.OPEN

        # Wait for transition to HALF_OPEN
        time.sleep(2.5)
        breaker._check_half_open_transition()

        assert breaker.state == CircuitState.HALF_OPEN

        # Succeed twice to close circuit
        for _ in range(2):
            result = breaker.execute(function_that_fails_then_succeeds)
            assert result == "success"

        assert breaker.state == CircuitState.CLOSED

    def test_concurrent_access(self, breaker):
        """Test thread-safe concurrent access."""
        import threading

        def successful_function():
            time.sleep(0.01)  # Small delay
            return "success"

        # Execute concurrently
        threads = []
        for _ in range(10):
            t = threading.Thread(target=lambda: breaker.execute(successful_function))
            t.start()
            threads.append(t)

        for t in threads:
            t.join()

        # All should succeed
        assert breaker.metrics.success_count >= 10
