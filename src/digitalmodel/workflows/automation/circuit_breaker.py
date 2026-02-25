"""
ABOUTME: Circuit breaker pattern implementation for agent fault tolerance.
Provides automatic failure detection, fallback agent selection, and recovery management.
"""

import json
import time
from datetime import datetime, timedelta
from enum import Enum
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Tuple
from dataclasses import dataclass, asdict
import logging
import threading


class CircuitState(Enum):
    """Circuit breaker states."""
    CLOSED = "CLOSED"      # Normal operation
    OPEN = "OPEN"          # Failure state, fast-fail
    HALF_OPEN = "HALF_OPEN"  # Testing recovery


@dataclass
class CircuitBreakerConfig:
    """Configuration for circuit breaker."""
    failure_threshold: int = 5        # Failures before OPEN
    failure_window: int = 60          # Seconds to track failures
    open_duration: int = 30           # Seconds before HALF-OPEN
    success_threshold: int = 2        # Successes to close circuit
    timeout: int = 30                 # Agent execution timeout (seconds)
    checkpoint_retention: int = 5     # Number of checkpoints to keep


@dataclass
class CircuitMetrics:
    """Metrics tracked by circuit breaker."""
    success_count: int = 0
    failure_count: int = 0
    total_executions: int = 0
    circuit_opens: int = 0
    last_failure_time: Optional[float] = None
    last_recovery_time: Optional[float] = None

    @property
    def success_rate(self) -> float:
        """Calculate success rate."""
        if self.total_executions == 0:
            return 0.0
        return (self.success_count / self.total_executions) * 100.0

    @property
    def recovery_time(self) -> Optional[float]:
        """Calculate recovery time in seconds."""
        if self.last_failure_time and self.last_recovery_time:
            return self.last_recovery_time - self.last_failure_time
        return None


class CircuitOpenError(Exception):
    """Raised when circuit is open and execution is blocked."""
    def __init__(self, message: str, fallback_agent: Optional[str] = None):
        super().__init__(message)
        self.fallback_agent = fallback_agent


class AllAgentsFailedError(Exception):
    """Raised when all agents including fallbacks have failed."""
    def __init__(self, message: str, errors: List[str]):
        super().__init__(message)
        self.errors = errors


class CircuitBreaker:
    """
    Circuit breaker for agent fault tolerance.

    Implements three-state circuit breaker pattern:
    - CLOSED: Normal operation
    - OPEN: Fast-fail after threshold exceeded
    - HALF-OPEN: Testing recovery
    """

    # Default fallback agent registry
    DEFAULT_FALLBACKS = {
        "coder": ["backend-dev", "sparc-coder"],
        "tester": ["tdd-london-swarm"],
        "researcher": ["code-analyzer"],
        "reviewer": ["code-review-swarm"],
        "planner": ["task-orchestrator"],
    }

    def __init__(
        self,
        agent_type: str,
        config: Optional[CircuitBreakerConfig] = None,
        storage_path: Optional[Path] = None,
        fallback_registry: Optional[Dict[str, List[str]]] = None
    ):
        """
        Initialize circuit breaker.

        Args:
            agent_type: Type of agent being protected
            config: Circuit breaker configuration
            storage_path: Path for state persistence
            fallback_registry: Custom fallback agent mappings
        """
        self.agent_type = agent_type
        self.config = config or CircuitBreakerConfig()
        self.storage_path = storage_path or Path(".claude-flow")
        self.storage_path.mkdir(parents=True, exist_ok=True)

        # Setup logging
        self.logger = self._setup_logging()

        # State management
        self.state = CircuitState.CLOSED
        self.state_changed_at = time.time()
        self.failure_times: List[float] = []
        self.success_count_in_half_open = 0
        self.metrics = CircuitMetrics()
        self._lock = threading.Lock()

        # Fallback registry
        self.fallback_registry = fallback_registry or self.DEFAULT_FALLBACKS

        # Load persisted state
        self._load_state()

    def _setup_logging(self) -> logging.Logger:
        """Setup logging for circuit breaker."""
        log_file = self.storage_path / "circuit-breaker.log"

        logger = logging.getLogger(f"circuit-breaker.{self.agent_type}")
        logger.setLevel(logging.INFO)

        # File handler
        fh = logging.FileHandler(log_file)
        fh.setLevel(logging.INFO)

        # Format
        formatter = logging.Formatter(
            "[%(asctime)s] [%(name)s] %(message)s",
            datefmt="%Y-%m-%d %H:%M:%S"
        )
        fh.setFormatter(formatter)

        logger.addHandler(fh)
        return logger

    def _get_state_file(self) -> Path:
        """Get path to state persistence file."""
        return self.storage_path / f"circuit-state-{self.agent_type}.json"

    def _load_state(self) -> None:
        """Load persisted circuit state."""
        state_file = self._get_state_file()
        if not state_file.exists():
            return

        try:
            with open(state_file, 'r') as f:
                data = json.load(f)

            self.state = CircuitState(data.get("state", "CLOSED"))
            self.state_changed_at = data.get("state_changed_at", time.time())
            self.failure_times = data.get("failure_times", [])
            self.success_count_in_half_open = data.get("success_count_in_half_open", 0)

            # Load metrics
            metrics_data = data.get("metrics", {})
            self.metrics = CircuitMetrics(**metrics_data)

            self.logger.info(f"Loaded state: {self.state.value}")

        except Exception as e:
            self.logger.error(f"Failed to load state: {e}")

    def _save_state(self) -> None:
        """Persist circuit state."""
        state_file = self._get_state_file()

        data = {
            "state": self.state.value,
            "state_changed_at": self.state_changed_at,
            "failure_times": self.failure_times,
            "success_count_in_half_open": self.success_count_in_half_open,
            "metrics": asdict(self.metrics)
        }

        try:
            with open(state_file, 'w') as f:
                json.dump(data, f, indent=2)
        except Exception as e:
            self.logger.error(f"Failed to save state: {e}")

    def _transition_state(self, new_state: CircuitState, reason: str = "") -> None:
        """
        Transition to new circuit state.

        Args:
            new_state: New circuit state
            reason: Reason for transition
        """
        if self.state == new_state:
            return

        old_state = self.state
        self.state = new_state
        self.state_changed_at = time.time()

        # Log transition
        log_msg = f"STATE_TRANSITION: {old_state.value} → {new_state.value}"
        if reason:
            log_msg += f" ({reason})"
        self.logger.info(log_msg)

        # Update metrics
        if new_state == CircuitState.OPEN:
            self.metrics.circuit_opens += 1
        elif new_state == CircuitState.CLOSED and old_state == CircuitState.HALF_OPEN:
            self.metrics.last_recovery_time = time.time()

        # Persist state
        self._save_state()

    def _clean_old_failures(self) -> None:
        """Remove failures outside the failure window."""
        current_time = time.time()
        cutoff_time = current_time - self.config.failure_window
        self.failure_times = [t for t in self.failure_times if t > cutoff_time]

    def _record_failure(self) -> None:
        """Record a failure and update circuit state."""
        current_time = time.time()

        with self._lock:
            self.failure_times.append(current_time)
            self._clean_old_failures()

            self.metrics.failure_count += 1
            self.metrics.last_failure_time = current_time

            # Check if we should open circuit
            if self.state == CircuitState.CLOSED:
                if len(self.failure_times) >= self.config.failure_threshold:
                    self._transition_state(
                        CircuitState.OPEN,
                        f"{len(self.failure_times)} failures in {self.config.failure_window}s"
                    )

            elif self.state == CircuitState.HALF_OPEN:
                # Any failure in HALF_OPEN returns to OPEN
                self._transition_state(
                    CircuitState.OPEN,
                    "failure during recovery testing"
                )
                self.success_count_in_half_open = 0

            self._save_state()

    def _record_success(self) -> None:
        """Record a success and update circuit state."""
        with self._lock:
            self.metrics.success_count += 1

            if self.state == CircuitState.HALF_OPEN:
                self.success_count_in_half_open += 1

                if self.success_count_in_half_open >= self.config.success_threshold:
                    self._transition_state(
                        CircuitState.CLOSED,
                        f"{self.success_count_in_half_open} consecutive successes"
                    )
                    self.success_count_in_half_open = 0
                    self.failure_times.clear()

            self._save_state()

    def _check_half_open_transition(self) -> None:
        """Check if circuit should transition from OPEN to HALF_OPEN."""
        if self.state != CircuitState.OPEN:
            return

        current_time = time.time()
        time_in_open = current_time - self.state_changed_at

        if time_in_open >= self.config.open_duration:
            self._transition_state(
                CircuitState.HALF_OPEN,
                f"after {time_in_open:.1f}s in OPEN"
            )

    def _get_fallback_agents(self) -> List[str]:
        """
        Get fallback agents for current agent type.

        Returns:
            List of fallback agent types
        """
        return self.fallback_registry.get(self.agent_type, [])

    def execute(
        self,
        agent_function: Callable,
        args: Optional[Dict[str, Any]] = None,
        kwargs: Optional[Dict[str, Any]] = None,
        fallback_agents: Optional[List[str]] = None
    ) -> Any:
        """
        Execute agent function with circuit breaker protection.

        Args:
            agent_function: Function to execute
            args: Positional arguments dict
            kwargs: Keyword arguments
            fallback_agents: Optional custom fallback agents

        Returns:
            Result from agent function

        Raises:
            CircuitOpenError: When circuit is open
            AllAgentsFailedError: When all agents fail
        """
        args = args or {}
        kwargs = kwargs or {}

        with self._lock:
            self.metrics.total_executions += 1
            self._check_half_open_transition()
            current_state = self.state

        # If circuit is OPEN, try fallback
        if current_state == CircuitState.OPEN:
            fallbacks = fallback_agents or self._get_fallback_agents()
            if fallbacks:
                fallback_name = fallbacks[0]  # Use first available
                self.logger.warning(
                    f"Circuit OPEN, attempting fallback: {fallback_name}"
                )
                raise CircuitOpenError(
                    f"Circuit breaker OPEN for agent {self.agent_type}",
                    fallback_agent=fallback_name
                )
            else:
                raise CircuitOpenError(
                    f"Circuit breaker OPEN for agent {self.agent_type}, no fallbacks available"
                )

        # Execute with timeout
        try:
            import signal
            import threading

            def timeout_handler(signum, frame):
                raise TimeoutError(f"Agent execution exceeded {self.config.timeout}s")

            # Set timeout (Unix only, Windows will skip)
            # Also skip if not in main thread (signals only work in main thread)
            can_use_signal = (
                hasattr(signal, 'SIGALRM') and
                threading.current_thread() is threading.main_thread()
            )

            if can_use_signal:
                try:
                    signal.signal(signal.SIGALRM, timeout_handler)
                    signal.alarm(self.config.timeout)
                except (AttributeError, ValueError):
                    can_use_signal = False

            try:
                result = agent_function(**args, **kwargs)
                self._record_success()
                return result
            finally:
                if can_use_signal:
                    try:
                        signal.alarm(0)  # Cancel timeout
                    except (AttributeError, ValueError):
                        pass

        except Exception as e:
            self._record_failure()
            self.logger.error(f"Agent execution failed: {e}")
            raise

    def reset(self) -> None:
        """Manually reset circuit to CLOSED state."""
        with self._lock:
            self._transition_state(CircuitState.CLOSED, "manual reset")
            self.failure_times.clear()
            self.success_count_in_half_open = 0
            self._save_state()

    def get_state(self) -> Dict[str, Any]:
        """
        Get current circuit state and metrics.

        Returns:
            Dictionary with state and metrics
        """
        with self._lock:
            return {
                "agent_type": self.agent_type,
                "state": self.state.value,
                "state_changed_at": datetime.fromtimestamp(self.state_changed_at).isoformat(),
                "failure_count_in_window": len(self.failure_times),
                "success_count_in_half_open": self.success_count_in_half_open,
                "metrics": {
                    "success_rate": f"{self.metrics.success_rate:.1f}%",
                    "total_executions": self.metrics.total_executions,
                    "success_count": self.metrics.success_count,
                    "failure_count": self.metrics.failure_count,
                    "circuit_opens": self.metrics.circuit_opens,
                    "recovery_time": f"{self.metrics.recovery_time:.1f}s" if self.metrics.recovery_time else None
                }
            }


class CircuitBreakerManager:
    """
    Manages multiple circuit breakers for different agent types.
    """

    def __init__(
        self,
        config: Optional[CircuitBreakerConfig] = None,
        storage_path: Optional[Path] = None
    ):
        """
        Initialize circuit breaker manager.

        Args:
            config: Default configuration for all breakers
            storage_path: Path for state persistence
        """
        self.config = config or CircuitBreakerConfig()
        self.storage_path = storage_path or Path(".claude-flow")
        self.breakers: Dict[str, CircuitBreaker] = {}
        self._lock = threading.Lock()

    def get_breaker(self, agent_type: str) -> CircuitBreaker:
        """
        Get or create circuit breaker for agent type.

        Args:
            agent_type: Type of agent

        Returns:
            Circuit breaker instance
        """
        with self._lock:
            if agent_type not in self.breakers:
                self.breakers[agent_type] = CircuitBreaker(
                    agent_type=agent_type,
                    config=self.config,
                    storage_path=self.storage_path
                )
            return self.breakers[agent_type]

    def get_all_states(self) -> Dict[str, Dict[str, Any]]:
        """
        Get state of all circuit breakers.

        Returns:
            Dictionary mapping agent type to state
        """
        with self._lock:
            return {
                agent_type: breaker.get_state()
                for agent_type, breaker in self.breakers.items()
            }

    def reset_all(self) -> None:
        """Reset all circuit breakers."""
        with self._lock:
            for breaker in self.breakers.values():
                breaker.reset()
