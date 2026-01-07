"""
ABOUTME: Demonstration of circuit breaker pattern for agent fault tolerance.
Shows real-world usage patterns and integration examples.
"""

from pathlib import Path
from digitalmodel.modules.automation.circuit_breaker import (
    CircuitBreaker,
    CircuitBreakerConfig,
    CircuitOpenError,
    CircuitBreakerManager,
)
from digitalmodel.modules.automation.rollback_manager import RollbackManager


def demo_basic_circuit_breaker():
    """Demonstrate basic circuit breaker usage."""
    print("\n=== Basic Circuit Breaker Demo ===\n")

    # Create circuit breaker for a coder agent
    breaker = CircuitBreaker(agent_type="coder")

    # Simulate agent function
    def coder_agent_function(task: str):
        """Simulated coder agent."""
        print(f"Executing task: {task}")
        return f"Completed: {task}"

    # Execute with circuit breaker protection
    try:
        result = breaker.execute(
            agent_function=coder_agent_function,
            args={"task": "implement authentication"}
        )
        print(f"[OK] Result: {result}")

        # Check circuit state
        state = breaker.get_state()
        print(f"\nCircuit State: {state['state']}")
        print(f"Success Rate: {state['metrics']['success_rate']}")

    except CircuitOpenError as e:
        print(f"[\!] Circuit OPEN: {e}")
        print(f"   Fallback agent: {e.fallback_agent}")


def demo_failure_handling():
    """Demonstrate circuit breaker failure handling."""
    print("\n=== Failure Handling Demo ===\n")

    config = CircuitBreakerConfig(
        failure_threshold=3,
        failure_window=60,
        open_duration=5,
        success_threshold=2
    )

    breaker = CircuitBreaker(agent_type="tester", config=config)

    # Simulate failing agent
    def failing_agent_function(task: str):
        """Agent that always fails."""
        raise RuntimeError("Simulated agent failure")

    # Trigger failures to open circuit
    print("Triggering failures to open circuit...")
    for i in range(3):
        try:
            breaker.execute(
                agent_function=failing_agent_function,
                args={"task": f"test-{i}"}
            )
        except RuntimeError as e:
            print(f"  Failure {i+1}: {e}")

    # Check if circuit is open
    state = breaker.get_state()
    print(f"\n[OK] Circuit State: {state['state']}")
    print(f"  Failures: {state['failure_count_in_window']}")

    # Try to execute - should raise CircuitOpenError
    print("\nAttempting execution with open circuit...")
    try:
        breaker.execute(
            agent_function=failing_agent_function,
            args={"task": "test-after-open"}
        )
    except CircuitOpenError as e:
        print(f"[OK] Circuit OPEN as expected: {e}")
        print(f"  Suggested fallback: {e.fallback_agent}")


def demo_rollback_integration():
    """Demonstrate rollback manager integration."""
    print("\n=== Rollback Integration Demo ===\n")

    # Setup
    breaker = CircuitBreaker(agent_type="coder")
    rollback = RollbackManager(
        workflow_id="demo-workflow",
        checkpoint_retention=3,
        git_enabled=False  # Disable git for demo
    )

    # Create temporary test file
    test_file = Path("temp_demo_file.txt")
    test_file.write_text("Original content")

    # Create checkpoint
    print("Creating checkpoint...")
    checkpoint_id = rollback.create_checkpoint(files=[test_file])
    print(f"[OK] Checkpoint created: {checkpoint_id}")

    # Modify file (simulating agent work)
    print("\nModifying file...")
    test_file.write_text("Modified content by agent")
    print(f"  Current content: {test_file.read_text()}")

    # Simulate failure and rollback
    print("\nSimulating failure, rolling back...")
    success = rollback.rollback_to(checkpoint_id)

    if success:
        print(f"[OK] Rollback successful")
        print(f"  Restored content: {test_file.read_text()}")

    # Cleanup
    test_file.unlink()
    rollback.cleanup()


def demo_multi_agent_manager():
    """Demonstrate circuit breaker manager for multiple agents."""
    print("\n=== Multi-Agent Manager Demo ===\n")

    # Create manager
    manager = CircuitBreakerManager()

    # Get breakers for different agent types
    coder_breaker = manager.get_breaker("coder")
    tester_breaker = manager.get_breaker("tester")
    reviewer_breaker = manager.get_breaker("reviewer")

    print("Managing circuits for 3 agent types:")

    # Simulate some executions
    def dummy_agent(task: str):
        return f"Done: {task}"

    # Execute on different agents
    coder_breaker.execute(dummy_agent, args={"task": "code feature"})
    tester_breaker.execute(dummy_agent, args={"task": "run tests"})
    reviewer_breaker.execute(dummy_agent, args={"task": "review code"})

    # Get all states
    all_states = manager.get_all_states()

    print("\nCircuit States:")
    for agent_type, state in all_states.items():
        print(f"\n  {agent_type}:")
        print(f"    State: {state['state']}")
        print(f"    Executions: {state['metrics']['total_executions']}")
        print(f"    Success Rate: {state['metrics']['success_rate']}")


def demo_full_workflow():
    """Demonstrate complete workflow with circuit breaker and rollback."""
    print("\n=== Full Workflow Demo ===\n")

    # Configuration
    config = CircuitBreakerConfig(
        failure_threshold=2,
        failure_window=60,
        open_duration=3,
        success_threshold=1
    )

    # Setup components
    breaker = CircuitBreaker(agent_type="coder", config=config)
    rollback = RollbackManager(workflow_id="full-demo", git_enabled=False)

    # Create test file
    test_file = Path("workflow_demo.txt")
    test_file.write_text("Initial state")

    print("Starting workflow...")

    # Step 1: Create checkpoint
    print("\n1. Creating checkpoint...")
    checkpoint_id = rollback.create_checkpoint(files=[test_file])
    print(f"   [OK] Checkpoint: {checkpoint_id}")

    # Step 2: Execute agent (success)
    print("\n2. Executing primary agent...")

    def primary_agent(task: str):
        """Primary agent that succeeds."""
        test_file.write_text("Updated by primary agent")
        return "Primary agent success"

    try:
        result = breaker.execute(
            agent_function=primary_agent,
            args={"task": "update file"}
        )
        print(f"   [OK] {result}")
        print(f"   File content: {test_file.read_text()}")

    except CircuitOpenError as e:
        print(f"   [\!] Circuit open, using fallback: {e.fallback_agent}")

        # Rollback changes
        print("\n3. Rolling back to checkpoint...")
        rollback.rollback_to(checkpoint_id)
        print(f"   [OK] Restored content: {test_file.read_text()}")

        # Execute fallback agent
        print(f"\n4. Executing fallback agent: {e.fallback_agent}")
        # (In real scenario, would execute actual fallback agent)

    # Step 3: Get final state
    print("\n5. Final circuit state:")
    state = breaker.get_state()
    print(f"   State: {state['state']}")
    print(f"   Success Rate: {state['metrics']['success_rate']}")
    print(f"   Total Executions: {state['metrics']['total_executions']}")

    # Cleanup
    test_file.unlink()
    rollback.cleanup()


def demo_custom_fallback_registry():
    """Demonstrate custom fallback agent registry."""
    print("\n=== Custom Fallback Registry Demo ===\n")

    # Define custom fallback mappings
    custom_registry = {
        "my-agent": ["backup-agent-1", "backup-agent-2"],
        "critical-agent": ["high-priority-backup"],
    }

    # Create breaker with custom registry
    breaker = CircuitBreaker(
        agent_type="my-agent",
        fallback_registry=custom_registry
    )

    print("Custom fallback registry configured:")
    print(f"  my-agent → {custom_registry['my-agent']}")
    print(f"  critical-agent → {custom_registry['critical-agent']}")

    # Force circuit open to demonstrate fallback
    breaker.state = breaker.state.OPEN

    print("\nForcing circuit OPEN to demonstrate fallback...")

    def dummy_agent():
        return "result"

    try:
        breaker.execute(dummy_agent)
    except CircuitOpenError as e:
        print(f"[OK] Circuit OPEN")
        print(f"  Suggested fallback: {e.fallback_agent}")
        print(f"  (from custom registry)")


def main():
    """Run all demonstrations."""
    print("\n" + "="*60)
    print("Circuit Breaker Pattern Demonstration")
    print("="*60)

    demos = [
        ("Basic Usage", demo_basic_circuit_breaker),
        ("Failure Handling", demo_failure_handling),
        ("Rollback Integration", demo_rollback_integration),
        ("Multi-Agent Manager", demo_multi_agent_manager),
        ("Full Workflow", demo_full_workflow),
        ("Custom Fallback Registry", demo_custom_fallback_registry),
    ]

    for name, demo_func in demos:
        try:
            demo_func()
        except Exception as e:
            print(f"\n[\!] Demo '{name}' failed: {e}")

    print("\n" + "="*60)
    print("All demonstrations completed!")
    print("="*60 + "\n")


if __name__ == "__main__":
    main()
