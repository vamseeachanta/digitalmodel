"""
ABOUTME: Demonstration of intelligent agent selection system.
Shows how to use the multi-factor scoring algorithm for agent selection.
"""

import json
from pathlib import Path
import sys

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from digitalmodel.modules.automation import (
    AgentPerformanceTracker,
    IntelligentAgentSelector,
)


def setup_demo_data():
    """Create demo performance data for agents."""
    print("Setting up demo performance data...")

    tracker = AgentPerformanceTracker(".claude-flow/agent-performance-demo.db")

    # Simulate historical performance for different agents
    # Agent A: High success rate, fast execution
    print("  - Recording agent-a performance (excellent)")
    for i in range(18):
        tracker.record_task(
            "agent-a", "code-generation", success=True, execution_time=25.0
        )
    for i in range(2):
        tracker.record_task(
            "agent-a", "code-generation", success=False, execution_time=30.0
        )

    # Agent B: Moderate success rate, slower execution
    print("  - Recording agent-b performance (moderate)")
    for i in range(12):
        tracker.record_task(
            "agent-b", "code-review", success=True, execution_time=45.0
        )
    for i in range(8):
        tracker.record_task(
            "agent-b", "code-review", success=False, execution_time=50.0
        )

    # Agent C: New agent, minimal history
    print("  - Recording agent-c performance (new agent)")
    for i in range(3):
        tracker.record_task(
            "agent-c", "testing", success=True, execution_time=35.0
        )

    print("Demo data setup complete!\n")


def demo_basic_selection():
    """Demonstrate basic agent selection."""
    print("=" * 70)
    print("DEMO 1: Basic Agent Selection")
    print("=" * 70)

    # Create temp config for demo
    config_path = "config/agent-selection-weights.yaml"
    registry_path = "modules/config/ai-agents-registry.json"

    # Check if files exist, otherwise skip
    if not Path(registry_path).exists():
        print(f"Note: Registry not found at {registry_path}")
        print("Using minimal demo registry\n")

        # Create minimal registry for demo
        demo_registry = {
            "agents": {
                "agent-a": {
                    "platform": "claude-code",
                    "type": "coder",
                    "capabilities": ["python", "javascript", "testing", "refactoring"]
                },
                "agent-b": {
                    "platform": "claude-flow",
                    "type": "reviewer",
                    "capabilities": ["code review", "security", "quality", "performance"]
                },
                "agent-c": {
                    "platform": "factory-ai",
                    "type": "tester",
                    "capabilities": ["testing", "automation", "ci/cd", "quality assurance"]
                }
            },
            "taskTypeAgentMapping": {
                "code-generation": {
                    "primary": "agent-a",
                    "alternatives": ["agent-b", "agent-c"]
                },
                "code-review": {
                    "primary": "agent-b",
                    "alternatives": ["agent-a"]
                },
                "testing": {
                    "primary": "agent-c",
                    "alternatives": ["agent-a", "agent-b"]
                }
            }
        }

        Path("temp-demo-registry.json").write_text(json.dumps(demo_registry, indent=2))
        registry_path = "temp-demo-registry.json"

    selector = IntelligentAgentSelector(
        config_path=config_path,
        registry_path=registry_path,
        performance_db=".claude-flow/agent-performance-demo.db"
    )

    # Select agent for code generation task
    task_description = "Create a Python REST API with authentication and database integration"

    print(f"Task: {task_description}")
    print()

    result = selector.select_agent(
        task_type="code-generation",
        task_description=task_description
    )

    print("Selection Result:")
    print(json.dumps(result, indent=2))
    print()


def demo_with_load_balancing():
    """Demonstrate selection with agent load consideration."""
    print("=" * 70)
    print("DEMO 2: Selection with Load Balancing")
    print("=" * 70)

    registry_path = "temp-demo-registry.json"
    if not Path(registry_path).exists():
        print("Skipping: Requires demo registry from Demo 1\n")
        return

    selector = IntelligentAgentSelector(
        config_path="config/agent-selection-weights.yaml",
        registry_path=registry_path,
        performance_db=".claude-flow/agent-performance-demo.db"
    )

    # Scenario: agent-a is heavily loaded
    agent_loads = {
        "agent-a": 4,  # Near capacity
        "agent-b": 1,  # Lightly loaded
        "agent-c": 0   # Available
    }

    print("Current agent loads:")
    print(json.dumps(agent_loads, indent=2))
    print()

    task_description = "Refactor authentication module for better security"

    print(f"Task: {task_description}")
    print()

    result = selector.select_agent(
        task_type="code-generation",
        task_description=task_description,
        agent_loads=agent_loads,
        max_concurrent=5
    )

    print("Selection Result (with load balancing):")
    print(json.dumps(result, indent=2))
    print("\nNote: Despite agent-a having best history, high load may shift selection")
    print()


def demo_score_breakdown():
    """Show detailed score breakdown for transparency."""
    print("=" * 70)
    print("DEMO 3: Detailed Score Breakdown")
    print("=" * 70)

    registry_path = "temp-demo-registry.json"
    if not Path(registry_path).exists():
        print("Skipping: Requires demo registry from Demo 1\n")
        return

    selector = IntelligentAgentSelector(
        config_path="config/agent-selection-weights.yaml",
        registry_path=registry_path,
        performance_db=".claude-flow/agent-performance-demo.db"
    )

    task_description = "Create comprehensive test suite with unit and integration tests"

    print(f"Task: {task_description}")
    print()

    # Get scores for all agents
    print("Comparing all agents:\n")

    for agent in ["agent-a", "agent-b", "agent-c"]:
        score, components = selector.calculate_agent_score(
            agent,
            task_description,
            current_load=None,
            max_concurrent=5
        )

        print(f"{agent}:")
        print(f"  Total Score: {score:.3f}")
        print(f"  Components:")
        for key, value in components.items():
            if key != "total":
                weight = selector.weights.get(key, 0)
                contribution = value * weight
                print(f"    {key:20s}: {value:.3f} (weight: {weight:.2f}, contribution: {contribution:.3f})")
        print()


def demo_domain_matching():
    """Demonstrate domain expertise matching."""
    print("=" * 70)
    print("DEMO 4: Domain Expertise Matching")
    print("=" * 70)

    registry_path = "temp-demo-registry.json"
    if not Path(registry_path).exists():
        print("Skipping: Requires demo registry from Demo 1\n")
        return

    selector = IntelligentAgentSelector(
        config_path="config/agent-selection-weights.yaml",
        registry_path=registry_path,
        performance_db=".claude-flow/agent-performance-demo.db"
    )

    # Test different task descriptions
    tasks = [
        ("Python API development", "code-generation"),
        ("Security code review and vulnerability analysis", "code-review"),
        ("Automated testing and CI/CD pipeline", "testing"),
    ]

    for task_desc, task_type in tasks:
        print(f"Task: {task_desc}")
        print(f"Type: {task_type}")

        result = selector.select_agent(task_type, task_desc)

        print(f"  → Selected: {result['agent']}")
        print(f"  → Confidence: {result['confidence']:.3f}")
        print(f"  → Reasoning: {result['reasoning']}")
        print()


def cleanup_demo():
    """Clean up demo files."""
    print("\nCleaning up demo files...")

    files_to_remove = [
        ".claude-flow/agent-performance-demo.db",
        "temp-demo-registry.json"
    ]

    for file in files_to_remove:
        path = Path(file)
        if path.exists():
            path.unlink()
            print(f"  Removed: {file}")


def main():
    """Run all demos."""
    print("\n" + "=" * 70)
    print("INTELLIGENT AGENT SELECTION SYSTEM - DEMONSTRATION")
    print("=" * 70)
    print()

    try:
        # Setup
        setup_demo_data()

        # Run demos
        demo_basic_selection()
        demo_with_load_balancing()
        demo_score_breakdown()
        demo_domain_matching()

        # Show summary
        print("=" * 70)
        print("DEMO COMPLETE")
        print("=" * 70)
        print()
        print("Key Features Demonstrated:")
        print("  ✓ Multi-factor scoring algorithm")
        print("  ✓ Historical performance tracking")
        print("  ✓ Load balancing and availability checks")
        print("  ✓ Domain expertise matching")
        print("  ✓ Transparent score breakdown")
        print()
        print("To use in production:")
        print("  1. Configure weights in config/agent-selection-weights.yaml")
        print("  2. Ensure agent registry is up to date")
        print("  3. Use CLI: python -m src.digitalmodel.modules.automation.intelligent_agent_selector")
        print("  4. Or use bash wrapper: ./modules/automation/agent_orchestrator.sh")
        print()

    finally:
        cleanup_demo()


if __name__ == "__main__":
    main()
