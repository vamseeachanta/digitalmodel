"""ABOUTME: Demonstration of work-stealing load balancer with various scenarios.
Shows hot spot handling, cascading failure recovery, and metrics tracking."""

import sys
import time
import random
from pathlib import Path

# Add src to path for direct execution
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

from digitalmodel.workflows.automation import (
    WorkStealingScheduler,
    Agent,
    Task
)


def demo_basic_usage():
    """Demonstrate basic scheduler usage."""
    print("=" * 60)
    print("DEMO 1: Basic Work Stealing")
    print("=" * 60)

    # Create scheduler
    scheduler = WorkStealingScheduler()
    scheduler.start()

    # Create 5 agents with different capabilities
    agents = [
        Agent(agent_id="python_agent_1", capabilities=["python", "testing"]),
        Agent(agent_id="python_agent_2", capabilities=["python", "testing"]),
        Agent(agent_id="data_agent_1", capabilities=["python", "data_analysis"]),
        Agent(agent_id="js_agent_1", capabilities=["javascript", "frontend"]),
        Agent(agent_id="js_agent_2", capabilities=["javascript", "frontend"]),
    ]

    # Register agents
    for agent in agents:
        scheduler.register_agent(agent)
    print(f"[OK] Registered {len(agents)} agents\n")

    # Submit 20 tasks
    print("Submitting 20 tasks...")
    for i in range(20):
        task = Task(
            task_id=f"task_{i:02d}",
            skill_requirements=random.choice([
                ["python"],
                ["javascript"],
                ["data_analysis"]
            ]),
            priority=random.randint(1, 5)
        )
        scheduler.submit_task(task)

    time.sleep(0.5)

    # Show distribution
    print("\nTask Distribution:")
    status = scheduler.get_agent_status()
    for agent_id, info in status.items():
        print(f"  {agent_id:20} | Queue: {info['queue_length']:2} | Capabilities: {info['capabilities']}")

    # Show metrics
    metrics = scheduler.get_metrics()
    print(f"\nMetrics:")
    print(f"  Total steals attempted: {metrics['total_steals']}")
    print(f"  Successful steals: {metrics['successful_steals']}")
    print(f"  Tasks migrated: {metrics['tasks_migrated']}")

    scheduler.stop()
    print("\n[OK] Demo 1 complete\n")


def demo_hot_spot():
    """Demonstrate hot spot load balancing."""
    print("=" * 60)
    print("DEMO 2: Hot Spot Handling")
    print("=" * 60)

    scheduler = WorkStealingScheduler()
    scheduler.start()

    # Create agents
    agents = [
        Agent(agent_id=f"agent_{i}", capabilities=["python"])
        for i in range(5)
    ]

    for agent in agents:
        scheduler.register_agent(agent)

    # Manually create hot spot on agent_0
    print("Creating hot spot on agent_0...")
    hot_agent = agents[0]
    for i in range(20):
        task = Task(
            task_id=f"hotspot_task_{i}",
            skill_requirements=["python"],
            priority=2  # Normal priority (can be stolen)
        )
        hot_agent.enqueue_task(task)

    print(f"[OK] Loaded agent_0 with {hot_agent.queue_length} tasks\n")

    # Show before rebalancing
    print("Before rebalancing:")
    for agent in agents:
        print(f"  {agent.agent_id}: {agent.queue_length} tasks")

    # Trigger rebalancing
    print("\n[>] Triggering rebalance...")
    time.sleep(0.5)
    scheduler._rebalance_push()

    # Show after rebalancing
    print("\nAfter rebalancing:")
    for agent in agents:
        print(f"  {agent.agent_id}: {agent.queue_length} tasks")

    metrics = scheduler.get_metrics()
    print(f"\n[OK] Rebalanced! Migrated {metrics['tasks_migrated']} tasks")

    scheduler.stop()
    print("\n[OK] Demo 2 complete\n")


def demo_cascading_failure():
    """Demonstrate handling of agent failure."""
    print("=" * 60)
    print("DEMO 3: Cascading Failure Recovery")
    print("=" * 60)

    scheduler = WorkStealingScheduler()
    scheduler.start()

    # Create agents
    agents = [
        Agent(agent_id=f"agent_{i}", capabilities=["python"])
        for i in range(5)
    ]

    for agent in agents:
        scheduler.register_agent(agent)

    # Distribute 30 tasks
    print("Distributing 30 tasks across 5 agents...")
    for i in range(30):
        task = Task(
            task_id=f"task_{i}",
            skill_requirements=["python"],
            priority=random.randint(1, 3)
        )
        scheduler.submit_task(task)

    time.sleep(0.3)

    print(f"[OK] Tasks distributed\n")

    # Show distribution
    print("Initial distribution:")
    total_before = 0
    for agent in scheduler.agents.values():
        print(f"  {agent.agent_id}: {agent.queue_length} tasks")
        total_before += agent.queue_length

    # Simulate agent failure
    failed_agent_id = "agent_2"
    print(f"\n[!] Simulating failure of {failed_agent_id}...")
    remaining_tasks = scheduler.unregister_agent(failed_agent_id)

    print(f"[OK] Recovered {len(remaining_tasks)} tasks from failed agent")

    # Redistribute tasks
    print("Redistributing tasks to remaining agents...")
    for task in remaining_tasks:
        scheduler.submit_task(task)

    time.sleep(0.3)

    # Show final distribution
    print("\nFinal distribution (4 agents):")
    total_after = 0
    for agent in scheduler.agents.values():
        print(f"  {agent.agent_id}: {agent.queue_length} tasks")
        total_after += agent.queue_length

    print(f"\n[OK] All {total_after} tasks recovered (started with {total_before})")

    scheduler.stop()
    print("\n[OK] Demo 3 complete\n")


def demo_priority_handling():
    """Demonstrate priority task handling."""
    print("=" * 60)
    print("DEMO 4: Priority Task Handling")
    print("=" * 60)

    scheduler = WorkStealingScheduler()
    scheduler.start()

    # Create agents
    agents = [
        Agent(agent_id=f"agent_{i}", capabilities=["python"])
        for i in range(3)
    ]

    for agent in agents:
        scheduler.register_agent(agent)

    # Submit mixed priority tasks
    print("Submitting mixed priority tasks...")
    priorities = [5, 5, 5, 4, 4, 3, 3, 2, 2, 1]  # Mix of high and low priority

    for i, priority in enumerate(priorities):
        task = Task(
            task_id=f"task_p{priority}_{i}",
            skill_requirements=["python"],
            priority=priority
        )
        scheduler.submit_task(task)

    time.sleep(0.2)

    # Check that tasks are ordered by priority in queues
    print("\nTask queues (should be priority-ordered):")
    for agent in scheduler.agents.values():
        queue_priorities = [t.priority for t in agent.task_queue]
        print(f"  {agent.agent_id}: {queue_priorities}")

    # Try to steal from busy agent
    print("\n[>] Attempting work stealing...")
    busy_agent = max(scheduler.agents.values(), key=lambda a: a.queue_length)
    idle_agent = min(scheduler.agents.values(), key=lambda a: a.queue_length)

    print(f"  Busy agent: {busy_agent.agent_id} ({busy_agent.queue_length} tasks)")
    print(f"  Idle agent: {idle_agent.agent_id} ({idle_agent.queue_length} tasks)")

    # Manually overload one agent with high priority tasks
    for i in range(5):
        task = Task(
            task_id=f"high_priority_{i}",
            skill_requirements=["python"],
            priority=5  # High priority (protected)
        )
        busy_agent.enqueue_task(task)

    print(f"\n[OK] Added 5 high-priority tasks to {busy_agent.agent_id}")
    print(f"  Total queue: {busy_agent.queue_length} tasks")

    # Try to steal (should fail for high priority)
    stolen = scheduler.request_work(idle_agent.agent_id)

    if stolen:
        print(f"\n[OK] Successfully stole task: {stolen.task_id} (priority: {stolen.priority})")
    else:
        print(f"\n[!] Could not steal (high-priority tasks protected)")

    scheduler.stop()
    print("\n[OK] Demo 4 complete\n")


def demo_metrics_tracking():
    """Demonstrate comprehensive metrics tracking."""
    print("=" * 60)
    print("DEMO 5: Metrics Tracking")
    print("=" * 60)

    scheduler = WorkStealingScheduler()
    scheduler.start()

    # Create agents
    agents = [
        Agent(agent_id=f"agent_{i}", capabilities=["python"])
        for i in range(5)
    ]

    for agent in agents:
        scheduler.register_agent(agent)

    # Run workload
    print("Running workload with 50 tasks...")
    for i in range(50):
        task = Task(
            task_id=f"metric_task_{i}",
            skill_requirements=["python"],
            priority=random.randint(1, 3)
        )
        scheduler.submit_task(task)

    # Let scheduler run for a bit
    print("[>] Running scheduler for 2 seconds...")
    time.sleep(2)

    # Get comprehensive metrics
    metrics = scheduler.get_metrics()

    print("\n" + "=" * 60)
    print("COMPREHENSIVE METRICS")
    print("=" * 60)
    print(f"Agent Statistics:")
    print(f"  Total agents: {metrics['agent_count']}")
    print(f"  Busy agents: {metrics['busy_agents']}")
    print(f"  Idle agents: {metrics['idle_agents']}")
    print(f"  Total queued tasks: {metrics['total_queued']}")

    print(f"\nWork Stealing:")
    print(f"  Total steal attempts: {metrics['total_steals']}")
    print(f"  Successful steals: {metrics['successful_steals']}")
    print(f"  Failed steals: {metrics['failed_steals']}")
    print(f"  Success rate: {metrics['success_rate']:.1%}")
    print(f"  Tasks migrated: {metrics['tasks_migrated']}")

    print(f"\nRebalancing:")
    print(f"  Rebalance cycles: {metrics['rebalance_cycles']}")

    print(f"\nSampling:")
    print(f"  Utilization samples: {metrics['total_samples']}")

    if metrics['avg_task_wait_time'] > 0:
        print(f"  Average wait time: {metrics['avg_task_wait_time']:.3f}s")

    # Agent-level details
    print("\nPer-Agent Status:")
    status = scheduler.get_agent_status()
    for agent_id, info in status.items():
        print(f"  {agent_id}:")
        print(f"    Queue length: {info['queue_length']}")
        print(f"    Success rate: {info['success_rate']:.1%}")
        print(f"    Completed: {info['total_completed']}")

    scheduler.stop()
    print("\n[OK] Demo 5 complete\n")


def main():
    """Run all demonstrations."""
    print("\n" + "=" * 60)
    print("Work-Stealing Load Balancer Demonstration")
    print("=" * 60 + "\n")

    demos = [
        ("Basic Usage", demo_basic_usage),
        ("Hot Spot Handling", demo_hot_spot),
        ("Cascading Failure Recovery", demo_cascading_failure),
        ("Priority Task Handling", demo_priority_handling),
        ("Metrics Tracking", demo_metrics_tracking),
    ]

    for name, demo_func in demos:
        try:
            demo_func()
            time.sleep(0.5)
        except Exception as e:
            print(f"\n[X] Error in {name}: {e}")
            import traceback
            traceback.print_exc()

    print("=" * 60)
    print("All demonstrations complete!")
    print("=" * 60)


if __name__ == "__main__":
    main()
