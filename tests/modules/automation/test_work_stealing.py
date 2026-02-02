"""ABOUTME: Comprehensive tests for work-stealing load balancer.
Tests hot spot, uniform load, cascading failure scenarios."""

import pytest
import time
import random
from collections import defaultdict
from typing import List

from digitalmodel.automation.work_stealing_scheduler import (
    WorkStealingScheduler,
    Agent,
    Task,
    WorkStealingMetrics
)


@pytest.fixture
def scheduler():
    """Create scheduler instance with test config."""
    sched = WorkStealingScheduler()
    sched.start()
    yield sched
    sched.stop()


@pytest.fixture
def agents():
    """Create 10 test agents with varying capabilities."""
    agents_list = [
        Agent(agent_id=f"agent_{i}", capabilities=["python", "testing"])
        for i in range(5)
    ]
    agents_list.extend([
        Agent(agent_id=f"agent_{i}", capabilities=["python", "data_analysis"])
        for i in range(5, 8)
    ])
    agents_list.extend([
        Agent(agent_id=f"agent_{i}", capabilities=["javascript", "frontend"])
        for i in range(8, 10)
    ])
    return agents_list


@pytest.fixture
def setup_scheduler_with_agents(scheduler, agents):
    """Register all agents with scheduler."""
    for agent in agents:
        scheduler.register_agent(agent)
    return scheduler, agents


class TestBasicOperations:
    """Test basic scheduler operations."""

    def test_agent_registration(self, scheduler):
        """Test agent registration and unregistration."""
        agent = Agent(agent_id="test_agent", capabilities=["python"])
        scheduler.register_agent(agent)

        assert "test_agent" in scheduler.agents
        assert len(scheduler.agents) == 1

        # Unregister
        tasks = scheduler.unregister_agent("test_agent")
        assert "test_agent" not in scheduler.agents
        assert tasks == []

    def test_task_submission(self, scheduler, agents):
        """Test task submission and assignment."""
        agent = agents[0]
        scheduler.register_agent(agent)

        task = Task(
            task_id="task_1",
            skill_requirements=["python"],
            priority=3
        )

        result = scheduler.submit_task(task)
        assert result is True
        assert agent.queue_length == 1

    def test_capability_matching(self, scheduler, agents):
        """Test strict and fallback capability matching."""
        # Register agents with different capabilities
        scheduler.register_agent(agents[0])  # python, testing
        scheduler.register_agent(agents[8])  # javascript, frontend

        # High priority task requires exact match
        high_priority_task = Task(
            task_id="hp_task",
            skill_requirements=["python", "testing"],
            priority=5
        )

        scheduler.submit_task(high_priority_task)
        assert agents[0].queue_length == 1  # Should go to agent_0

        # Normal priority with partial match
        normal_task = Task(
            task_id="normal_task",
            skill_requirements=["python"],
            priority=2
        )

        scheduler.submit_task(normal_task)
        assert agents[0].queue_length == 2  # Should also go to agent_0


class TestWorkStealing:
    """Test work stealing mechanisms."""

    def test_pull_based_stealing(self, setup_scheduler_with_agents):
        """Test idle agent pulling work from busy agent."""
        scheduler, agents = setup_scheduler_with_agents

        # Overload one agent
        busy_agent = agents[0]
        for i in range(10):
            task = Task(
                task_id=f"task_{i}",
                skill_requirements=["python"],
                priority=2
            )
            busy_agent.enqueue_task(task)

        assert busy_agent.queue_length == 10

        # Idle agent requests work
        idle_agent = agents[1]
        stolen_task = scheduler.request_work(idle_agent.agent_id)

        assert stolen_task is not None
        assert idle_agent.queue_length == 1
        assert busy_agent.queue_length == 9
        assert scheduler.metrics.successful_steals == 1

    def test_high_priority_protection(self, setup_scheduler_with_agents):
        """Test that high-priority tasks are not stolen."""
        scheduler, agents = setup_scheduler_with_agents

        busy_agent = agents[0]

        # Add high priority tasks
        for i in range(5):
            task = Task(
                task_id=f"hp_task_{i}",
                skill_requirements=["python"],
                priority=5  # High priority
            )
            busy_agent.enqueue_task(task)

        # Try to steal
        idle_agent = agents[1]
        stolen_task = scheduler.request_work(idle_agent.agent_id)

        # Should fail because all tasks are high priority
        assert stolen_task is None
        assert busy_agent.queue_length == 5

    def test_priority_ordering(self, scheduler, agents):
        """Test that tasks maintain priority ordering in queue."""
        scheduler.register_agent(agents[0])

        # Submit tasks with different priorities
        priorities = [1, 5, 3, 2, 4]
        for i, priority in enumerate(priorities):
            task = Task(
                task_id=f"task_{i}",
                skill_requirements=["python"],
                priority=priority
            )
            scheduler.submit_task(task)

        # Check queue is sorted by priority
        agent = agents[0]
        queue_priorities = [task.priority for task in agent.task_queue]
        assert queue_priorities == [5, 4, 3, 2, 1]


class TestLoadScenarios:
    """Test different load scenarios."""

    def test_uniform_load(self, setup_scheduler_with_agents):
        """Test uniform load distribution."""
        scheduler, agents = setup_scheduler_with_agents

        # Submit 100 tasks uniformly
        for i in range(100):
            task = Task(
                task_id=f"task_{i}",
                skill_requirements=["python"],
                priority=random.randint(1, 3)
            )
            scheduler.submit_task(task)

        # Check distribution
        queue_lengths = [agent.queue_length for agent in agents[:5]]  # Python agents
        avg_queue = sum(queue_lengths) / len(queue_lengths)

        # Should be relatively balanced (within 50% of average)
        for length in queue_lengths:
            assert abs(length - avg_queue) <= avg_queue * 0.5

    def test_hot_spot_scenario(self, setup_scheduler_with_agents):
        """Test hot spot: one agent heavily loaded."""
        scheduler, agents = setup_scheduler_with_agents

        # Heavily load one agent
        hot_agent = agents[0]
        for i in range(20):
            task = Task(
                task_id=f"task_{i}",
                skill_requirements=["python"],
                priority=2
            )
            hot_agent.enqueue_task(task)

        assert hot_agent.queue_length == 20

        # Trigger rebalancing
        time.sleep(0.5)
        scheduler._rebalance_push()

        # Check load redistribution
        assert hot_agent.queue_length < 20
        total_queued = sum(agent.queue_length for agent in agents)
        assert total_queued == 20  # No tasks lost

    def test_cascading_failure(self, setup_scheduler_with_agents):
        """Test agent failure and task redistribution."""
        scheduler, agents = setup_scheduler_with_agents

        # Load multiple agents
        for i in range(30):
            task = Task(
                task_id=f"task_{i}",
                skill_requirements=["python"],
                priority=2
            )
            scheduler.submit_task(task)

        # Simulate agent failure
        failed_agent = agents[2]
        remaining_tasks = scheduler.unregister_agent(failed_agent.agent_id)

        # Reassign tasks
        for task in remaining_tasks:
            scheduler.submit_task(task)

        # Verify all tasks reassigned
        total_queued = sum(agent.queue_length for agent in scheduler.agents.values())
        assert total_queued == 30
        assert failed_agent.agent_id not in scheduler.agents

    def test_bursty_traffic(self, setup_scheduler_with_agents):
        """Test handling of bursty task arrivals."""
        scheduler, agents = setup_scheduler_with_agents

        # Burst 1: 50 tasks rapidly
        start_time = time.time()
        for i in range(50):
            task = Task(
                task_id=f"burst1_task_{i}",
                skill_requirements=["python"],
                priority=random.randint(1, 5)
            )
            scheduler.submit_task(task)

        burst1_time = time.time() - start_time

        # Small delay
        time.sleep(0.1)

        # Burst 2: 50 more tasks
        for i in range(50):
            task = Task(
                task_id=f"burst2_task_{i}",
                skill_requirements=["python"],
                priority=random.randint(1, 5)
            )
            scheduler.submit_task(task)

        # Verify all tasks accepted
        total_queued = sum(agent.queue_length for agent in agents)
        assert total_queued == 100

        # Verify burst handled quickly (< 1 second)
        assert burst1_time < 1.0


class TestMetrics:
    """Test metrics tracking."""

    def test_steal_metrics(self, setup_scheduler_with_agents):
        """Test work stealing metrics tracking."""
        scheduler, agents = setup_scheduler_with_agents

        # Setup imbalanced load
        busy_agent = agents[0]
        for i in range(15):
            task = Task(
                task_id=f"task_{i}",
                skill_requirements=["python"],
                priority=2
            )
            busy_agent.enqueue_task(task)

        # Perform steals
        for _ in range(3):
            stolen = scheduler.request_work(agents[1].agent_id)

        metrics = scheduler.get_metrics()
        assert metrics['total_steals'] >= 3
        assert metrics['successful_steals'] >= 1
        assert metrics['tasks_migrated'] >= 1

    def test_utilization_sampling(self, setup_scheduler_with_agents):
        """Test utilization sampling over time."""
        scheduler, agents = setup_scheduler_with_agents

        # Submit tasks
        for i in range(50):
            task = Task(
                task_id=f"task_{i}",
                skill_requirements=["python"],
                priority=2
            )
            scheduler.submit_task(task)

        # Wait for samples
        time.sleep(0.2)

        metrics = scheduler.get_metrics()
        assert metrics['total_samples'] > 0
        assert len(scheduler.metrics.utilization_samples) > 0

    def test_wait_time_tracking(self, scheduler, agents):
        """Test task wait time tracking."""
        scheduler.register_agent(agents[0])

        task = Task(
            task_id="test_task",
            skill_requirements=["python"],
            priority=3
        )

        # Submit and simulate wait
        scheduler.submit_task(task)
        time.sleep(0.1)

        # Mark as started
        task.started_at = time.time()
        wait_time = task.wait_time

        assert wait_time >= 0.1

        # Record metrics
        scheduler.metrics.record_task_wait(wait_time)
        assert len(scheduler.metrics.task_wait_times) == 1


class TestPerformance:
    """Test scheduler performance under load."""

    def test_high_throughput(self, setup_scheduler_with_agents):
        """Test handling 10 tasks/second for 10 seconds."""
        scheduler, agents = setup_scheduler_with_agents

        total_tasks = 100
        duration = 10  # seconds
        tasks_per_second = total_tasks / duration

        start_time = time.time()

        for i in range(total_tasks):
            task = Task(
                task_id=f"task_{i}",
                skill_requirements=random.choice([
                    ["python"],
                    ["javascript"],
                    ["data_analysis"]
                ]),
                priority=random.randint(1, 5)
            )
            scheduler.submit_task(task)

            # Maintain ~10 tasks/second
            if (i + 1) % 10 == 0:
                time.sleep(1.0)

        elapsed = time.time() - start_time

        # Verify throughput
        actual_throughput = total_tasks / elapsed
        assert actual_throughput >= 8  # Allow some variance

        # Verify all tasks submitted
        total_queued = sum(agent.queue_length for agent in agents)
        assert total_queued == total_tasks

    def test_rebalance_efficiency(self, setup_scheduler_with_agents):
        """Test rebalancing reduces variance in queue lengths."""
        scheduler, agents = setup_scheduler_with_agents

        # Create severe imbalance
        for i in range(30):
            agents[0].enqueue_task(Task(
                task_id=f"task_{i}",
                skill_requirements=["python"],
                priority=2
            ))

        # Calculate initial variance
        queue_lengths_before = [agent.queue_length for agent in agents[:5]]
        variance_before = sum((x - sum(queue_lengths_before)/5)**2 for x in queue_lengths_before) / 5

        # Trigger rebalance
        scheduler._rebalance_push()

        # Calculate new variance
        queue_lengths_after = [agent.queue_length for agent in agents[:5]]
        variance_after = sum((x - sum(queue_lengths_after)/5)**2 for x in queue_lengths_after) / 5

        # Variance should decrease
        assert variance_after < variance_before


class TestEdgeCases:
    """Test edge cases and error handling."""

    def test_no_agents(self, scheduler):
        """Test task submission with no agents."""
        task = Task(
            task_id="orphan_task",
            skill_requirements=["python"],
            priority=3
        )

        result = scheduler.submit_task(task)
        assert result is False

    def test_no_capable_agents(self, scheduler, agents):
        """Test task with no capable agents."""
        # Register only JavaScript agents
        scheduler.register_agent(agents[8])
        scheduler.register_agent(agents[9])

        # Submit Python task
        task = Task(
            task_id="mismatch_task",
            skill_requirements=["python", "data_analysis"],
            priority=5
        )

        result = scheduler.submit_task(task)
        assert result is False

    def test_single_agent(self, scheduler, agents):
        """Test system with single agent."""
        scheduler.register_agent(agents[0])

        # Submit multiple tasks
        for i in range(10):
            task = Task(
                task_id=f"task_{i}",
                skill_requirements=["python"],
                priority=2
            )
            scheduler.submit_task(task)

        # No rebalancing should occur
        scheduler._rebalance_push()
        assert agents[0].queue_length == 10

    def test_empty_capabilities(self, scheduler):
        """Test agent with empty capabilities."""
        agent = Agent(agent_id="empty_agent", capabilities=[])
        scheduler.register_agent(agent)

        # Task with no requirements should still work
        task = Task(
            task_id="no_req_task",
            skill_requirements=[],
            priority=2
        )

        result = scheduler.submit_task(task)
        assert result is True
        assert agent.queue_length == 1


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])
