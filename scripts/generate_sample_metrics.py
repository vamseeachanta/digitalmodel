#!/usr/bin/env python3
# ABOUTME: Generate sample agent metrics CSV for dashboard demonstration
# Creates realistic 30-day sample data for all 54 agents
"""
Sample Agent Metrics Generator

Creates realistic CSV data for agent health monitoring dashboard testing.
Generates 30 days of metrics for all 54 agents with realistic patterns.
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from pathlib import Path
import random


# All 54 agents from CLAUDE.md
ALL_AGENTS = [
    # Core Development
    "coder", "reviewer", "tester", "planner", "researcher",
    # Swarm Coordination
    "hierarchical-coordinator", "mesh-coordinator", "adaptive-coordinator",
    "collective-intelligence-coordinator", "swarm-memory-manager",
    # Consensus & Distributed
    "byzantine-coordinator", "raft-manager", "gossip-coordinator",
    "consensus-builder", "crdt-synchronizer", "quorum-manager", "security-manager",
    # Performance & Optimization
    "perf-analyzer", "performance-benchmarker", "task-orchestrator",
    "memory-coordinator", "smart-agent",
    # GitHub & Repository
    "github-modes", "pr-manager", "code-review-swarm", "issue-tracker",
    "release-manager", "workflow-automation", "project-board-sync",
    "repo-architect", "multi-repo-swarm",
    # SPARC Methodology
    "sparc-coord", "sparc-coder", "specification", "pseudocode",
    "architecture", "refinement",
    # Specialized Development
    "backend-dev", "mobile-dev", "ml-developer", "cicd-engineer",
    "api-docs", "system-architect", "code-analyzer", "base-template-generator",
    # Testing & Validation
    "tdd-london-swarm", "production-validator",
    # Migration & Planning
    "migration-planner", "swarm-init"
]

TASK_TYPES = [
    "code_generation", "code_review", "testing", "analysis",
    "refactoring", "documentation", "debugging", "optimization",
    "architecture", "planning", "research", "deployment"
]

ERROR_TYPES = [
    "timeout", "syntax_error", "runtime_error", "resource_limit",
    "validation_error", "network_error", "dependency_error", None
]


def generate_sample_metrics(output_dir: Path, days: int = 30, tasks_per_day_range: tuple = (50, 150)):
    """
    Generate sample agent metrics CSV

    Args:
        output_dir: Output directory for CSV
        days: Number of days of historical data
        tasks_per_day_range: Min/max tasks per day
    """
    records = []

    # Generate data for each day
    end_date = datetime.now()
    start_date = end_date - timedelta(days=days)

    # Agent success rate profiles (some agents more reliable than others)
    agent_profiles = {}
    for agent in ALL_AGENTS:
        # Most agents 85-98% success, some "problem" agents 60-80%
        if random.random() < 0.1:  # 10% are problematic
            base_success = random.uniform(0.6, 0.8)
        else:
            base_success = random.uniform(0.85, 0.98)

        agent_profiles[agent] = {
            'base_success': base_success,
            'avg_duration': random.uniform(500, 5000),  # ms
            'avg_tokens': random.randint(500, 3000),
            'cost_per_token': random.uniform(0.00001, 0.00005)  # USD
        }

    current_date = start_date
    while current_date <= end_date:
        # Variable task count per day
        tasks_today = random.randint(*tasks_per_day_range)

        for _ in range(tasks_today):
            # Select random agent (weighted towards core agents)
            if random.random() < 0.6:
                agent = random.choice(ALL_AGENTS[:5])  # Core agents used more
            else:
                agent = random.choice(ALL_AGENTS)

            profile = agent_profiles[agent]

            # Generate timestamp (random time during day)
            timestamp = current_date + timedelta(
                hours=random.randint(0, 23),
                minutes=random.randint(0, 59),
                seconds=random.randint(0, 59)
            )

            # Success based on agent profile with some randomness
            success = random.random() < profile['base_success']

            # Task type
            task_type = random.choice(TASK_TYPES)

            # Duration (longer if failed)
            if success:
                duration = abs(np.random.normal(profile['avg_duration'], profile['avg_duration'] * 0.3))
            else:
                duration = abs(np.random.normal(profile['avg_duration'] * 1.5, profile['avg_duration'] * 0.4))

            # Tokens
            tokens = abs(int(np.random.normal(profile['avg_tokens'], profile['avg_tokens'] * 0.3)))

            # Cost
            cost = tokens * profile['cost_per_token']

            # Error type (only if failed)
            error_type = None if success else random.choice([e for e in ERROR_TYPES if e is not None])

            records.append({
                'timestamp': timestamp.strftime('%Y-%m-%d %H:%M:%S'),
                'agent_name': agent,
                'task_type': task_type,
                'success': success,
                'duration_ms': round(duration, 2),
                'tokens_used': tokens,
                'cost_usd': round(cost, 6),
                'error_type': error_type or ''
            })

        current_date += timedelta(days=1)

    # Create DataFrame and save
    df = pd.DataFrame(records)
    df = df.sort_values('timestamp').reset_index(drop=True)

    output_dir.mkdir(parents=True, exist_ok=True)
    output_path = output_dir / "agent_metrics.csv"
    df.to_csv(output_path, index=False)

    print(f"Generated {len(df):,} sample metrics records")
    print(f"Date range: {df['timestamp'].min()} to {df['timestamp'].max()}")
    print(f"Agents: {df['agent_name'].nunique()}")
    print(f"Total cost: ${df['cost_usd'].sum():.2f}")
    print(f"Success rate: {df['success'].mean()*100:.1f}%")
    print(f"Saved to: {output_path}")

    return output_path


if __name__ == "__main__":
    # Generate sample data
    output_dir = Path("data/agent_metrics")
    generate_sample_metrics(output_dir, days=30, tasks_per_day_range=(80, 200))
