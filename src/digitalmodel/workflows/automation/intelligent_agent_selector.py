"""
ABOUTME: Intelligent agent selection system with multi-factor scoring algorithm.
Selects optimal agents based on historical performance, availability, cost, and domain expertise.
"""

import json
import sqlite3
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Optional, Tuple
import yaml


class AgentPerformanceTracker:
    """Tracks and queries agent performance metrics from SQLite database."""

    def __init__(self, db_path: str = ".claude-flow/agent-performance.db"):
        """Initialize performance tracker with database connection.

        Args:
            db_path: Path to SQLite database file
        """
        self.db_path = Path(db_path)
        self.db_path.parent.mkdir(parents=True, exist_ok=True)
        self._init_database()

    def _init_database(self):
        """Initialize database schema if not exists."""
        with sqlite3.connect(self.db_path) as conn:
            conn.execute("""
                CREATE TABLE IF NOT EXISTS agent_tasks (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    agent_type TEXT NOT NULL,
                    task_type TEXT NOT NULL,
                    started_at TIMESTAMP NOT NULL,
                    completed_at TIMESTAMP,
                    success BOOLEAN NOT NULL,
                    execution_time REAL,
                    error_count INTEGER DEFAULT 0,
                    quality_score REAL DEFAULT 1.0
                )
            """)
            conn.execute("""
                CREATE INDEX IF NOT EXISTS idx_agent_type
                ON agent_tasks(agent_type)
            """)
            conn.execute("""
                CREATE INDEX IF NOT EXISTS idx_started_at
                ON agent_tasks(started_at)
            """)
            conn.commit()

    def get_success_rate(self, agent_type: str, days: int = 30) -> float:
        """Calculate success rate for agent over specified time window.

        Args:
            agent_type: Type of agent to query
            days: Number of days to look back

        Returns:
            Success rate as float between 0.0 and 1.0
        """
        cutoff_date = datetime.now() - timedelta(days=days)

        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.execute("""
                SELECT
                    COUNT(*) as total_tasks,
                    SUM(CASE WHEN success = 1 THEN 1 ELSE 0 END) as success_count
                FROM agent_tasks
                WHERE agent_type = ? AND started_at >= ?
            """, (agent_type, cutoff_date))

            row = cursor.fetchone()
            total_tasks, success_count = row[0], row[1]

            if total_tasks == 0:
                return 0.5  # Default neutral score for new agents

            return success_count / total_tasks

    def get_avg_execution_time(self, agent_type: str, days: int = 30) -> float:
        """Get average execution time for successful tasks.

        Args:
            agent_type: Type of agent to query
            days: Number of days to look back

        Returns:
            Average execution time in seconds, or 60.0 if no data
        """
        cutoff_date = datetime.now() - timedelta(days=days)

        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.execute("""
                SELECT AVG(execution_time)
                FROM agent_tasks
                WHERE agent_type = ?
                  AND started_at >= ?
                  AND success = 1
                  AND execution_time IS NOT NULL
            """, (agent_type, cutoff_date))

            avg_time = cursor.fetchone()[0]
            return avg_time if avg_time is not None else 60.0  # Default 60s

    def record_task(
        self,
        agent_type: str,
        task_type: str,
        success: bool,
        execution_time: Optional[float] = None,
        error_count: int = 0,
        quality_score: float = 1.0
    ) -> int:
        """Record a completed task for an agent.

        Args:
            agent_type: Type of agent that performed task
            task_type: Type of task performed
            success: Whether task completed successfully
            execution_time: Time taken in seconds (optional)
            error_count: Number of errors encountered
            quality_score: Quality score between 0.0 and 1.0

        Returns:
            ID of inserted record
        """
        now = datetime.now()

        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.execute("""
                INSERT INTO agent_tasks (
                    agent_type, task_type, started_at, completed_at,
                    success, execution_time, error_count, quality_score
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
            """, (
                agent_type, task_type, now, now,
                success, execution_time, error_count, quality_score
            ))
            conn.commit()
            return cursor.lastrowid


class IntelligentAgentSelector:
    """Multi-factor agent selection with configurable weights."""

    def __init__(
        self,
        config_path: str = "config/agent-selection-weights.yaml",
        registry_path: str = "config/ai-agents-registry.json",
        performance_db: str = ".claude-flow/agent-performance.db"
    ):
        """Initialize agent selector.

        Args:
            config_path: Path to weights configuration YAML
            registry_path: Path to agent registry JSON
            performance_db: Path to performance tracking database
        """
        self.config_path = Path(config_path)
        self.registry_path = Path(registry_path)
        self.tracker = AgentPerformanceTracker(performance_db)
        self.weights = self._load_weights()
        self.registry = self._load_registry()

    def _load_weights(self) -> Dict[str, float]:
        """Load selection weights from configuration file.

        Returns:
            Dictionary of factor weights
        """
        if not self.config_path.exists():
            # Return default weights
            return {
                "historical_success": 0.70,
                "availability": 0.15,
                "cost_benefit": 0.10,
                "domain_expertise": 0.05
            }

        with open(self.config_path) as f:
            config = yaml.safe_load(f)
            return config.get("weights", {})

    def _load_registry(self) -> Dict:
        """Load agent registry data.

        Returns:
            Registry dictionary
        """
        if not self.registry_path.exists():
            return {"agents": {}, "taskTypeAgentMapping": {}}

        with open(self.registry_path) as f:
            return json.load(f)

    def _calculate_historical_score(self, agent_type: str) -> float:
        """Calculate historical success rate score.

        Args:
            agent_type: Type of agent

        Returns:
            Score between 0.0 and 1.0
        """
        return self.tracker.get_success_rate(agent_type, days=30)

    def _calculate_availability_score(
        self,
        agent_type: str,
        current_load: Optional[int] = None,
        max_concurrent: int = 5
    ) -> float:
        """Calculate availability score based on current load.

        Args:
            agent_type: Type of agent
            current_load: Current number of tasks (None = available)
            max_concurrent: Maximum concurrent tasks allowed

        Returns:
            Score between 0.0 and 1.0, or -1 if unavailable
        """
        if current_load is None:
            return 1.0  # Fully available

        if current_load >= max_concurrent:
            return -1.0  # Hard constraint: unavailable

        return 1.0 - (current_load / max_concurrent)

    def _calculate_cost_benefit_score(
        self,
        agent_type: str,
        cost_per_task: float = 1.0
    ) -> float:
        """Calculate cost-benefit score.

        Args:
            agent_type: Type of agent
            cost_per_task: Cost per task (default 1.0 for equal costs)

        Returns:
            Score between 0.0 and 1.0
        """
        success_rate = self.tracker.get_success_rate(agent_type, days=30)
        avg_time = self.tracker.get_avg_execution_time(agent_type, days=30)

        # Benefit = success_rate / execution_time
        # Normalized by dividing by cost
        if avg_time == 0:
            return 0.0

        benefit = success_rate / avg_time
        cost_benefit = benefit / cost_per_task

        # Normalize to 0-1 range (assume max benefit of 0.1 = 1.0 success / 10s)
        normalized = min(cost_benefit / 0.1, 1.0)
        return normalized

    def _calculate_domain_expertise_score(
        self,
        agent_type: str,
        task_keywords: List[str]
    ) -> float:
        """Calculate domain expertise match score.

        Args:
            agent_type: Type of agent
            task_keywords: Keywords from task description

        Returns:
            Score between 0.0 and 1.0
        """
        agent_data = self.registry.get("agents", {}).get(agent_type, {})
        capabilities = agent_data.get("capabilities", [])

        if not capabilities or not task_keywords:
            return 0.5  # Neutral score

        # Simple keyword overlap matching
        capability_text = " ".join(capabilities).lower()
        matches = sum(
            1 for keyword in task_keywords
            if keyword.lower() in capability_text
        )

        # Normalize by number of keywords
        score = matches / len(task_keywords) if task_keywords else 0.5
        return min(score, 1.0)

    def _extract_task_keywords(self, task_description: str) -> List[str]:
        """Extract keywords from task description.

        Args:
            task_description: Natural language task description

        Returns:
            List of keywords
        """
        # Simple keyword extraction (can be enhanced)
        import re

        # Remove common words and extract meaningful terms
        stop_words = {
            'a', 'an', 'the', 'and', 'or', 'but', 'in', 'on', 'at',
            'to', 'for', 'of', 'with', 'by', 'from', 'is', 'are', 'was'
        }

        words = re.findall(r'\b\w+\b', task_description.lower())
        keywords = [w for w in words if w not in stop_words and len(w) > 3]

        return keywords[:10]  # Limit to top 10 keywords

    def calculate_agent_score(
        self,
        agent_type: str,
        task_description: str,
        current_load: Optional[int] = None,
        max_concurrent: int = 5
    ) -> Tuple[float, Dict[str, float]]:
        """Calculate overall weighted score for an agent.

        Args:
            agent_type: Type of agent to score
            task_description: Description of task
            current_load: Current task load (None = available)
            max_concurrent: Maximum concurrent tasks

        Returns:
            Tuple of (total_score, component_scores)
        """
        # Calculate individual scores
        historical = self._calculate_historical_score(agent_type)
        availability = self._calculate_availability_score(
            agent_type, current_load, max_concurrent
        )

        # Hard constraint: exclude unavailable agents
        if availability < 0:
            return -1.0, {}

        cost_benefit = self._calculate_cost_benefit_score(agent_type)

        task_keywords = self._extract_task_keywords(task_description)
        domain = self._calculate_domain_expertise_score(
            agent_type, task_keywords
        )

        # Calculate weighted total
        total_score = (
            self.weights.get("historical_success", 0.70) * historical +
            self.weights.get("availability", 0.15) * availability +
            self.weights.get("cost_benefit", 0.10) * cost_benefit +
            self.weights.get("domain_expertise", 0.05) * domain
        )

        component_scores = {
            "historical_success": historical,
            "availability": availability,
            "cost_benefit": cost_benefit,
            "domain_expertise": domain,
            "total": total_score
        }

        return total_score, component_scores

    def select_agent(
        self,
        task_type: str,
        task_description: str,
        agent_loads: Optional[Dict[str, int]] = None,
        max_concurrent: int = 5
    ) -> Dict:
        """Select best agent for a task.

        Args:
            task_type: Type of task to perform
            task_description: Natural language description
            agent_loads: Current load per agent type (optional)
            max_concurrent: Maximum concurrent tasks per agent

        Returns:
            Dictionary with selected agent and metadata:
            {
                "agent": agent_type,
                "confidence": score,
                "reasoning": explanation,
                "scores": component_scores,
                "alternatives": [other_options]
            }
        """
        agent_loads = agent_loads or {}

        # Get candidate agents from registry
        task_mapping = self.registry.get("taskTypeAgentMapping", {})
        task_config = task_mapping.get(task_type, {})

        primary_agent = task_config.get("primary")
        alternatives = task_config.get("alternatives", [])

        # Build candidate list
        candidates = []
        if primary_agent:
            candidates.append(primary_agent)
        candidates.extend(alternatives)

        if not candidates:
            # Fallback to all registered agents
            candidates = list(self.registry.get("agents", {}).keys())

        if not candidates:
            return {
                "agent": None,
                "confidence": 0.0,
                "reasoning": "No agents available in registry",
                "scores": {},
                "alternatives": []
            }

        # Score all candidates
        scored_agents = []
        for agent_type in candidates:
            current_load = agent_loads.get(agent_type)
            score, components = self.calculate_agent_score(
                agent_type,
                task_description,
                current_load,
                max_concurrent
            )

            if score >= 0:  # Exclude unavailable agents
                scored_agents.append({
                    "agent": agent_type,
                    "score": score,
                    "components": components
                })

        if not scored_agents:
            return {
                "agent": None,
                "confidence": 0.0,
                "reasoning": "All agents are currently unavailable",
                "scores": {},
                "alternatives": []
            }

        # Sort by score descending
        scored_agents.sort(key=lambda x: x["score"], reverse=True)

        # Select best agent
        best = scored_agents[0]
        alternatives_list = [
            {
                "agent": a["agent"],
                "confidence": round(a["score"], 3)
            }
            for a in scored_agents[1:4]  # Top 3 alternatives
        ]

        # Generate reasoning
        reasoning = self._generate_reasoning(
            best["agent"],
            best["components"],
            task_type
        )

        return {
            "agent": best["agent"],
            "confidence": round(best["score"], 3),
            "reasoning": reasoning,
            "scores": {
                k: round(v, 3) for k, v in best["components"].items()
            },
            "alternatives": alternatives_list
        }

    def _generate_reasoning(
        self,
        agent: str,
        scores: Dict[str, float],
        task_type: str
    ) -> str:
        """Generate human-readable reasoning for selection.

        Args:
            agent: Selected agent type
            scores: Component scores
            task_type: Type of task

        Returns:
            Reasoning explanation string
        """
        parts = [f"Selected {agent} for {task_type}"]

        # Identify strongest factors
        factors = []
        if scores.get("historical_success", 0) > 0.7:
            factors.append(
                f"high success rate ({scores['historical_success']:.1%})"
            )
        if scores.get("availability", 0) > 0.8:
            factors.append("immediately available")
        if scores.get("cost_benefit", 0) > 0.7:
            factors.append("excellent cost-benefit ratio")
        if scores.get("domain_expertise", 0) > 0.6:
            factors.append("strong domain match")

        if factors:
            parts.append(f" based on: {', '.join(factors)}")
        else:
            parts.append(f" (confidence: {scores.get('total', 0):.1%})")

        return "".join(parts)


def main():
    """CLI entry point for agent selection."""
    import argparse
    import sys

    parser = argparse.ArgumentParser(
        description="Intelligent agent selection system"
    )
    parser.add_argument(
        "task_type",
        help="Type of task (e.g., code-generation, code-review)"
    )
    parser.add_argument(
        "description",
        help="Task description"
    )
    parser.add_argument(
        "--config",
        default="config/agent-selection-weights.yaml",
        help="Path to weights configuration"
    )
    parser.add_argument(
        "--registry",
        default="config/ai-agents-registry.json",
        help="Path to agent registry"
    )
    parser.add_argument(
        "--load",
        type=str,
        help="Agent loads as JSON (e.g., '{\"agent1\": 2, \"agent2\": 1}')"
    )
    parser.add_argument(
        "--max-concurrent",
        type=int,
        default=5,
        help="Maximum concurrent tasks per agent"
    )

    args = parser.parse_args()

    # Parse agent loads
    agent_loads = None
    if args.load:
        try:
            agent_loads = json.loads(args.load)
        except json.JSONDecodeError as e:
            print(f"Error parsing --load JSON: {e}", file=sys.stderr)
            sys.exit(1)

    # Initialize selector
    selector = IntelligentAgentSelector(
        config_path=args.config,
        registry_path=args.registry
    )

    # Select agent
    result = selector.select_agent(
        args.task_type,
        args.description,
        agent_loads,
        args.max_concurrent
    )

    # Output result as JSON
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
