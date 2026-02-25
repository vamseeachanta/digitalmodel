"""ABOUTME: Work-stealing load balancer for task distribution across agents.
Implements hybrid strategy: pull-based with push intervention for severe imbalance."""

import time
import threading
import logging
from typing import List, Dict, Optional, Set, Any, Tuple
from dataclasses import dataclass, field
from collections import defaultdict, deque
from datetime import datetime
import yaml
from pathlib import Path


logger = logging.getLogger(__name__)


@dataclass
class Task:
    """Represents a task to be executed by an agent."""
    task_id: str
    skill_requirements: List[str]
    priority: int  # 1-5, where 5 is highest
    dependencies: List[str] = field(default_factory=list)
    created_at: float = field(default_factory=time.time)
    assigned_at: Optional[float] = None
    started_at: Optional[float] = None
    completed_at: Optional[float] = None
    data: Dict[str, Any] = field(default_factory=dict)

    @property
    def is_high_priority(self) -> bool:
        """Check if task is high priority (>= 4)."""
        return self.priority >= 4

    @property
    def wait_time(self) -> float:
        """Calculate time spent waiting in queue."""
        end_time = self.started_at if self.started_at else time.time()
        return end_time - self.created_at

    def __lt__(self, other):
        """Compare tasks by priority (higher priority first)."""
        return self.priority > other.priority


@dataclass
class Agent:
    """Represents an agent that can execute tasks."""
    agent_id: str
    capabilities: List[str]
    task_queue: deque = field(default_factory=deque)
    current_task: Optional[Task] = None
    performance_history: Dict[str, float] = field(default_factory=dict)
    total_completed: int = 0
    total_failed: int = 0

    @property
    def queue_length(self) -> int:
        """Get number of queued tasks (excluding current task)."""
        return len(self.task_queue)

    @property
    def is_idle(self) -> bool:
        """Check if agent is completely idle."""
        return self.current_task is None and self.queue_length == 0

    @property
    def success_rate(self) -> float:
        """Calculate task success rate."""
        total = self.total_completed + self.total_failed
        return self.total_completed / total if total > 0 else 1.0

    def can_execute(self, task: Task, strict: bool = False) -> bool:
        """Check if agent can execute task based on capabilities."""
        if strict:
            # Exact match required
            return all(skill in self.capabilities for skill in task.skill_requirements)
        else:
            # Best-effort: at least one matching skill
            if not task.skill_requirements:
                return True
            return any(skill in self.capabilities for skill in task.skill_requirements)

    def get_skill_score(self, task: Task) -> float:
        """Calculate skill match score (0-1) for task assignment."""
        if not task.skill_requirements:
            return 1.0

        matches = sum(1 for skill in task.skill_requirements if skill in self.capabilities)
        return matches / len(task.skill_requirements)

    def enqueue_task(self, task: Task):
        """Add task to queue (maintains priority ordering)."""
        task.assigned_at = time.time()
        # Insert task maintaining priority order
        inserted = False
        for i, queued_task in enumerate(self.task_queue):
            if task.priority > queued_task.priority:
                self.task_queue.insert(i, task)
                inserted = True
                break
        if not inserted:
            self.task_queue.append(task)

    def dequeue_task(self) -> Optional[Task]:
        """Remove and return highest priority task from queue."""
        if self.task_queue:
            return self.task_queue.popleft()
        return None

    def steal_task(self, prefer_recent: bool = True) -> Optional[Task]:
        """Steal a task from this agent's queue (lowest priority first)."""
        if not self.task_queue:
            return None

        # Don't steal high-priority tasks
        stealable_tasks = [t for t in self.task_queue if not t.is_high_priority]
        if not stealable_tasks:
            return None

        if prefer_recent:
            # Steal most recent low-priority task (LIFO)
            for i in range(len(self.task_queue) - 1, -1, -1):
                task = self.task_queue[i]
                if not task.is_high_priority:
                    del self.task_queue[i]
                    return task
        else:
            # Steal oldest low-priority task (FIFO)
            for i, task in enumerate(self.task_queue):
                if not task.is_high_priority:
                    del self.task_queue[i]
                    return task

        return None


@dataclass
class WorkStealingMetrics:
    """Tracks metrics for work stealing operations."""
    total_steals: int = 0
    successful_steals: int = 0
    failed_steals: int = 0
    tasks_migrated: int = 0
    rebalance_cycles: int = 0

    # Agent utilization over time
    utilization_samples: List[Dict[str, int]] = field(default_factory=list)

    # Task wait times
    task_wait_times: List[float] = field(default_factory=list)

    # Timestamp tracking
    last_rebalance: float = field(default_factory=time.time)

    def record_steal_attempt(self, success: bool, tasks_stolen: int = 0):
        """Record a work stealing attempt."""
        self.total_steals += 1
        if success:
            self.successful_steals += 1
            self.tasks_migrated += tasks_stolen
        else:
            self.failed_steals += 1

    def record_utilization_sample(self, agents: List[Agent]):
        """Record current queue lengths for all agents."""
        sample = {
            'timestamp': time.time(),
            'queue_lengths': {agent.agent_id: agent.queue_length for agent in agents},
            'total_queued': sum(agent.queue_length for agent in agents)
        }
        self.utilization_samples.append(sample)

    def record_task_wait(self, wait_time: float):
        """Record task wait time."""
        self.task_wait_times.append(wait_time)

    def get_stats(self) -> Dict[str, Any]:
        """Get comprehensive statistics."""
        avg_wait = sum(self.task_wait_times) / len(self.task_wait_times) if self.task_wait_times else 0

        return {
            'total_steals': self.total_steals,
            'successful_steals': self.successful_steals,
            'failed_steals': self.failed_steals,
            'success_rate': self.successful_steals / self.total_steals if self.total_steals > 0 else 0,
            'tasks_migrated': self.tasks_migrated,
            'rebalance_cycles': self.rebalance_cycles,
            'avg_task_wait_time': avg_wait,
            'total_samples': len(self.utilization_samples)
        }


class WorkStealingScheduler:
    """
    Hybrid work-stealing scheduler for load balancing across agents.

    Features:
    - Pull-based: Idle agents request work from busy agents
    - Push-based: Scheduler intervenes on severe imbalance
    - Capability-aware task matching
    - Priority preservation for high-priority tasks
    - Real-time metrics tracking
    """

    def __init__(self, config_path: Optional[str] = None):
        """Initialize scheduler with configuration."""
        self.agents: Dict[str, Agent] = {}
        self.metrics = WorkStealingMetrics()
        self.config = self._load_config(config_path)
        self.running = False
        self.rebalance_thread: Optional[threading.Thread] = None
        self.lock = threading.RLock()

        # Performance cache
        self.capability_cache: Dict[Tuple[str, str], float] = {}
        self.cache_timestamp = time.time()

        logger.info("WorkStealingScheduler initialized with config: %s", config_path)

    def _load_config(self, config_path: Optional[str] = None) -> Dict[str, Any]:
        """Load configuration from YAML file."""
        if config_path is None:
            # Default to project config
            config_path = Path(__file__).parent.parent.parent.parent.parent / 'config' / 'load-balancing-config.yaml'

        try:
            with open(config_path, 'r') as f:
                config = yaml.safe_load(f)
                return config.get('load_balancing', {})
        except Exception as e:
            logger.warning(f"Failed to load config from {config_path}: {e}. Using defaults.")
            return self._default_config()

    def _default_config(self) -> Dict[str, Any]:
        """Return default configuration."""
        return {
            'strategy': 'hybrid',
            'thresholds': {
                'busy_threshold': 5,
                'idle_threshold': 2,
                'rebalance_interval_sec': 30,
                'severe_imbalance_ratio': 3.0
            },
            'work_stealing': {
                'max_steal_per_cycle': 3,
                'prefer_recent_tasks': True,
                'allow_task_migration': False,
                'min_tasks_to_steal': 1
            },
            'capabilities': {
                'matching_strategy': 'best_fit',
                'consider_history': True,
                'fallback_enabled': True,
                'history_weight': 0.3
            },
            'priority': {
                'high_priority_threshold': 4,
                'preserve_high_priority': True,
                'priority_stealing_order': True
            },
            'metrics': {
                'track_migrations': True,
                'track_utilization': True,
                'track_wait_times': True,
                'utilization_sample_interval_sec': 5
            },
            'performance': {
                'cache_capability_matches': True,
                'cache_ttl_sec': 300
            }
        }

    def register_agent(self, agent: Agent):
        """Register an agent with the scheduler."""
        with self.lock:
            self.agents[agent.agent_id] = agent
            logger.info(f"Registered agent {agent.agent_id} with capabilities: {agent.capabilities}")

    def unregister_agent(self, agent_id: str) -> Optional[List[Task]]:
        """Unregister agent and return its remaining tasks."""
        with self.lock:
            agent = self.agents.pop(agent_id, None)
            if agent:
                remaining_tasks = list(agent.task_queue)
                if agent.current_task:
                    remaining_tasks.insert(0, agent.current_task)
                logger.info(f"Unregistered agent {agent_id}, returning {len(remaining_tasks)} tasks")
                return remaining_tasks
            return None

    def submit_task(self, task: Task) -> bool:
        """Submit task to scheduler for assignment."""
        with self.lock:
            # Find best agent for task
            best_agent = self._find_best_agent(task)

            if best_agent is None:
                logger.error(f"No suitable agent found for task {task.task_id}")
                return False

            best_agent.enqueue_task(task)
            logger.debug(f"Task {task.task_id} assigned to agent {best_agent.agent_id}")

            # Record metrics
            if self.config['metrics']['track_utilization']:
                self.metrics.record_utilization_sample(list(self.agents.values()))

            return True

    def _find_best_agent(self, task: Task) -> Optional[Agent]:
        """Find best agent for task based on capabilities and load."""
        if not self.agents:
            return None

        strict_match = task.is_high_priority
        consider_history = self.config['capabilities']['consider_history']
        history_weight = self.config['capabilities']['history_weight']

        # Filter capable agents
        capable_agents = [
            agent for agent in self.agents.values()
            if agent.can_execute(task, strict=strict_match)
        ]

        if not capable_agents:
            # Try fallback for normal priority tasks
            if not strict_match and self.config['capabilities']['fallback_enabled']:
                capable_agents = [
                    agent for agent in self.agents.values()
                    if agent.can_execute(task, strict=False)
                ]

        if not capable_agents:
            return None

        # Score agents based on skill match, load, and performance
        def score_agent(agent: Agent) -> float:
            skill_score = agent.get_skill_score(task)
            load_penalty = agent.queue_length / 10.0  # Normalize queue length

            if consider_history:
                history_bonus = agent.success_rate * history_weight
                return skill_score + history_bonus - load_penalty
            else:
                return skill_score - load_penalty

        # Return agent with highest score
        return max(capable_agents, key=score_agent)

    def request_work(self, requesting_agent_id: str) -> Optional[Task]:
        """Pull-based: Idle agent requests work from busy agents."""
        with self.lock:
            requesting_agent = self.agents.get(requesting_agent_id)
            if not requesting_agent:
                return None

            # Find busiest agent with stealable tasks
            busy_agents = self._get_busy_agents()
            if not busy_agents:
                return None

            # Sort by queue length (descending)
            busy_agents.sort(key=lambda a: a.queue_length, reverse=True)

            # Try to steal from busiest agents
            prefer_recent = self.config['work_stealing']['prefer_recent_tasks']

            for busy_agent in busy_agents:
                stolen_task = busy_agent.steal_task(prefer_recent=prefer_recent)
                if stolen_task:
                    # Verify requesting agent can execute
                    if requesting_agent.can_execute(stolen_task, strict=False):
                        requesting_agent.enqueue_task(stolen_task)
                        self.metrics.record_steal_attempt(success=True, tasks_stolen=1)
                        logger.info(f"Agent {requesting_agent_id} stole task {stolen_task.task_id} from {busy_agent.agent_id}")
                        return stolen_task
                    else:
                        # Put it back
                        busy_agent.enqueue_task(stolen_task)

            self.metrics.record_steal_attempt(success=False)
            return None

    def _get_busy_agents(self) -> List[Agent]:
        """Get agents exceeding busy threshold."""
        busy_threshold = self.config['thresholds']['busy_threshold']
        return [agent for agent in self.agents.values() if agent.queue_length > busy_threshold]

    def _get_idle_agents(self) -> List[Agent]:
        """Get agents below idle threshold."""
        idle_threshold = self.config['thresholds']['idle_threshold']
        return [agent for agent in self.agents.values() if agent.queue_length < idle_threshold]

    def _check_severe_imbalance(self) -> bool:
        """Check if there's severe load imbalance requiring intervention."""
        if len(self.agents) < 2:
            return False

        queue_lengths = [agent.queue_length for agent in self.agents.values()]
        max_queue = max(queue_lengths)
        min_queue = min(queue_lengths)

        if min_queue == 0:
            return max_queue > self.config['thresholds']['busy_threshold']

        ratio = max_queue / min_queue
        return ratio > self.config['thresholds']['severe_imbalance_ratio']

    def _rebalance_push(self):
        """Push-based: Scheduler intervenes to rebalance load."""
        with self.lock:
            if not self._check_severe_imbalance():
                return

            busy_agents = self._get_busy_agents()
            idle_agents = self._get_idle_agents()

            if not busy_agents or not idle_agents:
                return

            max_steal = self.config['work_stealing']['max_steal_per_cycle']
            prefer_recent = self.config['work_stealing']['prefer_recent_tasks']

            # Sort agents
            busy_agents.sort(key=lambda a: a.queue_length, reverse=True)
            idle_agents.sort(key=lambda a: a.queue_length)

            tasks_stolen = 0

            for busy_agent in busy_agents:
                if tasks_stolen >= max_steal:
                    break

                for idle_agent in idle_agents:
                    if tasks_stolen >= max_steal:
                        break

                    stolen_task = busy_agent.steal_task(prefer_recent=prefer_recent)
                    if stolen_task and idle_agent.can_execute(stolen_task, strict=False):
                        idle_agent.enqueue_task(stolen_task)
                        tasks_stolen += 1
                        logger.info(f"Rebalance: moved task {stolen_task.task_id} from {busy_agent.agent_id} to {idle_agent.agent_id}")
                    elif stolen_task:
                        # Put it back if idle agent can't execute
                        busy_agent.enqueue_task(stolen_task)

            if tasks_stolen > 0:
                self.metrics.record_steal_attempt(success=True, tasks_stolen=tasks_stolen)
                self.metrics.rebalance_cycles += 1
                logger.info(f"Rebalance cycle completed: {tasks_stolen} tasks migrated")

    def _rebalance_loop(self):
        """Background thread for periodic rebalancing."""
        interval = self.config['thresholds']['rebalance_interval_sec']

        while self.running:
            time.sleep(interval)

            if not self.running:
                break

            try:
                self._rebalance_push()

                # Sample utilization
                if self.config['metrics']['track_utilization']:
                    with self.lock:
                        self.metrics.record_utilization_sample(list(self.agents.values()))

            except Exception as e:
                logger.error(f"Error in rebalance loop: {e}", exc_info=True)

    def start(self):
        """Start background rebalancing thread."""
        if self.running:
            logger.warning("Scheduler already running")
            return

        self.running = True
        self.rebalance_thread = threading.Thread(target=self._rebalance_loop, daemon=True)
        self.rebalance_thread.start()
        logger.info("WorkStealingScheduler started")

    def stop(self):
        """Stop background rebalancing thread."""
        if not self.running:
            return

        self.running = False
        if self.rebalance_thread:
            self.rebalance_thread.join(timeout=5)
        logger.info("WorkStealingScheduler stopped")

    def get_metrics(self) -> Dict[str, Any]:
        """Get comprehensive metrics."""
        with self.lock:
            stats = self.metrics.get_stats()
            stats['agent_count'] = len(self.agents)
            stats['total_queued'] = sum(agent.queue_length for agent in self.agents.values())
            stats['busy_agents'] = len(self._get_busy_agents())
            stats['idle_agents'] = len(self._get_idle_agents())
            return stats

    def get_agent_status(self) -> Dict[str, Dict[str, Any]]:
        """Get status of all agents."""
        with self.lock:
            return {
                agent_id: {
                    'queue_length': agent.queue_length,
                    'capabilities': agent.capabilities,
                    'success_rate': agent.success_rate,
                    'total_completed': agent.total_completed,
                    'has_current_task': agent.current_task is not None
                }
                for agent_id, agent in self.agents.items()
            }
