#!/usr/bin/env python3
"""
ABOUTME: Workflow result caching and persistence system for storing and
retrieving workflow execution results to avoid redundant computations.
"""

import hashlib
import json
import pickle
from pathlib import Path
from typing import Any, Optional, Dict
from datetime import datetime, timedelta

from .models import WorkflowResult, WorkflowDefinition, WorkflowTask


class WorkflowCache:
    """
    Caching system for workflow execution results

    Provides:
    - Task-level result caching based on input hashing
    - Workflow-level result caching
    - Cache invalidation and expiration
    - Disk-based persistence
    - Memory-efficient storage
    """

    def __init__(
        self,
        cache_dir: Optional[str] = None,
        ttl_hours: int = 24,
        max_cache_size_mb: int = 1000,
    ):
        """
        Initialize workflow cache

        Args:
            cache_dir: Directory for cache storage (default: ./.workflow_cache)
            ttl_hours: Time-to-live for cached results in hours (default: 24)
            max_cache_size_mb: Maximum cache size in MB (default: 1000)
        """
        self.cache_dir = Path(cache_dir) if cache_dir else Path(".workflow_cache")
        self.cache_dir.mkdir(parents=True, exist_ok=True)

        self.ttl_hours = ttl_hours
        self.max_cache_size_mb = max_cache_size_mb

        # In-memory cache for fast access
        self._memory_cache: Dict[str, Any] = {}

        # Task cache directory
        self.task_cache_dir = self.cache_dir / "tasks"
        self.task_cache_dir.mkdir(exist_ok=True)

        # Workflow cache directory
        self.workflow_cache_dir = self.cache_dir / "workflows"
        self.workflow_cache_dir.mkdir(exist_ok=True)

    def _compute_task_hash(self, task: WorkflowTask, inputs: Dict[str, Any]) -> str:
        """
        Compute hash for task + inputs combination

        Args:
            task: WorkflowTask
            inputs: Task input values

        Returns:
            Hash string for caching key
        """
        # Create deterministic representation
        cache_key = {
            'module': task.module,
            'function': task.function,
            'inputs': inputs,
        }

        # Convert to stable JSON string
        json_str = json.dumps(cache_key, sort_keys=True, default=str)

        # Compute SHA256 hash
        return hashlib.sha256(json_str.encode('utf-8')).hexdigest()

    def _compute_workflow_hash(self, workflow: WorkflowDefinition) -> str:
        """
        Compute hash for workflow definition

        Args:
            workflow: WorkflowDefinition

        Returns:
            Hash string for workflow
        """
        # Create workflow representation
        workflow_dict = {
            'name': workflow.name,
            'version': workflow.version,
            'tasks': [
                {
                    'task_id': task.task_id,
                    'module': task.module,
                    'function': task.function,
                    'inputs': task.inputs,
                }
                for task in workflow.tasks
            ]
        }

        json_str = json.dumps(workflow_dict, sort_keys=True, default=str)
        return hashlib.sha256(json_str.encode('utf-8')).hexdigest()

    def get_task_result(
        self,
        task: WorkflowTask,
        inputs: Dict[str, Any]
    ) -> Optional[Any]:
        """
        Retrieve cached task result

        Args:
            task: WorkflowTask
            inputs: Task inputs

        Returns:
            Cached result if available and valid, None otherwise
        """
        cache_key = self._compute_task_hash(task, inputs)

        # Check memory cache first
        if cache_key in self._memory_cache:
            return self._memory_cache[cache_key]

        # Check disk cache
        cache_file = self.task_cache_dir / f"{cache_key}.pkl"

        if not cache_file.exists():
            return None

        # Check cache age
        file_age = datetime.now() - datetime.fromtimestamp(cache_file.stat().st_mtime)
        if file_age > timedelta(hours=self.ttl_hours):
            # Cache expired
            cache_file.unlink()
            return None

        # Load from disk
        try:
            with open(cache_file, 'rb') as f:
                result = pickle.load(f)

            # Store in memory cache for fast access
            self._memory_cache[cache_key] = result

            return result
        except Exception as e:
            # Cache corrupted, remove it
            cache_file.unlink()
            return None

    def set_task_result(
        self,
        task: WorkflowTask,
        inputs: Dict[str, Any],
        result: Any
    ) -> None:
        """
        Store task result in cache

        Args:
            task: WorkflowTask
            inputs: Task inputs
            result: Task result to cache
        """
        if not task.cache_results:
            return

        cache_key = self._compute_task_hash(task, inputs)

        # Store in memory
        self._memory_cache[cache_key] = result

        # Store on disk
        cache_file = self.task_cache_dir / f"{cache_key}.pkl"

        try:
            with open(cache_file, 'wb') as f:
                pickle.dump(result, f)
        except Exception as e:
            # Failed to cache, continue without caching
            pass

        # Check cache size and cleanup if needed
        self._cleanup_if_needed()

    def get_workflow_result(
        self,
        workflow: WorkflowDefinition
    ) -> Optional[WorkflowResult]:
        """
        Retrieve cached workflow result

        Args:
            workflow: WorkflowDefinition

        Returns:
            Cached WorkflowResult if available, None otherwise
        """
        if not workflow.cache_enabled:
            return None

        cache_key = self._compute_workflow_hash(workflow)
        cache_file = self.workflow_cache_dir / f"{cache_key}.json"

        if not cache_file.exists():
            return None

        # Check cache age
        file_age = datetime.now() - datetime.fromtimestamp(cache_file.stat().st_mtime)
        if file_age > timedelta(hours=self.ttl_hours):
            cache_file.unlink()
            return None

        # Load workflow result
        try:
            with open(cache_file, 'r') as f:
                data = json.load(f)

            # Reconstruct WorkflowResult
            # Note: This is simplified - full reconstruction would need more work
            return data
        except Exception:
            cache_file.unlink()
            return None

    def set_workflow_result(
        self,
        workflow: WorkflowDefinition,
        result: WorkflowResult
    ) -> None:
        """
        Store workflow result in cache

        Args:
            workflow: WorkflowDefinition
            result: WorkflowResult to cache
        """
        if not workflow.cache_enabled:
            return

        cache_key = self._compute_workflow_hash(workflow)
        cache_file = self.workflow_cache_dir / f"{cache_key}.json"

        try:
            with open(cache_file, 'w') as f:
                json.dump(result.to_dict(), f, indent=2, default=str)
        except Exception:
            pass

    def invalidate_task(self, task: WorkflowTask, inputs: Dict[str, Any]) -> None:
        """
        Invalidate cached task result

        Args:
            task: WorkflowTask
            inputs: Task inputs
        """
        cache_key = self._compute_task_hash(task, inputs)

        # Remove from memory
        self._memory_cache.pop(cache_key, None)

        # Remove from disk
        cache_file = self.task_cache_dir / f"{cache_key}.pkl"
        if cache_file.exists():
            cache_file.unlink()

    def invalidate_workflow(self, workflow: WorkflowDefinition) -> None:
        """
        Invalidate cached workflow result

        Args:
            workflow: WorkflowDefinition
        """
        cache_key = self._compute_workflow_hash(workflow)
        cache_file = self.workflow_cache_dir / f"{cache_key}.json"

        if cache_file.exists():
            cache_file.unlink()

    def clear(self) -> None:
        """Clear entire cache"""
        # Clear memory
        self._memory_cache.clear()

        # Clear disk
        for cache_file in self.task_cache_dir.glob("*.pkl"):
            cache_file.unlink()

        for cache_file in self.workflow_cache_dir.glob("*.json"):
            cache_file.unlink()

    def get_cache_stats(self) -> Dict[str, Any]:
        """
        Get cache statistics

        Returns:
            Dictionary with cache stats
        """
        task_files = list(self.task_cache_dir.glob("*.pkl"))
        workflow_files = list(self.workflow_cache_dir.glob("*.json"))

        # Calculate total size
        total_size = sum(f.stat().st_size for f in task_files)
        total_size += sum(f.stat().st_size for f in workflow_files)

        return {
            'task_cache_entries': len(task_files),
            'workflow_cache_entries': len(workflow_files),
            'memory_cache_entries': len(self._memory_cache),
            'total_size_mb': total_size / (1024 * 1024),
            'cache_dir': str(self.cache_dir),
        }

    def _cleanup_if_needed(self) -> None:
        """Cleanup cache if size exceeds maximum"""
        stats = self.get_cache_stats()

        if stats['total_size_mb'] > self.max_cache_size_mb:
            # Remove oldest files first
            all_files = list(self.task_cache_dir.glob("*.pkl"))
            all_files.extend(self.workflow_cache_dir.glob("*.json"))

            # Sort by modification time
            all_files.sort(key=lambda f: f.stat().st_mtime)

            # Remove oldest 25%
            files_to_remove = all_files[:len(all_files) // 4]

            for f in files_to_remove:
                f.unlink()

            # Clear memory cache
            self._memory_cache.clear()


class CacheManager:
    """
    Global cache manager for workflow execution

    Singleton pattern for managing workflow cache across application.
    """

    _instance: Optional['CacheManager'] = None

    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
            cls._instance.cache = WorkflowCache()
        return cls._instance

    @classmethod
    def get_cache(cls) -> WorkflowCache:
        """Get global cache instance"""
        instance = cls()
        return instance.cache

    @classmethod
    def configure(
        cls,
        cache_dir: Optional[str] = None,
        ttl_hours: int = 24,
        max_cache_size_mb: int = 1000,
    ) -> None:
        """
        Configure global cache

        Args:
            cache_dir: Cache directory
            ttl_hours: Cache TTL
            max_cache_size_mb: Maximum cache size
        """
        instance = cls()
        instance.cache = WorkflowCache(
            cache_dir=cache_dir,
            ttl_hours=ttl_hours,
            max_cache_size_mb=max_cache_size_mb,
        )
