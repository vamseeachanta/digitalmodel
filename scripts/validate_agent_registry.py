"""
ABOUTME: Validates agent registry structure and data consistency.
Checks required fields, data types, references to definition files, and business rules.
"""

import json
from pathlib import Path
from typing import Dict, List, Any, Tuple
import sys
import argparse


def validate_required_fields(agent_name: str, agent_data: Dict[str, Any]) -> List[str]:
    """
    Validate that agent has all required fields.

    Required fields:
    - name
    - description
    - capabilities (array)
    - performance_score (0-100)

    Args:
        agent_name: Name of the agent
        agent_data: Agent metadata dictionary

    Returns:
        List of error messages (empty if valid)
    """
    errors = []

    # Check required fields
    required_fields = ['name', 'description', 'capabilities', 'performance_score']

    for field in required_fields:
        if field not in agent_data:
            errors.append(f"Agent '{agent_name}': Missing required field '{field}'")

    # Validate field types
    if 'capabilities' in agent_data:
        if not isinstance(agent_data['capabilities'], list):
            errors.append(f"Agent '{agent_name}': 'capabilities' must be an array")

    if 'performance_score' in agent_data:
        score = agent_data['performance_score']
        if not isinstance(score, (int, float)):
            errors.append(f"Agent '{agent_name}': 'performance_score' must be a number")
        elif not (0 <= score <= 100):
            errors.append(f"Agent '{agent_name}': 'performance_score' must be 0-100 (got {score})")

    return errors


def validate_performance_metrics(agent_name: str, agent_data: Dict[str, Any]) -> List[str]:
    """
    Validate performance metrics structure.

    Args:
        agent_name: Name of the agent
        agent_data: Agent metadata dictionary

    Returns:
        List of error messages (empty if valid)
    """
    errors = []

    if 'performance' not in agent_data:
        errors.append(f"Agent '{agent_name}': Missing 'performance' metrics")
        return errors

    perf = agent_data['performance']

    # Check required performance fields
    required = [
        'success_rate',
        'avg_execution_time',
        'total_tasks',
        'successful_tasks',
        'failed_tasks',
        'cost_per_task'
    ]

    for field in required:
        if field not in perf:
            errors.append(f"Agent '{agent_name}': Missing performance field '{field}'")

    # Validate success_rate
    if 'success_rate' in perf:
        rate = perf['success_rate']
        if not isinstance(rate, (int, float)) or not (0 <= rate <= 100):
            errors.append(f"Agent '{agent_name}': Invalid success_rate {rate} (must be 0-100)")

    # Validate avg_execution_time
    if 'avg_execution_time' in perf:
        time_val = perf['avg_execution_time']
        if not isinstance(time_val, (int, float)) or time_val < 0:
            errors.append(f"Agent '{agent_name}': Invalid avg_execution_time {time_val}")

    # Validate task counts
    if 'total_tasks' in perf and 'successful_tasks' in perf and 'failed_tasks' in perf:
        total = perf['total_tasks']
        successful = perf['successful_tasks']
        failed = perf['failed_tasks']

        if total != successful + failed:
            errors.append(
                f"Agent '{agent_name}': Task count mismatch "
                f"(total={total}, successful={successful}, failed={failed})"
            )

    return errors


def validate_availability_metrics(agent_name: str, agent_data: Dict[str, Any]) -> List[str]:
    """
    Validate availability metrics structure.

    Args:
        agent_name: Name of the agent
        agent_data: Agent metadata dictionary

    Returns:
        List of error messages (empty if valid)
    """
    errors = []

    if 'availability' not in agent_data:
        errors.append(f"Agent '{agent_name}': Missing 'availability' metrics")
        return errors

    avail = agent_data['availability']

    # Check required availability fields
    required = [
        'is_available',
        'current_load',
        'max_concurrent',
        'queue_length',
        'uptime_percentage'
    ]

    for field in required:
        if field not in avail:
            errors.append(f"Agent '{agent_name}': Missing availability field '{field}'")

    # Validate is_available
    if 'is_available' in avail and not isinstance(avail['is_available'], bool):
        errors.append(f"Agent '{agent_name}': 'is_available' must be boolean")

    # Validate current_load
    if 'current_load' in avail:
        load = avail['current_load']
        if not isinstance(load, (int, float)) or not (0 <= load <= 1):
            errors.append(f"Agent '{agent_name}': Invalid current_load {load} (must be 0-1)")

    # Validate max_concurrent
    if 'max_concurrent' in avail:
        max_conc = avail['max_concurrent']
        if not isinstance(max_conc, int) or max_conc <= 0:
            errors.append(f"Agent '{agent_name}': Invalid max_concurrent {max_conc}")

    # Validate uptime_percentage
    if 'uptime_percentage' in avail:
        uptime = avail['uptime_percentage']
        if not isinstance(uptime, (int, float)) or not (0 <= uptime <= 100):
            errors.append(f"Agent '{agent_name}': Invalid uptime_percentage {uptime}")

    return errors


def validate_definition_file(agent_name: str, agent_data: Dict[str, Any], agents_dir: Path) -> List[str]:
    """
    Validate that definition file exists.

    Args:
        agent_name: Name of the agent
        agent_data: Agent metadata dictionary
        agents_dir: Base directory for agent definitions

    Returns:
        List of warning messages (empty if valid)
    """
    warnings = []

    if 'definition_file' not in agent_data:
        warnings.append(f"Agent '{agent_name}': No definition_file specified")
        return warnings

    def_file = Path(agent_data['definition_file'])

    # Skip validation for JSON registry files
    if def_file.suffix == '.json':
        return warnings

    # Check if file exists (relative to project root)
    if not def_file.exists():
        warnings.append(f"Agent '{agent_name}': Definition file not found: {def_file}")

    return warnings


def validate_alias_uniqueness(agents: Dict[str, Dict[str, Any]]) -> List[str]:
    """
    Validate that all aliases are unique.

    Args:
        agents: Dictionary of all agents

    Returns:
        List of error messages (empty if valid)
    """
    errors = []
    alias_map = {}

    for agent_name, agent_data in agents.items():
        alias = agent_data.get('alias')

        if not alias:
            errors.append(f"Agent '{agent_name}': Missing alias")
            continue

        if alias in alias_map:
            errors.append(
                f"Duplicate alias '{alias}': used by '{alias_map[alias]}' and '{agent_name}'"
            )
        else:
            alias_map[alias] = agent_name

    return errors


def validate_performance_history(agent_name: str, agent_data: Dict[str, Any]) -> List[str]:
    """
    Validate performance history structure.

    Args:
        agent_name: Name of the agent
        agent_data: Agent metadata dictionary

    Returns:
        List of error messages (empty if valid)
    """
    errors = []

    if 'performance_history' not in agent_data:
        errors.append(f"Agent '{agent_name}': Missing performance_history")
        return errors

    history = agent_data['performance_history']

    if not isinstance(history, list):
        errors.append(f"Agent '{agent_name}': performance_history must be an array")
        return errors

    # Validate each history entry
    for i, entry in enumerate(history):
        if not isinstance(entry, dict):
            errors.append(f"Agent '{agent_name}': history[{i}] must be an object")
            continue

        # Check required fields
        if 'timestamp' not in entry:
            errors.append(f"Agent '{agent_name}': history[{i}] missing timestamp")

        # Could add more validation for metrics in history entries

    return errors


def validate_registry_structure(registry: Dict[str, Any]) -> List[str]:
    """
    Validate top-level registry structure.

    Args:
        registry: Complete registry dictionary

    Returns:
        List of error messages (empty if valid)
    """
    errors = []

    # Check required top-level keys
    required_keys = ['meta', 'agents', 'aliases', 'categories']

    for key in required_keys:
        if key not in registry:
            errors.append(f"Registry missing required key: '{key}'")

    # Validate meta
    if 'meta' in registry:
        meta = registry['meta']
        required_meta = ['version', 'last_updated', 'total_agents']

        for key in required_meta:
            if key not in meta:
                errors.append(f"Registry meta missing: '{key}'")

        # Validate total_agents matches actual count
        if 'total_agents' in meta and 'agents' in registry:
            expected = meta['total_agents']
            actual = len(registry['agents'])

            if expected != actual:
                errors.append(
                    f"Registry meta.total_agents mismatch: "
                    f"expected {expected}, got {actual}"
                )

    # Validate aliases reference valid agents
    if 'aliases' in registry and 'agents' in registry:
        for alias, agent_name in registry['aliases'].items():
            if agent_name not in registry['agents']:
                errors.append(f"Alias '{alias}' references unknown agent '{agent_name}'")

    return errors


def validate_registry(
    registry: Dict[str, Any],
    agents_dir: Path = Path('.claude/agents')
) -> Tuple[bool, List[str]]:
    """
    Complete registry validation.

    Args:
        registry: Registry dictionary to validate
        agents_dir: Base directory for agent definitions

    Returns:
        Tuple of (is_valid, error_messages)
    """
    # Set UTF-8 output for Windows
    if sys.platform == 'win32':
        import io
        if not isinstance(sys.stdout, io.TextIOWrapper):
            sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
        if not isinstance(sys.stderr, io.TextIOWrapper):
            sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

    all_errors = []
    all_warnings = []

    # Validate registry structure
    all_errors.extend(validate_registry_structure(registry))

    # Validate each agent
    agents = registry.get('agents', {})

    for agent_name, agent_data in agents.items():
        # Required fields (strict)
        all_errors.extend(validate_required_fields(agent_name, agent_data))

        # Performance metrics (strict)
        all_errors.extend(validate_performance_metrics(agent_name, agent_data))

        # Availability metrics (strict)
        all_errors.extend(validate_availability_metrics(agent_name, agent_data))

        # Performance history (strict)
        all_errors.extend(validate_performance_history(agent_name, agent_data))

        # Definition file (warning only)
        all_warnings.extend(validate_definition_file(agent_name, agent_data, agents_dir))

    # Validate alias uniqueness (strict)
    all_errors.extend(validate_alias_uniqueness(agents))

    # Combine errors and warnings
    all_messages = all_errors + all_warnings

    # Registry is valid if no strict errors (warnings allowed)
    is_valid = len(all_errors) == 0

    return is_valid, all_messages


def main():
    """Main validation workflow."""
    # Set UTF-8 output for Windows
    if sys.platform == 'win32':
        import io
        sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
        sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

    parser = argparse.ArgumentParser(
        description='Validate agent registry structure and consistency'
    )
    parser.add_argument(
        'registry_file',
        type=Path,
        nargs='?',
        default=Path('.claude/agents-registry.json'),
        help='Path to registry file to validate'
    )
    parser.add_argument(
        '--agents-dir',
        type=Path,
        default=Path('.claude/agents'),
        help='Directory containing agent definition files'
    )
    parser.add_argument(
        '--strict',
        action='store_true',
        help='Treat warnings as errors'
    )

    args = parser.parse_args()

    # Load registry
    if not args.registry_file.exists():
        print(f"❌ Error: Registry file not found: {args.registry_file}")
        return 1

    try:
        with open(args.registry_file, 'r', encoding='utf-8') as f:
            registry = json.load(f)
    except Exception as e:
        print(f"❌ Error loading registry: {e}")
        return 1

    print(f"🔍 Validating registry: {args.registry_file}")
    print()

    # Validate
    is_valid, messages = validate_registry(registry, args.agents_dir)

    # Print results
    if not messages:
        print("✅ Validation passed: No issues found")
        print(f"  Total agents: {len(registry.get('agents', {}))}")
        print(f"  Unique aliases: {len(registry.get('aliases', {}))}")
        print(f"  Categories: {len(registry.get('categories', {}))}")
        return 0

    # Separate errors and warnings
    errors = []
    warnings = []

    for msg in messages:
        if any(keyword in msg for keyword in ['Missing required', 'must be', 'Invalid', 'mismatch', 'Duplicate']):
            errors.append(msg)
        else:
            warnings.append(msg)

    # Print errors
    if errors:
        print("❌ ERRORS:")
        for error in errors:
            print(f"  {error}")
        print()

    # Print warnings
    if warnings:
        print("⚠️  WARNINGS:")
        for warning in warnings:
            print(f"  {warning}")
        print()

    # Determine exit code
    if is_valid and not args.strict:
        print("✅ Validation passed (with warnings)")
        return 0
    elif is_valid and args.strict and warnings:
        print("❌ Validation failed (strict mode: warnings treated as errors)")
        return 1
    else:
        print("❌ Validation failed")
        return 1


if __name__ == '__main__':
    sys.exit(main())
