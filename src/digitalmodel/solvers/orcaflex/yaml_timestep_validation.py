"""Conservative source-order checks before YAML mappings lose information.

This is not an include interpreter or a native OrcaFlex acceptance check.
"""

from collections import Counter
from math import isfinite

import yaml
from yaml.nodes import MappingNode, ScalarNode, SequenceNode

MAXIMUM = "ImplicitVariableMaxTimeStep"
MODE = "ImplicitUseVariableTimeStep"
METHOD = "DynamicsSolutionMethod"
RELEVANT = {MAXIMUM, MODE, METHOD}


def _key(node):
    return node.value if isinstance(node, ScalarNode) else None


def _inventory(root):
    """Bound expanded traversal as well as unique nodes (alias fan-out)."""
    stack = [(root, (), frozenset())]
    mappings, counts, visits = [], Counter(), 0
    while stack:
        node, path, ancestors = stack.pop()
        visits += 1
        if id(node) in ancestors:
            return mappings, counts, "Unsupported cyclic YAML alias graph"
        if visits > 100000 or len(ancestors) > 100:
            return mappings, counts, "Unsupported YAML graph traversal limit"
        counts[id(node)] += 1
        ancestors = ancestors | {id(node)}
        if isinstance(node, MappingNode):
            mappings.append((node, path))
            for key, value in reversed(node.value):
                stack.append((value, path + (_key(key),), ancestors))
                stack.append((key, path, ancestors))
        elif isinstance(node, SequenceNode):
            for value in reversed(node.value):
                stack.append((value, path + (None,), ancestors))
    return mappings, counts, None


def _ambiguous(node, counts):
    keys = Counter(_key(key) for key, _ in node.value)
    if keys["<<"] or any(keys[key] > 1 for key in RELEVANT):
        return True
    return counts[id(node)] > 1 or any(
        counts[id(value)] > 1 or counts[id(key)] > 1
        for key, value in node.value if _key(key) in RELEVANT
    )


def _boolean(node):
    if not isinstance(node, ScalarNode):
        return None
    if node.tag == "tag:yaml.org,2002:bool":
        return node.value.lower() in {"true", "yes", "on"}
    if node.tag == "tag:yaml.org,2002:str" and node.value in {"Yes", "No"}:
        return node.value == "Yes"
    return None


def _positive_number(node):
    if not isinstance(node, ScalarNode) or node.tag not in {
        "tag:yaml.org,2002:int", "tag:yaml.org,2002:float"
    }:
        return False
    try:
        # Construct the scalar with its original tag, not a guessed string type.
        loader = yaml.SafeLoader("")
        try:
            value = loader.construct_object(node)
        finally:
            loader.dispose()
        return isfinite(value) and value > 0
    except (ValueError, TypeError, OverflowError):
        return False


def _maximum_issue(value, method, mode, ambiguous, placement):
    if not _positive_number(value):
        return "error", f"{MAXIMUM} must be a finite positive number"
    if placement == "wrong":
        return "error", f"{MAXIMUM} belongs in General, not this section"
    if ambiguous or placement == "unknown":
        return "warning", f"Unresolved or ambiguous General context for {MAXIMUM}"
    if mode is False or method in {"Explicit time domain", "Frequency domain"}:
        return "error", f"{MAXIMUM} conflicts with preceding solution method or variable-step mode"
    if mode is not True or method != "Implicit time domain":
        return "warning", f"Unresolved inherited context for {MAXIMUM}"
    return None


def _mapping_issues(node, placement, ambiguous):
    method, mode = None, None
    issues = []
    for key_node, value in node.value:
        key = _key(key_node)
        if key in {"includefile", "BaseFile", "<<"}:
            method, mode = None, None
        elif key == METHOD:
            method = _key(value)
        elif key == MODE:
            mode = _boolean(value)
        elif key == MAXIMUM:
            issue = _maximum_issue(value, method, mode, ambiguous, placement)
            if issue:
                issues.append((*issue, MAXIMUM, key_node.start_mark.line + 1))
    return issues


def inspect_source(raw_text, sections, invalid_properties):
    """Return (severity, message, property, line) diagnostics and halt flag."""
    root = yaml.compose(raw_text, Loader=yaml.SafeLoader)
    mappings, counts, failure = _inventory(root)
    issues = []
    duplicate_general = isinstance(root, MappingNode) and sum(
        _key(key) == "General" for key, _ in root.value
    ) > 1
    seen = set()
    for node, path in mappings:
        # Preserve forbidden properties even when duplicate keys would hide them.
        for key, _ in node.value:
            name = _key(key)
            if name in invalid_properties and (id(key), name) not in seen:
                seen.add((id(key), name))
                issues.append(("error", f"Invalid property '{name}' — this property does not exist in OrcaFlex",
                               name, key.start_mark.line + 1))
        placement = "general" if path == ("General",) else "unknown"
        if path and path[0] in sections and placement != "general":
            placement = "wrong"
        ambiguous = _ambiguous(node, counts) or duplicate_general
        issues.extend(_mapping_issues(node, placement, ambiguous))
    if failure:
        issues.append(("error", failure, "", None))
    return issues, failure is not None
