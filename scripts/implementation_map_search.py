# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Bound selected ordered searches without changing their existence semantics.

Only the exact branches below receive ordered-token matching. Their first token
has a deterministic endpoint and subsequent tokens have no overlapping choices.
The wildcards exclude newlines; the force prefix alone may cross newlines.
Other branches retain regular-expression searches. This is not a general regular
expression optimizer.
"""

from functools import lru_cache
import re


_ORDERED_TOKENS = {
    r"(?:0\.5|1\s*/\s*2).*(?:rho|density).*(?:cd|drag|area)": (
        r"(?:0\.5|1\s*/\s*2)", r"(?:rho|density)", r"(?:cd|drag|area)"
    ),
    r"(?:stress|stress_range|delta_sigma).*\*\*.*\bm\b": (
        r"(?:stress|stress_range|delta_sigma)", r"\*\*", r"\bm\b"
    ),
    r"log10.*(?:cycles|stress)": (r"log10", r"(?:cycles|stress)"),
    r"internal_pressure.*(?:area|A_i)": (r"internal_pressure", r"(?:area|A_i)"),
    r"external_pressure.*(?:area|A_o)": (r"external_pressure", r"(?:area|A_o)"),
}


def _branches(pattern: str) -> tuple[str, ...] | None:
    """Split outer alternatives while preserving escapes, groups and classes.

    Args:
        pattern: A valid string regular expression.

    Returns:
        Outer branches, or None when comment syntax prevents reliable splitting.
    """
    if "(?#" in pattern:
        return None
    depth = 0
    class_start = -1
    escaped = False
    start = 0
    branches = []
    for index, char in enumerate(pattern):
        if escaped:
            escaped = False
            continue
        if char == "\\":
            escaped = True
            continue
        if class_start >= 0:
            literal_close = index == class_start + 1 or (
                index == class_start + 2 and pattern[class_start + 1] == "^"
            )
            if char == "]" and not literal_close:
                class_start = -1
            continue
        if char == "[":
            class_start = index
        elif char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
        elif char == "|" and depth == 0:
            branches.append(pattern[start:index])
            start = index + 1
    if depth or class_start >= 0 or escaped:
        return None
    return tuple(branches + [pattern[start:]])


@lru_cache(maxsize=128)
def _search_plan(pattern: str, flags: int) -> tuple[tuple[re.Pattern[str], ...], ...] | None:
    """Compile a bounded cache of exact ordered branches and ordinary branches.

    Args:
        pattern: Original regular-expression source.
        flags: Original compilation flags.

    Returns:
        Compiled branch tokens, or None when the original search is required.
    """
    branches = _branches(pattern)
    if branches is None or not any(branch in _ORDERED_TOKENS for branch in branches):
        return None
    try:
        return tuple(
            tuple(re.compile(token, flags) for token in _ORDERED_TOKENS.get(branch, (branch,)))
            for branch in branches
        )
    except re.error:
        return None


def _ordered_suffix(tokens: tuple[re.Pattern[str], ...], text: str, start: int, end: int) -> bool:
    """Find remaining tokens in order with original word-boundary context.

    Args:
        tokens: Nonoverlapping ordered suffix token expressions.
        text: Original input, retained to preserve boundary context.
        start: First permitted suffix position.
        end: Next physical newline position or end of input.

    Returns:
        Whether every suffix token occurs in order before the newline.
    """
    for token in tokens:
        found = token.search(text, start, end)
        if found is None:
            return False
        start = found.end()
    return True


def _ordered_match(tokens: tuple[re.Pattern[str], ...], text: str) -> bool:
    """Evaluate the earliest prefix ending on each physical line.

    Args:
        tokens: Prefix followed by ordered suffix expressions.
        text: Original input.

    Returns:
        Whether one prefix admits all suffix tokens before its following newline.
    """
    line_end = -1
    for prefix in tokens[0].finditer(text):
        if prefix.end() <= line_end:
            continue
        line_end = text.find("\n", prefix.end())
        if line_end < 0:
            line_end = len(text)
        if _ordered_suffix(tokens[1:], text, prefix.end(), line_end):
            return True
    return False


def matches(compiled_pattern: re.Pattern[str], text: str) -> bool:
    """Test existence using exact ordered rules or the original expression.

    Args:
        compiled_pattern: String regular expression with its original flags.
        text: Input to search.

    Returns:
        The same existence result as the compiled expression's search. Capturing
        expressions and DOTALL or VERBOSE flags retain the original search.
    """
    if compiled_pattern.groups or compiled_pattern.flags & (re.DOTALL | re.VERBOSE):
        return compiled_pattern.search(text) is not None
    plan = _search_plan(compiled_pattern.pattern, compiled_pattern.flags)
    if plan is None:
        return compiled_pattern.search(text) is not None
    return any(
        tokens[0].search(text) is not None if len(tokens) == 1 else _ordered_match(tokens, text)
        for tokens in plan
    )
