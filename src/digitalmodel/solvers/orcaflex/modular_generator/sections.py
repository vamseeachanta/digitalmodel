"""Variable resolution for template-based section overrides."""
from __future__ import annotations

import re
from typing import Any

import yaml

# Pattern: ${key} or ${key:format_spec}
_TOKEN_RE = re.compile(r"\$\{(\w+)(?::([^}]+))?\}")


class VariableResolver:
    """Resolves ${variable_name} tokens in template content."""

    @staticmethod
    def resolve(template_content: str, variables: dict[str, Any]) -> str:
        """Replace ${key} and ${key:format} tokens in template content.

        Rules:
            - Pattern: ${key} or ${key:format_spec}
            - Whole-number floats auto-format as int (e.g., 30.0 -> "30")
            - Format specifiers use Python f-string syntax (e.g., :.0f, :.2f)
            - After substitution, validates result as valid YAML via yaml.safe_load()
            - Raises ValueError if any ${...} token remains unresolved
            - Raises ValueError if post-substitution content is not valid YAML

        Args:
            template_content: String potentially containing ${...} tokens.
            variables: Mapping of variable names to values.

        Returns:
            Resolved string with all tokens replaced.

        Raises:
            ValueError: If unresolved tokens remain or result is invalid YAML.
        """

        def _replace(match: re.Match) -> str:
            key = match.group(1)
            fmt = match.group(2)

            if key not in variables:
                # Leave unresolved for later detection
                return match.group(0)

            value = variables[key]

            if fmt:
                return format(value, fmt)

            # Auto-format whole-number floats as int strings
            if isinstance(value, float) and value == int(value):
                return str(int(value))

            return str(value)

        result = _TOKEN_RE.sub(_replace, template_content)

        # Check for unresolved tokens
        remaining = _TOKEN_RE.findall(result)
        if remaining:
            unresolved_keys = [k for k, _ in remaining]
            raise ValueError(
                f"Template has unresolved tokens: {unresolved_keys}"
            )

        # Validate YAML
        try:
            yaml.safe_load(result)
        except yaml.YAMLError as exc:
            raise ValueError(
                f"Post-substitution content is not valid YAML: {exc}"
            ) from exc

        return result
