# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Run the synthetic parsed-mapping example without model files or a solver.

Numeric basis: SEMANTIC_DIFF_COMPARISON_POLICY.md, Examples. Display angles
and size are arbitrary; the depth pair has exactly equal numeric values.
"""

from .taxonomy import is_semantically_equivalent


def main() -> None:
    """Print the bounded policy result and every retained difference."""
    strict = {
        "General": {"DefaultViewAzimuth": 270, "DefaultViewSize": 400},
        "Environment": {"WaterDepth": 100, "MultipleCurrentDataCanBeDefined": "No"},
        "Groups": {"Structure": {"Child": "Parent", "Parent": "Model"}},
    }
    generated = {
        "General": {"DefaultViewAzimuth": 0},
        "Environment": {"WaterDepth": 100.0, "MultipleCurrentDataCanBeDefined": False},
        "Groups": {"Structure": {"Child": "Parent", "Parent": "Model"}},
    }
    policy_match, diffs = is_semantically_equivalent(strict, generated)
    print(f"policy_match={policy_match}")
    for diff in diffs:
        print(f"{diff.section}.{diff.key}: {diff.category.value}")


if __name__ == "__main__":
    main()
