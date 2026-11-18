# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Run offline: PYTHONPATH=src python examples/naval_architecture/tlp_setdown.py."""

from digitalmodel.naval_architecture.platforms.tlp import tlp_setdown


def main() -> None:
    """Print the synthetic right-triangle example with explicit metre units."""
    # Hand calculation: 5² - 3² = 4²; set-down = 5 - 4 = 1 m.
    # Derivation and scope: docs/naval_architecture/tlp-setdown.md.
    length_m = 5.0
    offset_m = 3.0
    result = tlp_setdown(length_m, offset_m)
    print(f"Tendon length: {length_m:.1f} m")
    print(f"Horizontal offset: {offset_m:.1f} m")
    print(f"Vertical span: {result.vertical_span_m:.1f} m")
    print(f"Set-down (positive downward): {result.setdown_m:.1f} m")
    for quantity in ("Tendon load", "Slack", "Pretension", "Restoring force", "Stability"):
        print(f"{quantity}: NOT_EVALUATED")


if __name__ == "__main__":
    main()
