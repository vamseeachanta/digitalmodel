# ABOUTME: Assembles a labelled hold-out test set for dynacard classification benchmarks.
# ABOUTME: Uses seeds >= base_seed to ensure no overlap with training data (seeds 0..N-1).

from __future__ import annotations

from dataclasses import dataclass

from ..card_generators import ALL_GENERATORS
from ..models import CardData

# Minimum seeds gap above training range to guarantee no overlap
_HOLDOUT_SEED_FLOOR = 1000


@dataclass
class LabelledCard:
    """A synthetic pump card with its ground-truth failure mode label."""

    card: CardData
    label: str
    seed: int


def build_hold_out_test_set(
    samples_per_mode: int = 3,
    base_seed: int = _HOLDOUT_SEED_FLOOR,
) -> list[LabelledCard]:
    """Generate a balanced hold-out test set across all 18 failure modes.

    Seeds used are [base_seed, base_seed + samples_per_mode) per mode to
    guarantee no overlap with training data, which uses seeds [0, N).

    Args:
        samples_per_mode: Number of cards to generate per failure mode.
            Set to 3 for fast tests; use 3+ for actual evaluation.
            With 18 modes * 3 samples = 54 cards (>= 50 acceptance criterion).
        base_seed: Starting seed offset. Must be >= 1000 to clear training range.

    Returns:
        List of LabelledCard objects in mode-then-seed order.
    """
    if base_seed < _HOLDOUT_SEED_FLOOR:
        raise ValueError(
            f"base_seed must be >= {_HOLDOUT_SEED_FLOOR} to avoid training overlap; "
            f"got {base_seed}"
        )

    result: list[LabelledCard] = []
    for mode_name, gen_func in ALL_GENERATORS.items():
        for offset in range(samples_per_mode):
            seed = base_seed + offset
            card = gen_func(seed=seed)
            result.append(LabelledCard(card=card, label=mode_name, seed=seed))

    return result
