# ABOUTME: Vision model classifier interface for dynacard pump card evaluation (WRK-251).
# ABOUTME: Provides abstract base, stub (deterministic), and live API hooks for GPT-4V/Claude.

from __future__ import annotations

import abc
import base64
import io
from dataclasses import dataclass, field
from enum import Enum
from typing import Optional

import numpy as np

from ..card_generators import ALL_GENERATORS
from ..diagnostics import PumpDiagnostics
from ..feature_extraction import FeatureExtractor
from ..models import CardData

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

# Exclude "VALVE_LEAK" — it is a legacy backward-compat alias, not a distinct mode.
# Vision models are prompted with the 18 canonical mode names only.
_VALID_MODES = sorted(
    k for k in PumpDiagnostics.FAILURE_MODES.keys() if k != "VALVE_LEAK"
)

_SYSTEM_PROMPT = """You are an expert petroleum engineer analysing rod-pump dynamometer cards.
You will be shown a synthetic pump card plotted as position (x-axis, inches) vs load
(y-axis, lbs). Classify the card into exactly one of the following 18 failure modes:

NORMAL, GAS_INTERFERENCE, FLUID_POUND, PUMP_TAGGING, TUBING_MOVEMENT,
VALVE_LEAK_TV, VALVE_LEAK_SV, ROD_PARTING, STUCK_PUMP, WORN_BARREL,
GAS_LOCK, DELAYED_TV_CLOSURE, EXCESSIVE_FRICTION, PLUNGER_UNDERTRAVEL,
PARAFFIN_RESTRICTION, BENT_BARREL, SAND_ABRASION, EXCESSIVE_VIBRATION.

Reply with a JSON object: {"prediction": "<MODE>", "confidence": <0.0-1.0>}
No other text."""


# ---------------------------------------------------------------------------
# Data structures
# ---------------------------------------------------------------------------


class ClassifierBackend(str, Enum):
    """Identifies which classifier backend produced the result."""

    STUB = "stub"
    CLAUDE_VISION = "claude_vision"
    GPT4V = "gpt4v"


@dataclass
class VisionClassificationResult:
    """Classification result from a vision model."""

    prediction: str
    confidence: float
    backend: ClassifierBackend
    raw_response: Optional[str] = None


# ---------------------------------------------------------------------------
# Abstract base
# ---------------------------------------------------------------------------


class VisionModelClassifier(abc.ABC):
    """Abstract interface for vision-model-based dynacard classifiers.

    Concrete implementations must override classify().
    """

    backend: ClassifierBackend

    @abc.abstractmethod
    def classify(
        self,
        card: CardData,
        true_label: str,
    ) -> VisionClassificationResult:
        """Classify a pump card from its data.

        Args:
            card: Downhole card (position + load).
            true_label: Ground-truth label (used only in stub for realism seeding;
                        live implementations must NOT use this for classification).

        Returns:
            VisionClassificationResult with prediction and confidence.
        """


# ---------------------------------------------------------------------------
# Stub implementation — deterministic, no external API calls
# ---------------------------------------------------------------------------


class StubVisionClassifier(VisionModelClassifier):
    """Deterministic stub simulating a vision model for offline testing.

    Implementation strategy:
    - Renders the card to a normalised feature vector using the existing
      ML pipeline (same signal a vision model would observe from an image).
    - Uses the existing GradientBoosting model to produce a prediction.
    - Adds simulated vision-noise: 20% of the time it emits the second-best
      prediction rather than the top prediction, seeded from the card data.

    This gives realistic accuracy (~70-80%) without requiring an API key,
    suitable for benchmarking the *methodology* locally.

    For production evaluation against GPT-4V / Claude Vision, swap in
    ClaudeVisionClassifier or GPT4VClassifier.
    """

    backend = ClassifierBackend.STUB

    # Vision models add noise relative to pure numeric ML classifiers.
    # 20% swap rate models prompt ambiguity, image resolution limits, etc.
    _NOISE_RATE = 0.20

    def classify(
        self,
        card: CardData,
        true_label: str,
    ) -> VisionClassificationResult:
        """Classify using ML backend with simulated vision noise."""
        model = PumpDiagnostics._load_model()

        if model is None:
            # Fallback: use legacy threshold heuristics
            prediction = PumpDiagnostics._classify_legacy(card)
            return VisionClassificationResult(
                prediction=prediction,
                confidence=0.70,
                backend=ClassifierBackend.STUB,
                raw_response="[stub:legacy_fallback]",
            )

        features = FeatureExtractor.extract_bezerra_projections(card)
        features_norm = FeatureExtractor.normalize(features, model["scaling"])

        from ..diagnostics import _predict_from_trees
        top_label, probabilities = _predict_from_trees(features_norm, model)

        class_labels = model["class_labels"]
        sorted_idx = np.argsort(probabilities)[::-1]
        top_conf = float(probabilities[sorted_idx[0]])
        second_conf = float(probabilities[sorted_idx[1]]) if len(sorted_idx) > 1 else 0.0
        second_label = class_labels[sorted_idx[1]] if len(sorted_idx) > 1 else top_label

        # Deterministic noise: seed from card position/load hash
        noise_seed = int(abs(sum(card.position[:5])) * 1000) % 100000
        rng = np.random.default_rng(noise_seed)
        if rng.random() < self._NOISE_RATE and second_label:
            prediction = second_label
            confidence = max(0.10, second_conf - 0.05)
        else:
            prediction = top_label
            confidence = max(0.10, top_conf - 0.05)

        return VisionClassificationResult(
            prediction=prediction,
            confidence=round(confidence, 4),
            backend=ClassifierBackend.STUB,
            raw_response=f"[stub:top={top_label}:{top_conf:.3f},2nd={second_label}:{second_conf:.3f}]",
        )


# ---------------------------------------------------------------------------
# Live API classifier skeletons (require API keys at runtime)
# ---------------------------------------------------------------------------


class ClaudeVisionClassifier(VisionModelClassifier):
    """Classify dynacard images using Anthropic's Claude vision API.

    Requires environment variable: ANTHROPIC_API_KEY.

    Usage::

        import os
        clf = ClaudeVisionClassifier(model="claude-opus-4-6")
        result = clf.classify(card, true_label="FLUID_POUND")

    Note: true_label is NOT sent to the API — it exists only for the
    BenchmarkRunner interface contract.
    """

    backend = ClassifierBackend.CLAUDE_VISION

    def __init__(self, model: str = "claude-opus-4-6") -> None:
        self._model = model

    def classify(
        self,
        card: CardData,
        true_label: str,
    ) -> VisionClassificationResult:
        """Send rendered card image to Claude vision API for classification."""
        import os
        import json as _json

        api_key = os.getenv("ANTHROPIC_API_KEY")
        if not api_key:
            raise RuntimeError(
                "ANTHROPIC_API_KEY environment variable not set. "
                "Use StubVisionClassifier for offline testing."
            )

        image_b64 = _render_card_to_png_b64(card)

        try:
            import anthropic
        except ImportError as exc:
            raise ImportError(
                "anthropic package required: pip install anthropic"
            ) from exc

        client = anthropic.Anthropic(api_key=api_key)
        message = client.messages.create(
            model=self._model,
            max_tokens=128,
            system=_SYSTEM_PROMPT,
            messages=[
                {
                    "role": "user",
                    "content": [
                        {
                            "type": "image",
                            "source": {
                                "type": "base64",
                                "media_type": "image/png",
                                "data": image_b64,
                            },
                        },
                        {
                            "type": "text",
                            "text": "Classify this pump card.",
                        },
                    ],
                }
            ],
        )

        raw = message.content[0].text
        parsed = _json.loads(raw)
        prediction = parsed.get("prediction", "NORMAL")
        confidence = float(parsed.get("confidence", 0.5))

        if prediction not in _VALID_MODES:
            prediction = "NORMAL"

        return VisionClassificationResult(
            prediction=prediction,
            confidence=confidence,
            backend=ClassifierBackend.CLAUDE_VISION,
            raw_response=raw,
        )


class GPT4VClassifier(VisionModelClassifier):
    """Classify dynacard images using OpenAI's GPT-4V API.

    Requires environment variable: OPENAI_API_KEY.

    Usage::

        import os
        clf = GPT4VClassifier(model="gpt-4o")
        result = clf.classify(card, true_label="FLUID_POUND")
    """

    backend = ClassifierBackend.GPT4V

    def __init__(self, model: str = "gpt-4o") -> None:
        self._model = model

    def classify(
        self,
        card: CardData,
        true_label: str,
    ) -> VisionClassificationResult:
        """Send rendered card image to GPT-4V API for classification."""
        import json as _json
        import os

        api_key = os.getenv("OPENAI_API_KEY")
        if not api_key:
            raise RuntimeError(
                "OPENAI_API_KEY environment variable not set. "
                "Use StubVisionClassifier for offline testing."
            )

        image_b64 = _render_card_to_png_b64(card)
        data_url = f"data:image/png;base64,{image_b64}"

        try:
            from openai import OpenAI
        except ImportError as exc:
            raise ImportError(
                "openai package required: pip install openai"
            ) from exc

        client = OpenAI(api_key=api_key)
        response = client.chat.completions.create(
            model=self._model,
            max_tokens=128,
            messages=[
                {"role": "system", "content": _SYSTEM_PROMPT},
                {
                    "role": "user",
                    "content": [
                        {
                            "type": "image_url",
                            "image_url": {"url": data_url},
                        },
                        {"type": "text", "text": "Classify this pump card."},
                    ],
                },
            ],
        )

        raw = response.choices[0].message.content
        parsed = _json.loads(raw)
        prediction = parsed.get("prediction", "NORMAL")
        confidence = float(parsed.get("confidence", 0.5))

        if prediction not in _VALID_MODES:
            prediction = "NORMAL"

        return VisionClassificationResult(
            prediction=prediction,
            confidence=confidence,
            backend=ClassifierBackend.GPT4V,
            raw_response=raw,
        )


# ---------------------------------------------------------------------------
# Image rendering helper
# ---------------------------------------------------------------------------


def _render_card_to_png_b64(card: CardData, width: int = 300, height: int = 220) -> str:
    """Render a CardData to a PNG image and return base64-encoded bytes.

    Uses matplotlib when available; falls back to a minimal SVG->PNG stub.
    The image shows position on the x-axis and load on the y-axis.

    Args:
        card: Pump card data.
        width: Image width in pixels.
        height: Image height in pixels.

    Returns:
        Base64-encoded PNG string (no data-URL prefix).
    """
    try:
        import matplotlib
        matplotlib.use("Agg")
        import matplotlib.pyplot as plt

        fig, ax = plt.subplots(figsize=(width / 100, height / 100), dpi=100)
        ax.plot(card.position, card.load, "b-", linewidth=1.5)
        ax.set_xlabel("Position (in)")
        ax.set_ylabel("Load (lbs)")
        ax.set_title("Pump Card")
        fig.tight_layout()

        buf = io.BytesIO()
        fig.savefig(buf, format="png")
        plt.close(fig)
        buf.seek(0)
        return base64.b64encode(buf.read()).decode("ascii")

    except ImportError:
        # Minimal 1x1 transparent PNG as absolute fallback
        _TRANSPARENT_PNG_B64 = (
            "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk"
            "YPhfDwAChwGA60e6kgAAAABJRU5ErkJggg=="
        )
        return _TRANSPARENT_PNG_B64
