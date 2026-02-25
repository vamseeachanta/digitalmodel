# ABOUTME: Benchmark package for dynacard vision model evaluation (WRK-251).
# ABOUTME: Provides test set builder, vision classifier interface, and benchmark runner.

from .test_set_builder import build_hold_out_test_set, LabelledCard
from .vision_classifier import (
    VisionModelClassifier,
    ClassifierBackend,
    VisionClassificationResult,
    StubVisionClassifier,
)
from .runner import BenchmarkRunner, BenchmarkResult, MethodResult, run_benchmark

__all__ = [
    "build_hold_out_test_set",
    "LabelledCard",
    "VisionModelClassifier",
    "ClassifierBackend",
    "VisionClassificationResult",
    "StubVisionClassifier",
    "BenchmarkRunner",
    "BenchmarkResult",
    "MethodResult",
    "run_benchmark",
]
