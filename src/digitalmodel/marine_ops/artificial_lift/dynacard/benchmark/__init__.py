# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
# ABOUTME: Benchmark package for dynacard vision model evaluation (WRK-251).
# ABOUTME: Provides test set builder, vision classifier interface, and benchmark runner.

from .test_set_builder import (
    build_hold_out_test_set, LabelledCard, LabellingProvenance, LabelledSet,
    load_labelled_set,
)
from .measurements import wilson_interval, score_predictions, loop_distance
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
    "LabellingProvenance",
    "LabelledSet",
    "load_labelled_set",
    "wilson_interval",
    "score_predictions",
    "loop_distance",
    "VisionModelClassifier",
    "ClassifierBackend",
    "VisionClassificationResult",
    "StubVisionClassifier",
    "BenchmarkRunner",
    "BenchmarkResult",
    "MethodResult",
    "run_benchmark",
]
