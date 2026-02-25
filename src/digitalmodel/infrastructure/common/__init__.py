# ABOUTME: Common utilities for digitalmodel package (backward-compat namespace)
# ABOUTME: Real implementations have moved to infrastructure/utils/

"""
digitalmodel.infrastructure.common - Backward-compat namespace (deprecated).

All utilities have migrated to digitalmodel.infrastructure.utils.
This package is preserved for backward compatibility only.
Migrated from aceengineercode on 2026-01-24.
Phase 2F: real files moved to infrastructure/utils/ on 2026-02-24.
"""

import warnings as _warnings
from pathlib import Path

COMMON_DIR = Path(__file__).parent

# Emit a package-level deprecation warning when the package itself is imported
# (individual shim files emit their own warnings for per-module imports)
