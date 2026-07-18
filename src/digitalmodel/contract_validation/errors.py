# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Errors raised by fail-closed contract validators."""


class ContractValidationError(ValueError):
    """Raised when a repository contract cannot be composed safely."""
