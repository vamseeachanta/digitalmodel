#!/bin/bash

# Hook Configuration

# Auto-execute readiness check before tasks
AUTO_READINESS_CHECK=1

# Cache duration (seconds)
READINESS_CACHE_DURATION=3600

# Minimum readiness score to proceed
MINIMUM_READINESS_SCORE=70

# Action on low readiness
# "prompt" = ask user, "block" = prevent execution, "warn" = show warning but proceed
LOW_READINESS_ACTION="prompt"
