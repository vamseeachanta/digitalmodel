#!/bin/bash
# ABOUTME: Unix/Linux shell script to run cross-repository tests
# Quick launcher for cross-repository test runner

set -e

echo "========================================"
echo "Cross-Repository Test Runner"
echo "========================================"
echo ""

cd "$(dirname "$0")/.."

if [ "$1" = "--help" ]; then
    echo "Usage: run_cross_repo_tests.sh [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --sequential    Run tests sequentially instead of parallel"
    echo "  --workers N     Set number of parallel workers (default: 4)"
    echo "  --timeout N     Set timeout per repo in seconds (default: 300)"
    echo "  --help          Show this help message"
    echo ""
    exit 0
fi

echo "Starting cross-repository tests..."
echo "This will test all repositories in workspace-hub"
echo ""

uv run python scripts/cross_repo_test_runner.py "$@"

if [ $? -eq 0 ]; then
    echo ""
    echo "========================================"
    echo "Tests completed successfully!"
    echo "Check reports/cross_repo_tests/ for results"
    echo "========================================"
else
    echo ""
    echo "========================================"
    echo "Tests failed with errors"
    echo "========================================"
    exit 1
fi
