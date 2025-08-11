# MANDATORY: All operations use parallel processing where applicable

.PHONY: all install test lint format clean parallel-check

# Default target runs everything in parallel
all: parallel-check
	@echo "🚀 Running all tasks in PARALLEL..."
	@$(MAKE) -j4 install test lint format

parallel-check:
	@echo "✓ Parallel processing is MANDATORY and enabled"

install:
	@echo "📦 Installing dependencies (parallel)..."
	@uv pip install -e . --compile

install-dev:
	@echo "📦 Installing dev dependencies (parallel)..."
	@uv pip install -e .[dev] --compile

test:
	@echo "🧪 Running tests (parallel)..."
	@pytest -n auto --dist loadscope

lint:
	@echo "🔍 Running linters (parallel)..."
	@echo "Running flake8..." & flake8 . & \
	echo "Running mypy..." & mypy . & \
	echo "Running pylint..." & pylint . & \
	wait

format:
	@echo "✨ Formatting code (parallel)..."
	@black . & isort . & wait

clean:
	@echo "🧹 Cleaning (parallel)..."
	@find . -type f -name "*.pyc" -delete & \
	find . -type d -name "__pycache__" -delete & \
	find . -type d -name ".pytest_cache" -exec rm -rf {} + 2>/dev/null & \
	find . -type d -name ".mypy_cache" -exec rm -rf {} + 2>/dev/null & \
	wait

# Parallel file processing example
process-files:
	@echo "📁 Processing files in parallel..."
	@python -c "from .common.parallel_utils import ParallelProcessor; \
	            p = ParallelProcessor(); \
	            p.process_files(['*.py'], lambda x: print(f'Processing {x}'))"

# Benchmark parallel vs sequential
benchmark:
	@echo "📊 Benchmarking parallel processing..."
	@time $(MAKE) all
	@echo "Parallel processing is MANDATORY for efficiency!"

# Git Management Commands (MANDATORY)
.PHONY: git-sync git-commit git-push git-pr git-clean git-status git-flow

git-sync:
	@python .git-commands/slash_commands.py sync

git-commit:
	@python .git-commands/slash_commands.py commit

git-push:
	@python .git-commands/slash_commands.py push

git-pr:
	@python .git-commands/slash_commands.py pr

git-clean:
	@python .git-commands/slash_commands.py clean

git-status:
	@python .git-commands/slash_commands.py status

git-flow:
	@python .git-commands/slash_commands.py flow

# Parallel Git operations for efficiency
git-parallel: parallel-check
	@echo "🚀 Running Git operations in parallel..."
	@$(MAKE) -j3 git-sync git-status git-clean
