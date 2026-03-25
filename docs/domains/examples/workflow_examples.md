# Workflow Templates - Example Usage

## Overview

The enhanced workflow templates system provides reusable patterns for common development tasks with:
- **Phase Dependencies**: Sequential and parallel execution with dependency management
- **Quality Gates**: Automatic code coverage, linting, and type checking
- **Agent Orchestration**: Coordinated multi-agent execution using Claude Code Task tool
- **Automatic Checkpointing**: Resume workflows from any checkpoint
- **Error Recovery**: Automatic retry and agent replacement on failures

## Quick Start

### 1. List Available Templates

```bash
# Using wrapper script
./scripts/workflow-cli.sh list

# Using uv directly
uv run python src/digitalmodel/cli/workflow_cli.py list
```

### 2. Execute a Workflow

```bash
# Full-stack development workflow
./scripts/workflow-cli.sh execute full_stack_development

# Bug fix workflow
./scripts/workflow-cli.sh execute bug_fix_workflow

# Refactoring workflow
./scripts/workflow-cli.sh execute refactoring_workflow
```

### 3. Check Workflow Status

```bash
# Get status of running workflow
./scripts/workflow-cli.sh status <workflow-id>

# JSON output for programmatic access
./scripts/workflow-cli.sh status <workflow-id> --json-output
```

### 4. Resume from Checkpoint

```bash
# Resume from specific checkpoint
./scripts/workflow-cli.sh execute full_stack_development \
  --checkpoint-id data/workflows/checkpoints/<workflow-id>/checkpoint_latest.json
```

## Example 1: Full-Stack Development Workflow

**Use Case**: Develop a new user authentication feature

### Workflow Steps

1. **Specification & Planning**
   - Requirements analyst analyzes authentication needs
   - Task planner creates detailed task breakdown

2. **Architecture Design**
   - System designer plans authentication architecture
   - Database architect designs user schema

3. **Backend Development** (Parallel)
   - API developer implements authentication endpoints
   - Business logic developer creates auth services
   - Database developer implements user models

4. **Frontend Development** (Parallel)
   - UI developer creates login/signup forms
   - State management developer integrates auth API

5. **Comprehensive Testing** (Parallel)
   - Unit tester achieves >85% coverage
   - Integration tester validates API endpoints
   - E2E tester tests authentication flows

6. **Code Review & QA**
   - Code reviewer checks quality and security
   - Security auditor performs vulnerability assessment

7. **Integration & Deployment Prep**
   - CI/CD engineer sets up pipeline
   - Integration developer integrates components

### Execution

```bash
# Execute workflow
./scripts/workflow-cli.sh execute full_stack_development \
  --workflow-id auth_feature_20260107 \
  --report-path reports/auth_feature_report.html

# Monitor progress
./scripts/workflow-cli.sh status auth_feature_20260107

# Resume if interrupted
./scripts/workflow-cli.sh execute full_stack_development \
  --checkpoint-id data/workflows/checkpoints/auth_feature_20260107/checkpoint_latest.json
```

### Quality Gates

- Code coverage must be >85%
- All linting checks must pass
- Type checking must pass
- Workflow halts on gate failure

### Expected Output

```
reports/
└── auth_feature_report.html     # Interactive HTML report with timeline

data/workflows/checkpoints/auth_feature_20260107/
├── checkpoint_phase_spec_start.json
├── checkpoint_phase_spec_complete.json
├── checkpoint_phase_architecture_start.json
├── checkpoint_phase_architecture_complete.json
├── ...
└── checkpoint_latest.json       # Most recent checkpoint
```

## Example 2: Bug Fix Workflow

**Use Case**: Fix authentication token expiration bug

### Workflow Steps

1. **Investigation & Root Cause Analysis**
   - Bug analyst reproduces and documents issue
   - Root cause analyzer identifies token validation bug

2. **Fix Implementation**
   - Bug fixer implements minimal fix

3. **Test & Verification** (Parallel)
   - Regression tester creates test for bug
   - Verification tester ensures no breakage

4. **Review & Approval**
   - Fix reviewer validates correctness

### Execution

```bash
# Execute bug fix workflow
./scripts/workflow-cli.sh execute bug_fix_workflow \
  --workflow-id token_bug_fix_20260107

# Check status
./scripts/workflow-cli.sh status token_bug_fix_20260107
```

### Quality Gates

- Code coverage must be >80%
- Linting must pass
- Type checking must pass

### Expected Duration

~20 minutes for typical bug fix

## Example 3: Refactoring Workflow

**Use Case**: Refactor authentication module to improve maintainability

### Workflow Steps

1. **Code Analysis & Planning**
   - Code quality analyzer identifies technical debt
   - Refactoring planner creates strategy

2. **Baseline Test Coverage**
   - Baseline tester ensures adequate test coverage

3. **Refactoring Implementation**
   - Refactoring specialist implements changes incrementally

4. **Verification & QA** (Parallel)
   - Regression verifier runs all tests
   - Quality reviewer checks improvements
   - Metrics analyzer measures improvements

### Execution

```bash
# Execute refactoring workflow
./scripts/workflow-cli.sh execute refactoring_workflow \
  --workflow-id auth_refactor_20260107

# Background mode for long-running refactoring
./scripts/workflow-cli.sh execute refactoring_workflow \
  --workflow-id auth_refactor_20260107 \
  --background
```

### Quality Gates

- Code coverage must maintain or improve (≥80%)
- Quality metrics must show improvement
- All tests must pass

### Expected Duration

~30 minutes for moderate refactoring

## Example 4: Research Workflow

**Use Case**: Research best practices for OAuth 2.0 implementation

### Workflow Steps

1. **Topic Definition**
   - Topic analyst defines research scope

2. **Information Gathering** (Parallel)
   - Literature researcher finds existing solutions
   - Best practices researcher identifies industry standards
   - Codebase analyzer examines current implementation

3. **Synthesis & Recommendations**
   - Synthesis specialist creates comprehensive report
   - Action planner provides implementation roadmap

### Execution

```bash
# Execute research workflow
./scripts/workflow-cli.sh execute research_workflow \
  --workflow-id oauth_research_20260107

# JSON output for automation
./scripts/workflow-cli.sh execute research_workflow \
  --workflow-id oauth_research_20260107 \
  --json-output
```

### Quality Gates

None (research workflow focuses on discovery, not code quality)

### Expected Duration

~25 minutes

## Example 5: Performance Optimization Workflow

**Use Case**: Optimize authentication API response times

### Workflow Steps

1. **Baseline Profiling**
   - Performance profiler identifies bottlenecks

2. **Bottleneck Analysis**
   - Bottleneck analyzer prioritizes optimizations
   - Optimization planner creates strategy

3. **Optimization Implementation** (Parallel)
   - Performance optimizer implements changes
   - Performance tester creates benchmarks

4. **Verification & Benchmarking**
   - Performance verifier measures improvements
   - Optimization reviewer checks correctness

### Execution

```bash
# Execute performance optimization
./scripts/workflow-cli.sh execute performance_optimization \
  --workflow-id auth_perf_opt_20260107 \
  --report-path reports/auth_performance_report.html
```

### Quality Gates

- Code coverage must maintain or improve (≥80%)
- Performance improvements must be measurable

### Expected Duration

~40 minutes

## Example 6: Multi-Repo Synchronization Workflow

**Use Case**: Synchronize all 26+ repositories in workspace-hub

### Workflow Steps

1. **Repository Discovery**
   - Repo scanner identifies all repositories

2. **Git Synchronization** (Parallel)
   - Git sync manager pulls latest changes

3. **Dependency Updates** (Parallel)
   - Dependency manager updates packages

4. **Verification**
   - Sync verifier confirms successful sync

### Execution

```bash
# Synchronize all repositories
./scripts/workflow-cli.sh execute multi_repo_sync \
  --workflow-id repo_sync_20260107

# Background mode (recommended for many repos)
./scripts/workflow-cli.sh execute multi_repo_sync \
  --workflow-id repo_sync_20260107 \
  --background
```

### Quality Gates

None (operational workflow)

### Expected Duration

~15 minutes for 26 repositories

## Advanced Features

### Custom Workflow IDs

```bash
# Use semantic workflow IDs
./scripts/workflow-cli.sh execute full_stack_development \
  --workflow-id feature_oauth_integration_v1
```

### Background Execution

```bash
# Run in background
./scripts/workflow-cli.sh execute full_stack_development \
  --background

# Poll status periodically
watch -n 5 './scripts/workflow-cli.sh status <workflow-id>'
```

### JSON Output for Automation

```bash
# Get JSON status for CI/CD integration
STATUS=$(./scripts/workflow-cli.sh status <workflow-id> --json-output)
echo $STATUS | jq '.status'
```

### Checkpoint Management

```bash
# List all checkpoints
./scripts/workflow-cli.sh checkpoints

# Resume from specific checkpoint
./scripts/workflow-cli.sh execute full_stack_development \
  --checkpoint-id data/workflows/checkpoints/<workflow-id>/checkpoint_phase_testing_complete.json
```

## Integration with SPARC Methodology

Workflows integrate seamlessly with SPARC:

```bash
# Use SPARC for specification phase
npx claude-flow sparc run specification "User authentication feature"

# Then execute workflow
./scripts/workflow-cli.sh execute full_stack_development
```

## Integration with Testing Framework

Quality gates automatically integrate with the testing framework:

- **Code Coverage**: Uses pytest-cov for coverage measurement
- **Linting**: Uses ruff for Python linting
- **Type Checking**: Uses mypy for static type analysis

## HTML Reports

Each workflow generates an interactive HTML report with:

- **Execution Metrics**: Status, duration, phase completion
- **Interactive Timeline**: Plotly-based phase execution visualization
- **Phase Details**: Agent execution details and status
- **Quality Gate Results**: Coverage, linting, type checking results

Reports follow HTML Reporting Standards:
- Interactive Plotly visualizations
- Relative CSV data imports
- Professional styling
- Responsive design

## Troubleshooting

### Workflow Fails at Quality Gate

```bash
# Check which gate failed
./scripts/workflow-cli.sh status <workflow-id> --json-output | jq '.phases[] | select(.status == "failed")'

# Fix issues and resume
# (fix code coverage, linting, or type checking)
./scripts/workflow-cli.sh execute <template> --checkpoint-id <checkpoint-path>
```

### Agent Failure

Workflows automatically:
1. Retry same agent once
2. Spawn replacement agent
3. Fail workflow after 2 failures per phase

### Resume from Checkpoint

```bash
# Find latest checkpoint
ls data/workflows/checkpoints/<workflow-id>/

# Resume from latest
./scripts/workflow-cli.sh execute <template> \
  --checkpoint-id data/workflows/checkpoints/<workflow-id>/checkpoint_latest.json
```

## Best Practices

1. **Use Semantic Workflow IDs**: Name workflows descriptively
2. **Monitor Long-Running Workflows**: Use background mode and status polling
3. **Review HTML Reports**: Check execution timeline and metrics
4. **Checkpoint Frequently**: Checkpoints save after each phase
5. **Fix Quality Gates Immediately**: Don't bypass quality requirements
6. **Clean Up Old Checkpoints**: Remove completed workflow checkpoints periodically

## Next Steps

- Create custom workflow templates in `config/workflow-templates-enhanced.yaml`
- Integrate workflows into CI/CD pipelines
- Extend quality gates with custom checks
- Add custom agent types for specialized tasks
