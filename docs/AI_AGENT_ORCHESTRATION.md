# AI Agent Orchestration System

> **Version:** 1.0.0
> **Last Updated:** 2025-10-03
> **Status:** Production Ready

## Overview

The AI Agent Orchestration System provides intelligent multi-agent collaboration across all 26 repositories in workspace-hub. It automatically selects the best AI agent for each task, implements gate-pass reviews at critical checkpoints, and maintains daily capability updates to ensure optimal performance.

## Key Features

### 🤖 Multi-Agent Registry

- **10+ Specialized Agents** across 4 platforms (Claude Code, Factory.ai, Claude Flow, Spec-Kit, Agent OS)
- **Daily Capability Updates** to stay current with platform improvements
- **Intelligent Agent Selection** based on task type, complexity, and domain
- **Performance Tracking** with automatic score adjustments

### 🚪 Gate-Pass Review System

- **7 SPARC Phases** with automated quality checkpoints
- **Multi-Agent Review** for comprehensive validation
- **Configurable Pass Criteria** per phase and checkpoint
- **Automated Approval** when criteria met

### 🔄 Workflow Orchestration

- **5 Pre-Built Workflows** for common development patterns
- **Platform Integration** with factory.ai, claude-flow, spec-kit, agent-os
- **Coordination Hooks** for seamless multi-agent collaboration
- **Memory Persistence** across agent sessions

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                   AI Agent Orchestrator                      │
│  (modules/automation/agent_orchestrator.sh)                  │
└──────────────────┬──────────────────────────────────────────┘
                   │
        ┌──────────┴──────────┐
        │                     │
        ▼                     ▼
┌──────────────┐      ┌──────────────┐
│  Agent       │      │  Gate-Pass   │
│  Registry    │◄────►│  Reviews     │
│  (JSON)      │      │  (Bash)      │
└──────────────┘      └──────────────┘
        │                     │
        └──────────┬──────────┘
                   │
        ┌──────────┴──────────┐
        │                     │
        ▼                     ▼
┌──────────────┐      ┌──────────────┐
│  Daily       │      │  Workflow    │
│  Updates     │      │  Templates   │
│  (Bash)      │      │  (JSON)      │
└──────────────┘      └──────────────┘
```

## Installation

The AI Agent Orchestration System is pre-installed across all 26 repositories in workspace-hub.

### Verify Installation

```bash
# Check registry exists
ls -la modules/config/ai-agents-registry.json

# Check scripts are executable
ls -la modules/automation/agent_orchestrator.sh
ls -la modules/automation/gate_pass_review.sh
ls -la modules/automation/update_ai_agents_daily.sh
```

### Setup Daily Updates

Add to crontab for automatic daily updates:

```bash
# Edit crontab
crontab -e

# Add this line (runs at midnight UTC daily)
0 0 * * * /mnt/github/workspace-hub/modules/automation/update_ai_agents_daily.sh
```

## Usage

### 1. Agent Orchestration

Select and execute the best agent for your task:

```bash
# Basic usage
./modules/automation/agent_orchestrator.sh <task-type> "<description>"

# With automated review
./modules/automation/agent_orchestrator.sh code-generation \
  "Create user authentication API" \
  --with-review

# Specify complexity
./modules/automation/agent_orchestrator.sh code-refactoring \
  "Optimize database queries" \
  --complexity complex

# Force specific agent
./modules/automation/agent_orchestrator.sh test-creation \
  "Add unit tests for payment module" \
  --agent claude-flow-tester

# Specify domain
./modules/automation/agent_orchestrator.sh architecture-design \
  "Design microservices architecture" \
  --domain python \
  --output report.md
```

### 2. Gate-Pass Reviews

Run automated quality checkpoints:

```bash
# Review specification phase
./modules/automation/gate_pass_review.sh specification . --auto

# Review implementation with report
./modules/automation/gate_pass_review.sh implementation . \
  --report review-report.md

# Verbose output
./modules/automation/gate_pass_review.sh testing . \
  --verbose

# All phases
for phase in specification pseudocode architecture implementation testing refinement completion; do
  ./modules/automation/gate_pass_review.sh $phase .
done
```

### 3. Daily Registry Updates

Update agent capabilities manually or via cron:

```bash
# Manual update
./modules/automation/update_ai_agents_daily.sh

# Check update log
tail -f modules/config/agent-updates.log

# View latest report
ls -lt modules/config/agent-update-report-*.md | head -1 | xargs cat
```

## Agent Registry

### Available Agents

#### Primary Agents

1. **claude-sonnet-4.5** (Claude Code)
   - **Best For:** Complex tasks, multi-step workflows, orchestration
   - **Capabilities:** Code generation (95), Architecture (92), Documentation (93)
   - **Cost:** High

2. **factory-ai-droid** (Factory.ai)
   - **Best For:** Automated refactoring, bulk updates, code migration
   - **Capabilities:** Refactoring (92), Migration (90), Code generation (88)
   - **Cost:** Medium

3. **claude-flow-coder** (Claude Flow)
   - **Best For:** TDD workflows, test-first development, SPARC phases
   - **Capabilities:** TDD implementation (93), Test writing (91), Code generation (90)
   - **Cost:** Low

#### Specialist Agents

4. **claude-flow-reviewer** - Code review and quality analysis
5. **claude-flow-tester** - Test generation and coverage analysis
6. **claude-flow-architect** - System design and architecture review
7. **claude-flow-researcher** - Requirements analysis and research
8. **spec-kit-analyzer** - Spec evolution and validation
9. **agent-os-planner** - Workflow planning and orchestration

### Agent Selection Algorithm

The orchestrator selects agents based on:

1. **Task Type** - Matches agent specialization
2. **Complexity** - Simple → Low-cost agents, Complex → High-capability agents
3. **Domain** - Language/framework expertise
4. **Past Performance** - Historical success rates
5. **Cost Efficiency** - Balances quality and cost
6. **Availability** - Platform and tool availability

## Gate-Pass Review Matrix

### SPARC Phases

| Phase | Primary Reviewer | Secondary Reviewer | Min Score |
|-------|-----------------|-------------------|-----------|
| Specification | claude-flow-researcher | spec-kit-analyzer | 90% |
| Pseudocode | claude-flow-architect | claude-sonnet-4.5 | 85% |
| Architecture | claude-flow-architect | claude-sonnet-4.5 | 88% |
| Implementation | claude-flow-reviewer | claude-sonnet-4.5 | 90% |
| Testing | claude-flow-tester | claude-flow-reviewer | 90% |
| Refinement | claude-flow-reviewer | factory-ai-droid | 85% |
| Completion | claude-sonnet-4.5 | claude-flow-reviewer | 92% |

### Checkpoints by Phase

#### Specification Phase
- ✓ Requirements completeness
- ✓ Feasibility analysis
- ✓ Spec validation
- ✓ Stakeholder alignment

#### Implementation Phase
- ✓ Code quality
- ✓ Test coverage
- ✓ Style compliance
- ✓ Best practices
- ✓ Security vulnerabilities

#### Testing Phase
- ✓ Test coverage (90%+)
- ✓ Edge cases
- ✓ Integration tests
- ✓ Test quality

## Workflow Templates

### 1. Complete SPARC Workflow

Full SPARC methodology with gate-pass reviews:

```bash
# Specification phase
./modules/automation/agent_orchestrator.sh spec-creation \
  "Design payment processing feature" \
  --with-review

./modules/automation/gate_pass_review.sh specification .

# Pseudocode phase
./modules/automation/agent_orchestrator.sh architecture-design \
  "Algorithm for payment processing"

./modules/automation/gate_pass_review.sh pseudocode .

# Architecture phase
./modules/automation/agent_orchestrator.sh architecture-design \
  "Payment processing system architecture"

./modules/automation/gate_pass_review.sh architecture .

# Implementation (TDD)
npx claude-flow sparc tdd "payment processing"

./modules/automation/gate_pass_review.sh implementation .

# Testing
./modules/automation/agent_orchestrator.sh test-creation \
  "Comprehensive payment processing tests" \
  --with-review

./modules/automation/gate_pass_review.sh testing .

# Refinement
./modules/automation/agent_orchestrator.sh code-refactoring \
  "Optimize payment processing performance"

./modules/automation/gate_pass_review.sh refinement .

# Completion
./modules/automation/gate_pass_review.sh completion .
```

### 2. Feature Development Workflow

Rapid feature development with factory.ai and claude-flow:

```bash
# 1. Planning (Agent OS)
# Follow: @~/.agent-os/instructions/create-spec.md

# 2. Implementation (Factory.ai)
droid exec "implement user authentication with JWT"

# 3. Review (Claude Flow)
./modules/automation/gate_pass_review.sh implementation .

# 4. Testing
./modules/automation/agent_orchestrator.sh test-creation \
  "Authentication tests with 90% coverage" \
  --with-review

# 5. Deploy
./modules/automation/gate_pass_review.sh completion .
```

### 3. Refactoring Workflow

Safe refactoring with automated testing:

```bash
# 1. Analysis
./modules/automation/agent_orchestrator.sh code-review \
  "Analyze code quality and refactoring opportunities"

# 2. Establish test baseline
./modules/automation/agent_orchestrator.sh test-creation \
  "Comprehensive tests for current behavior"

./modules/automation/gate_pass_review.sh testing .

# 3. Refactor
droid exec "refactor user service to use dependency injection"

# 4. Validate
./modules/automation/gate_pass_review.sh refinement .

# 5. Final review
./modules/automation/gate_pass_review.sh completion .
```

### 4. Bug Fix Workflow

Systematic bug fixing with root cause analysis:

```bash
# 1. Reproduction
./modules/automation/agent_orchestrator.sh bug-fixing \
  "Reproduce and document bug #123"

# 2. Root cause analysis
./modules/automation/agent_orchestrator.sh requirement-analysis \
  "Analyze root cause of authentication failure"

# 3. Fix implementation
droid exec "fix authentication token validation bug"

./modules/automation/gate_pass_review.sh implementation .

# 4. Prevention
./modules/automation/agent_orchestrator.sh test-creation \
  "Add regression tests for bug #123"

./modules/automation/gate_pass_review.sh testing .
```

### 5. Documentation Workflow

Comprehensive documentation creation:

```bash
# 1. Planning
./modules/automation/agent_orchestrator.sh documentation \
  "Plan API documentation structure"

# 2. Generation
droid exec "generate API documentation from code comments"

# 3. Validation
./modules/automation/gate_pass_review.sh completion .
```

## Integration Patterns

### Factory.ai + Claude Flow

Combine factory.ai's bulk operations with claude-flow's coordination:

```bash
# 1. Initialize coordination
npx claude-flow@alpha swarm init --topology mesh

# 2. Bulk operation with factory.ai
droid exec "refactor all Python files to use async/await"

# 3. Review with claude-flow agents
./modules/automation/gate_pass_review.sh refinement . --auto

# 4. Orchestrate PR creation
npx claude-flow@alpha hooks post-task
```

### Spec-Kit + Agent OS

Evolve specs and execute with agent-os:

```bash
# 1. Create initial spec with agent-os
# @~/.agent-os/instructions/create-spec.md

# 2. Validate with spec-kit
specify validate .agent-os/specs/[spec-name]/spec.md

# 3. Evolve based on feedback
specify evolve .agent-os/specs/[spec-name]/spec.md \
  --feedback "Add Stripe integration"

# 4. Execute tasks
# @~/.agent-os/instructions/execute-tasks.md

# 5. Update spec status
specify update .agent-os/specs/[spec-name]/spec.md --status completed
```

### Multi-Agent Review

Multiple agents review from different perspectives:

```bash
# Sequential specialized reviews
./modules/automation/agent_orchestrator.sh code-review \
  "Review for general quality" \
  --agent claude-flow-reviewer

./modules/automation/agent_orchestrator.sh security-audit \
  "Review for security vulnerabilities" \
  --agent claude-flow-reviewer

./modules/automation/agent_orchestrator.sh performance-opt \
  "Review for performance optimization" \
  --agent claude-sonnet-4.5

./modules/automation/agent_orchestrator.sh architecture-design \
  "Review design patterns and architecture" \
  --agent claude-flow-architect
```

## Configuration

### Agent Registry Configuration

Edit `modules/config/ai-agents-registry.json`:

```json
{
  "agents": {
    "custom-agent": {
      "platform": "custom",
      "type": "specialist",
      "capabilities": {
        "domain-specific": {
          "score": 90,
          "domains": ["oil-gas", "petroleum"]
        }
      },
      "bestFor": ["industry-specific-tasks"],
      "costTier": "medium"
    }
  }
}
```

### Gate-Pass Criteria

Adjust pass criteria per phase:

```json
{
  "gatePassReviewMatrix": {
    "implementation-phase": {
      "passCriteria": {
        "code-quality": 95,  // Increase from 90
        "test-coverage": 90,  // Increase from 85
        "security": 98        // Increase from 95
      }
    }
  }
}
```

### Workflow Customization

Create custom workflows in `modules/config/workflow-templates.json`:

```json
{
  "workflows": {
    "custom-workflow": {
      "name": "Custom Development Workflow",
      "phases": [
        {
          "name": "custom-phase",
          "agents": {
            "primary": "custom-agent",
            "support": ["claude-sonnet-4.5"]
          },
          "gatePass": {
            "enabled": true,
            "reviewer": "claude-flow-reviewer",
            "minScore": 90
          }
        }
      ]
    }
  }
}
```

## Best Practices

### 1. Always Use Gate-Pass Reviews

Run gate-pass reviews at the end of each SPARC phase:

```bash
# After each phase
./modules/automation/gate_pass_review.sh <phase> . --auto
```

### 2. Let the Orchestrator Choose

Trust the intelligent agent selection:

```bash
# Good - let orchestrator decide
./modules/automation/agent_orchestrator.sh code-generation "Create API"

# Only force agent if you have specific requirements
./modules/automation/agent_orchestrator.sh code-generation "Create API" \
  --agent factory-ai-droid  # Only when necessary
```

### 3. Review Before Committing

Always review AI-generated changes before committing:

```bash
# Review changes
git diff

# Run gate-pass review
./modules/automation/gate_pass_review.sh implementation .

# Commit only after approval
git add . && git commit -m "Add feature X"
```

### 4. Monitor Daily Updates

Check daily update reports weekly:

```bash
# View latest update report
ls -lt modules/config/agent-update-report-*.md | head -1 | xargs cat

# Check update log
tail -20 modules/config/agent-updates.log
```

### 5. Use Workflows for Complex Tasks

For multi-step tasks, follow pre-built workflows:

```bash
# Complete SPARC workflow for new features
# See: Workflow Templates → Complete SPARC Workflow

# Feature development for rapid iteration
# See: Workflow Templates → Feature Development Workflow
```

## Troubleshooting

### Agent Selection Issues

**Problem:** Orchestrator selects wrong agent for task

**Solution:**
```bash
# Force specific agent
./modules/automation/agent_orchestrator.sh <task-type> "<description>" \
  --agent <agent-name>

# Or adjust complexity
./modules/automation/agent_orchestrator.sh <task-type> "<description>" \
  --complexity complex
```

### Gate-Pass Review Failures

**Problem:** Gate-pass review consistently fails

**Solution:**
```bash
# Run with verbose output
./modules/automation/gate_pass_review.sh <phase> . \
  --verbose \
  --report review-details.md

# Review detailed report
cat review-details.md

# Address specific checkpoint failures
# Re-run review after fixes
```

### Registry Sync Issues

**Problem:** Registry not synced across repositories

**Solution:**
```bash
# Manually run daily update
./modules/automation/update_ai_agents_daily.sh

# Verify sync
for dir in */; do
  if [ -d "${dir}.git" ]; then
    echo "=== ${dir%/} ==="
    ls -la "${dir}modules/config/ai-agents-registry.json"
  fi
done
```

### Platform Not Available

**Problem:** Required platform (factory.ai, claude-flow) not installed

**Solution:**
```bash
# Check factory.ai
droid --version

# Check claude-flow
npx claude-flow@alpha --version

# Check spec-kit
specify --version

# Install if missing - see platform documentation
```

## Advanced Usage

### Custom Agent Coordination

Create multi-agent swarms for complex tasks:

```bash
# Initialize swarm with specific topology
npx claude-flow@alpha swarm init --topology hierarchical

# Spawn multiple agents via Claude Code Task tool
# [Single Message in Claude Code]:
Task("Backend Developer", "Build REST API", "backend-dev")
Task("Frontend Developer", "Create React UI", "coder")
Task("Test Engineer", "Write comprehensive tests", "tester")
Task("Reviewer", "Review all code", "reviewer")

# Run coordination hooks
npx claude-flow@alpha hooks session-end --export-metrics true
```

### Performance Optimization

Track and optimize agent performance:

```bash
# Check agent metrics
jq '.agents | to_entries[] | {agent: .key, avgScore: (.value.capabilities | to_entries | map(.value.score) | add / length)}' \
  modules/config/ai-agents-registry.json

# Review performance logs
ls -la .claude-flow/metrics/

# Analyze bottlenecks
npx claude-flow@alpha swarm status
```

### Cross-Repository Orchestration

Apply workflows across multiple repositories:

```bash
# Create orchestration script
for repo in repo1 repo2 repo3; do
  cd "$repo"

  ./modules/automation/agent_orchestrator.sh code-refactoring \
    "Standardize error handling" \
    --with-review

  ./modules/automation/gate_pass_review.sh refinement . --auto

  cd ..
done
```

## Resources

### Documentation

- **Agent Registry:** `modules/config/ai-agents-registry.json`
- **Workflow Templates:** `modules/config/workflow-templates.json`
- **Update Logs:** `modules/config/agent-updates.log`
- **Update Reports:** `modules/config/agent-update-report-*.md`

### Scripts

- **Orchestrator:** `modules/automation/agent_orchestrator.sh`
- **Gate-Pass Review:** `modules/automation/gate_pass_review.sh`
- **Daily Update:** `modules/automation/update_ai_agents_daily.sh`

### Platform Documentation

- **Claude Flow:** https://github.com/ruvnet/claude-flow
- **Factory.ai:** https://factory.ai/docs
- **Spec-Kit:** https://github.com/github/spec-kit
- **Agent OS:** https://buildermethods.com/agent-os

### Related Guides

- **AI Ecosystem:** `docs/AI_ECOSYSTEM.md`
- **Factory.ai Guide:** `docs/FACTORY_AI_GUIDE.md`
- **Factory.ai Quick Start:** `docs/FACTORY_AI_QUICK_START.md`

## Version History

### 1.0.0 (2025-10-03)

- Initial release
- 10+ specialized agents across 4 platforms
- 7 SPARC phase gate-pass reviews
- 5 pre-built workflow templates
- Daily agent capability updates
- Intelligent agent selection algorithm
- Multi-agent review system
- Platform integration (factory.ai, claude-flow, spec-kit, agent-os)

---

**Remember:** The orchestration system learns and improves with daily updates. Trust the intelligent agent selection, use gate-pass reviews religiously, and follow pre-built workflows for consistent, high-quality results! 🚀
