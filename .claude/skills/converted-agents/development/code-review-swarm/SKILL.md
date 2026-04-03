---
name: code-review-swarm
version: 1.0.0
category: development
description: Deploy specialized AI agents to perform comprehensive, intelligent code reviews that go beyond traditional static analysis
type: reference
tags: []
scripts_exempt: true
---
# Code Review Swarm

# Code Review Swarm - Automated Code Review with AI Agents

## Overview
Deploy specialized AI agents to perform comprehensive, intelligent code reviews that go beyond traditional static analysis.

## Core Features

### 1. Multi-Agent Review System
```bash
# Initialize code review swarm with gh CLI
# Get PR details
PR_DATA=$(gh pr view 123 --json files,additions,deletions,title,body)
PR_DIFF=$(gh pr diff 123)

# Initialize swarm with PR context
npx ruv-swarm github review-init \
  --pr 123 \
  --pr-data "$PR_DATA" \
  --diff "$PR_DIFF" \
  --agents "security,performance,style,architecture,accessibility" \
  --depth comprehensive

# Post initial review status
gh pr comment 123 --body "🔍 Multi-agent code review initiated"
```

### 2. Specialized Review Agents

#### Security Agent
```bash
# Security-focused review with gh CLI
# Get changed files
CHANGED_FILES=$(gh pr view 123 --json files --jq '.files[].path')

# Run security review
SECURITY_RESULTS=$(npx ruv-swarm github review-security \
  --pr 123 \
  --files "$CHANGED_FILES" \
  --check "owasp,cve,secrets,permissions" \
  --suggest-fixes)

# Post security findings
if echo "$SECURITY_RESULTS" | grep -q "critical"; then
  # Request changes for critical issues
  gh pr review 123 --request-changes --body "$SECURITY_RESULTS"
  # Add security label
  gh pr edit 123 --add-label "security-review-required"
else
  # Post as comment for non-critical issues
  gh pr comment 123 --body "$SECURITY_RESULTS"
fi
```

#### Performance Agent
```bash
# Performance analysis
npx ruv-swarm github review-performance \
  --pr 123 \
  --profile "cpu,memory,io" \
  --benchmark-against main \
  --suggest-optimizations
```

#### Architecture Agent
```bash
# Architecture review
npx ruv-swarm github review-architecture \
  --pr 123 \
  --check "patterns,coupling,cohesion,solid" \
  --visualize-impact \
  --suggest-refactoring
```

### 3. Review Configuration
```yaml
# .github/review-swarm.yml
version: 1
review:
  auto-trigger: true
  required-agents:
    - security
    - performance
    - style
  optional-agents:
    - architecture
    - accessibility
    - i18n
  
  thresholds:
    security: block
    performance: warn
    style: suggest
    
  rules:
    security:
      - no-eval
      - no-hardcoded-secrets
      - proper-auth-checks
    performance:
      - no-n-plus-one
      - efficient-queries
      - proper-caching
    architecture:
      - max-coupling: 5
      - min-cohesion: 0.7
      - follow-patterns
```

## Review Agents

### Security Review Agent
```javascript
// Security checks performed
{
  "checks": [
    "SQL injection vulnerabilities",
    "XSS attack vectors",
    "Authentication bypasses",
    "Authorization flaws",
    "Cryptographic weaknesses",
    "Dependency vulnerabilities",
    "Secret exposure",
    "CORS misconfigurations"
  ],
  "actions": [
    "Block PR on critical issues",
    "Suggest secure alternatives",
    "Add security test cases",
    "Update security documentation"
  ]
}
```

### Performance Review Agent
```javascript
// Performance analysis
{
  "metrics": [
    "Algorithm complexity",
    "Database query efficiency",
    "Memory allocation patterns",
    "Cache utilization",
    "Network request optimization",
    "Bundle size impact",
    "Render performance"
  ],
  "benchmarks": [
    "Compare with baseline",
    "Load test simulations",
    "Memory leak detection",
    "Bottleneck identification"
  ]
}
```

### Style & Convention Agent
```javascript
// Style enforcement
{
  "checks": [
    "Code formatting",
    "Naming conventions",
    "Documentation standards",
    "Comment quality",
    "Test coverage",
    "Error handling patterns",
    "Logging standards"
  ],
  "auto-fix": [
    "Formatting issues",
    "Import organization",
    "Trailing whitespace",
    "Simple naming issues"
  ]
}
```

### Architecture Review Agent
```javascript
// Architecture analysis
{
  "patterns": [
    "Design pattern adherence",
    "SOLID principles",
    "DRY violations",
    "Separation of concerns",
    "Dependency injection",
    "Layer violations",
    "Circular dependencies"
  ],
  "metrics": [
    "Coupling metrics",
    "Cohesion scores",
    "Complexity measures",
    "Maintainability index"
  ]
}
```

## Advanced Review Features

### 1. Context-Aware Reviews
```bash
# Review with full context
npx ruv-swarm github review-context \
  --pr 123 \
  --load-related-prs \
  --analyze-impact \
  --check-breaking-changes
```

### 2. Learning from History
```bash
# Learn from past reviews
npx ruv-swarm github review-learn \
  --analyze-past-reviews \
  --identify-patterns \
  --improve-suggestions \
  --reduce-false-positives
```

### 3. Cross-PR Analysis
```bash
# Analyze related PRs together
npx ruv-swarm github review-batch \
  --prs "123,124,125" \
  --check-consistency \
  --verify-integration \
  --combined-impact
```

## Review Automation

### Auto-Review on Push
```yaml
# .github/workflows/auto-review.yml
name: Automated Code Review
on:
  pull_request:
    types: [opened, synchronize]

jobs:
  swarm-review:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
        with:
          fetch-depth: 0
          
      - name: Setup GitHub CLI
        run: echo "${{ secrets.GITHUB_TOKEN }}" | gh auth login --with-token
          
      - name: Run Review Swarm
        run: |
          # Get PR context with gh CLI
          PR_NUM=${{ github.event.pull_request.number }}
          PR_DATA=$(gh pr view $PR_NUM --json files,title,body,labels)
          
          # Run swarm review
          REVIEW_OUTPUT=$(npx ruv-swarm github review-all \
            --pr $PR_NUM \
            --pr-data "$PR_DATA" \
            --agents "security,performance,style,architecture")
          
          # Post review results
          echo "$REVIEW_OUTPUT" | gh pr review $PR_NUM --comment -F -
          
          # Update PR status
          if echo "$REVIEW_OUTPUT" | grep -q "approved"; then
            gh pr review $PR_NUM --approve
          elif echo "$REVIEW_OUTPUT" | grep -q "changes-requested"; then
            gh pr review $PR_NUM --request-changes -b "See review comments above"
          fi
```

### Review Triggers
```javascript
// Custom review triggers
{
  "triggers": {
    "high-risk-files": {
      "paths": ["**/auth/**", "**/payment/**"],
      "agents": ["security", "architecture"],
      "depth": "comprehensive"
    },
    "performance-critical": {
      "paths": ["**/api/**", "**/database/**"],
      "agents": ["performance", "database"],
      "benchmarks": true
    },
    "ui-changes": {
      "paths": ["**/components/**", "**/styles/**"],
      "agents": ["accessibility", "style", "i18n"],
      "visual-tests": true
    }
  }
}
```

## Review Comments

### Intelligent Comment Generation
```bash
# Generate contextual review comments with gh CLI
# Get PR diff with context
PR_DIFF=$(gh pr diff 123 --color never)
PR_FILES=$(gh pr view 123 --json files)

# Generate review comments
COMMENTS=$(npx ruv-swarm github review-comment \
  --pr 123 \
  --diff "$PR_DIFF" \
  --files "$PR_FILES" \
  --style "constructive" \
  --include-examples \
  --suggest-fixes)

# Post comments using gh CLI
echo "$COMMENTS" | jq -c '.[]' | while read -r comment; do
  FILE=$(echo "$comment" | jq -r '.path')
  LINE=$(echo "$comment" | jq -r '.line')
  BODY=$(echo "$comment" | jq -r '.body')
  
  # Create review with inline comments
  gh api \
    --method POST \
    /repos/:owner/:repo/pulls/123/comments \
    -f path="$FILE" \
    -f line="$LINE" \
    -f body="$BODY" \
    -f commit_id="$(gh pr view 123 --json headRefOid -q .headRefOid)"
done
```

### Comment Templates
```markdown
<!-- Security Issue Template -->
🔒 **Security Issue: [Type]**

**Severity**: 🔴 Critical / 🟡 High / 🟢 Low

**Description**: 
[Clear explanation of the security issue]

**Impact**:
[Potential consequences if not addressed]

**Suggested Fix**:
```language
[Code example of the fix]
```

**References**:
- [OWASP Guide](link)
- [Security Best Practices](link)
```

### Batch Comment Management
```bash
# Manage review comments efficiently
npx ruv-swarm github review-comments \
  --pr 123 \
  --group-by "agent,severity" \
  --summarize \
  --resolve-outdated
```

## Integration with CI/CD

### Status Checks
```yaml
# Required status checks
protection_rules:
  required_status_checks:
    contexts:
      - "review-swarm/security"
      - "review-swarm/performance"
      - "review-swarm/architecture"
```

### Quality Gates
```bash
# Define quality gates
npx ruv-swarm github quality-gates \
  --define '{
    "security": {"threshold": "no-critical"},
    "performance": {"regression": "<5%"},
    "coverage": {"minimum": "80%"},
    "architecture": {"complexity": "<10"}
  }'
```

### Review Metrics
```bash
# Track review effectiveness
npx ruv-swarm github review-metrics \
  --period 30d \
  --metrics "issues-found,false-positives,fix-rate" \
  --export-dashboard
```

## Best Practices

### 1. Review Configuration
- Define clear review criteria
- Set appropriate thresholds
- Configure agent specializations
- Establish override procedures

### 2. Comment Quality
- Provide actionable feedback
- Include code examples
- Reference documentation
- Maintain respectful tone

### 3. Performance
- Cache analysis results
- Incremental reviews for large PRs
- Parallel agent execution
- Smart comment batching

## Advanced Features

### 1. AI Learning
```bash
# Train on your codebase
npx ruv-swarm github review-train \
  --learn-patterns \
  --adapt-to-style \
  --improve-accuracy
```

### 2. Custom Review Agents
```javascript
// Create custom review agent
class CustomReviewAgent {
  async review(pr) {
    const issues = [];
    
    // Custom logic here
    if (await this.checkCustomRule(pr)) {
      issues.push({
        severity: 'warning',
        message: 'Custom rule violation',
        suggestion: 'Fix suggestion'
      });
    }
    
    return issues;
  }
}
```

### 3. Review Orchestration
```bash
# Orchestrate complex reviews
npx ruv-swarm github review-orchestrate \
  --strategy "risk-based" \
  --allocate-time-budget \
  --prioritize-critical
```

## Examples

### Security-Critical PR
```bash
# Auth system changes
npx ruv-swarm github review-init \
  --pr 456 \
  --agents "security,authentication,audit" \
  --depth "maximum" \
  --require-security-approval
```

### Performance-Sensitive PR
```bash
# Database optimization
npx ruv-swarm github review-init \
  --pr 789 \
  --agents "performance,database,caching" \
  --benchmark \
  --profile
```

### UI Component PR
```bash
# New component library
npx ruv-swarm github review-init \
  --pr 321 \
  --agents "accessibility,style,i18n,docs" \
  --visual-regression \
  --component-tests
```

## Monitoring & Analytics

### Review Dashboard
```bash
# Launch review dashboard
npx ruv-swarm github review-dashboard \
  --real-time \
  --show "agent-activity,issue-trends,fix-rates"
```

### Review Reports
```bash
# Generate review reports
npx ruv-swarm github review-report \
  --format "markdown" \
  --include "summary,details,trends" \
  --email-stakeholders
```

See also: [swarm-pr.md](./swarm-pr.md), [workflow-automation.md](./workflow-automation.md)


---

## Source: github-modes.md

# GitHub Integration Modes

## Overview
This document describes all GitHub integration modes available in Claude-Flow with ruv-swarm coordination. Each mode is optimized for specific GitHub workflows and includes batch tool integration for maximum efficiency.

## GitHub Workflow Modes

### gh-coordinator
**GitHub workflow orchestration and coordination**
- **Coordination Mode**: Hierarchical
- **Max Parallel Operations**: 10
- **Batch Optimized**: Yes
- **Tools**: gh CLI commands, TodoWrite, TodoRead, Task, Memory, Bash
- **Usage**: `/github gh-coordinator <GitHub workflow description>`
- **Best For**: Complex GitHub workflows, multi-repo coordination

### pr-manager
**Pull request management and review coordination**
- **Review Mode**: Automated
- **Multi-reviewer**: Yes
- **Conflict Resolution**: Intelligent
- **Tools**: gh pr create, gh pr view, gh pr review, gh pr merge, TodoWrite, Task
- **Usage**: `/github pr-manager <PR management task>`
- **Best For**: PR reviews, merge coordination, conflict resolution

### issue-tracker
**Issue management and project coordination**
- **Issue Workflow**: Automated
- **Label Management**: Smart
- **Progress Tracking**: Real-time
- **Tools**: gh issue create, gh issue edit, gh issue comment, gh issue list, TodoWrite
- **Usage**: `/github issue-tracker <issue management task>`
- **Best For**: Project management, issue coordination, progress tracking

### release-manager
**Release coordination and deployment**
- **Release Pipeline**: Automated
- **Versioning**: Semantic
- **Deployment**: Multi-stage
- **Tools**: gh pr create, gh pr merge, gh release create, Bash, TodoWrite
- **Usage**: `/github release-manager <release task>`
- **Best For**: Release management, version coordination, deployment pipelines

## Repository Management Modes

### repo-architect
**Repository structure and organization**
- **Structure Optimization**: Yes
- **Multi-repo**: Support
- **Template Management**: Advanced
- **Tools**: gh repo create, gh repo clone, git commands, Write, Read, Bash
- **Usage**: `/github repo-architect <repository management task>`
- **Best For**: Repository setup, structure optimization, multi-repo management

### code-reviewer
**Automated code review and quality assurance**
- **Review Quality**: Deep
- **Security Analysis**: Yes
- **Performance Check**: Automated
- **Tools**: gh pr view --json files, gh pr review, gh pr comment, Read, Write
- **Usage**: `/github code-reviewer <review task>`
- **Best For**: Code quality, security reviews, performance analysis

### branch-manager
**Branch management and workflow coordination**
- **Branch Strategy**: GitFlow
- **Merge Strategy**: Intelligent
- **Conflict Prevention**: Proactive
- **Tools**: gh api (for branch operations), git commands, Bash
- **Usage**: `/github branch-manager <branch management task>`
- **Best For**: Branch coordination, merge strategies, workflow management

## Integration Commands

### sync-coordinator
**Multi-package synchronization**
- **Package Sync**: Intelligent
- **Version Alignment**: Automatic
- **Dependency Resolution**: Advanced
- **Tools**: git commands, gh pr create, Read, Write, Bash
- **Usage**: `/github sync-coordinator <sync task>`
- **Best For**: Package synchronization, version management, dependency updates

### ci-orchestrator
**CI/CD pipeline coordination**
- **Pipeline Management**: Advanced
- **Test Coordination**: Parallel
- **Deployment**: Automated
- **Tools**: gh pr checks, gh workflow list, gh run list, Bash, TodoWrite, Task
- **Usage**: `/github ci-orchestrator <CI/CD task>`
- **Best For**: CI/CD coordination, test management, deployment automation

### security-guardian
**Security and compliance management**
- **Security Scan**: Automated
- **Compliance Check**: Continuous
- **Vulnerability Management**: Proactive
- **Tools**: gh search code, gh issue create, gh secret list, Read, Write
- **Usage**: `/github security-guardian <security task>`
- **Best For**: Security audits, compliance checks, vulnerability management

## Usage Examples

### Creating a coordinated pull request workflow:
```bash
/github pr-manager "Review and merge feature/new-integration branch with automated testing and multi-reviewer coordination"
```

### Managing repository synchronization:
```bash
/github sync-coordinator "Synchronize claude-code-flow and ruv-swarm packages, align versions, and update cross-dependencies"
```

### Setting up automated issue tracking:
```bash
/github issue-tracker "Create and manage integration issues with automated progress tracking and swarm coordination"
```

## Batch Operations

All GitHub modes support batch operations for maximum efficiency:

### Parallel GitHub Operations Example:
```javascript
[Single Message with BatchTool]:
  Bash("gh issue create --title 'Feature A' --body '...'")
  Bash("gh issue create --title 'Feature B' --body '...'")
  Bash("gh pr create --title 'PR 1' --head 'feature-a' --base 'main'")
  Bash("gh pr create --title 'PR 2' --head 'feature-b' --base 'main'")
  TodoWrite { todos: [todo1, todo2, todo3] }
  Bash("git checkout main && git pull")
```

## Integration with ruv-swarm

All GitHub modes can be enhanced with ruv-swarm coordination:

```javascript
// Initialize swarm for GitHub workflow
mcp__claude-flow__swarm_init { topology: "hierarchical", maxAgents: 5 }
mcp__claude-flow__agent_spawn { type: "coordinator", name: "GitHub Coordinator" }
mcp__claude-flow__agent_spawn { type: "reviewer", name: "Code Reviewer" }
mcp__claude-flow__agent_spawn { type: "tester", name: "QA Agent" }

// Execute GitHub workflow with coordination
mcp__claude-flow__task_orchestrate { task: "GitHub workflow", strategy: "parallel" }
```


---

## Source: issue-tracker.md

# GitHub Issue Tracker

## Purpose
Intelligent issue management and project coordination with ruv-swarm integration for automated tracking, progress monitoring, and team coordination.

## Capabilities
- **Automated issue creation** with smart templates and labeling
- **Progress tracking** with swarm-coordinated updates
- **Multi-agent collaboration** on complex issues
- **Project milestone coordination** with integrated workflows
- **Cross-repository issue synchronization** for monorepo management

## Tools Available
- `mcp__github__create_issue`
- `mcp__github__list_issues`
- `mcp__github__get_issue`
- `mcp__github__update_issue`
- `mcp__github__add_issue_comment`
- `mcp__github__search_issues`
- `mcp__claude-flow__*` (all swarm coordination tools)
- `TodoWrite`, `TodoRead`, `Task`, `Bash`, `Read`, `Write`

## Usage Patterns

### 1. Create Coordinated Issue with Swarm Tracking
```javascript
// Initialize issue management swarm
mcp__claude-flow__swarm_init { topology: "star", maxAgents: 3 }
mcp__claude-flow__agent_spawn { type: "coordinator", name: "Issue Coordinator" }
mcp__claude-flow__agent_spawn { type: "researcher", name: "Requirements Analyst" }
mcp__claude-flow__agent_spawn { type: "coder", name: "Implementation Planner" }

// Create comprehensive issue
mcp__github__create_issue {
  owner: "ruvnet",
  repo: "ruv-FANN",
  title: "Integration Review: claude-code-flow and ruv-swarm complete integration",
  body: `## 🔄 Integration Review
  
  ### Overview
  Comprehensive review and integration between packages.
  
  ### Objectives
  - [ ] Verify dependencies and imports
  - [ ] Ensure MCP tools integration
  - [ ] Check hook system integration
  - [ ] Validate memory systems alignment
  
  ### Swarm Coordination
  This issue will be managed by coordinated swarm agents for optimal progress tracking.`,
  labels: ["integration", "review", "enhancement"],
  assignees: ["ruvnet"]
}

// Set up automated tracking
mcp__claude-flow__task_orchestrate {
  task: "Monitor and coordinate issue progress with automated updates",
  strategy: "adaptive",
  priority: "medium"
}
```

### 2. Automated Progress Updates
```javascript
// Update issue with progress from swarm memory
mcp__claude-flow__memory_usage {
  action: "retrieve",
  key: "issue/54/progress"
}

// Add coordinated progress comment
mcp__github__add_issue_comment {
  owner: "ruvnet",
  repo: "ruv-FANN",
  issue_number: 54,
  body: `## 🚀 Progress Update

  ### Completed Tasks
  - ✅ Architecture review completed (agent-1751574161764)
  - ✅ Dependency analysis finished (agent-1751574162044)
  - ✅ Integration testing verified (agent-1751574162300)
  
  ### Current Status
  - 🔄 Documentation review in progress
  - 📊 Integration score: 89% (Excellent)
  
  ### Next Steps
  - Final validation and merge preparation
  
  ---
  🤖 Generated with Claude Code using ruv-swarm coordination`
}

// Store progress in swarm memory
mcp__claude-flow__memory_usage {
  action: "store",
  key: "issue/54/latest_update",
  value: { timestamp: Date.now(), progress: "89%", status: "near_completion" }
}
```

### 3. Multi-Issue Project Coordination
```javascript
// Search and coordinate related issues
mcp__github__search_issues {
  q: "repo:ruvnet/ruv-FANN label:integration state:open",
  sort: "created",
  order: "desc"
}

// Create coordinated issue updates
mcp__github__update_issue {
  owner: "ruvnet",
  repo: "ruv-FANN",
  issue_number: 54,
  state: "open",
  labels: ["integration", "review", "enhancement", "in-progress"],
  milestone: 1
}
```

## Batch Operations Example

### Complete Issue Management Workflow:
```javascript
[Single Message - Issue Lifecycle Management]:
  // Initialize issue coordination swarm
  mcp__claude-flow__swarm_init { topology: "mesh", maxAgents: 4 }
  mcp__claude-flow__agent_spawn { type: "coordinator", name: "Issue Manager" }
  mcp__claude-flow__agent_spawn { type: "analyst", name: "Progress Tracker" }
  mcp__claude-flow__agent_spawn { type: "researcher", name: "Context Gatherer" }
  
  // Create multiple related issues using gh CLI
  Bash(`gh issue create \
    --repo :owner/:repo \
    --title "Feature: Advanced GitHub Integration" \
    --body "Implement comprehensive GitHub workflow automation..." \
    --label "feature,github,high-priority"`)
    
  Bash(`gh issue create \
    --repo :owner/:repo \
    --title "Bug: PR merge conflicts in integration branch" \
    --body "Resolve merge conflicts in integration/claude-code-flow-ruv-swarm..." \
    --label "bug,integration,urgent"`)
    
  Bash(`gh issue create \
    --repo :owner/:repo \
    --title "Documentation: Update integration guides" \
    --body "Update all documentation to reflect new GitHub workflows..." \
    --label "documentation,integration"`)
  
  
  // Set up coordinated tracking
  TodoWrite { todos: [
    { id: "github-feature", content: "Implement GitHub integration", status: "pending", priority: "high" },
    { id: "merge-conflicts", content: "Resolve PR conflicts", status: "pending", priority: "critical" },
    { id: "docs-update", content: "Update documentation", status: "pending", priority: "medium" }
  ]}
  
  // Store initial coordination state
  mcp__claude-flow__memory_usage {
    action: "store",
    key: "project/github_integration/issues",
    value: { created: Date.now(), total_issues: 3, status: "initialized" }
  }
```

## Smart Issue Templates

### Integration Issue Template:
```markdown
## 🔄 Integration Task

### Overview
[Brief description of integration requirements]

### Objectives
- [ ] Component A integration
- [ ] Component B validation  
- [ ] Testing and verification
- [ ] Documentation updates

### Integration Areas
#### Dependencies
- [ ] Package.json updates
- [ ] Version compatibility
- [ ] Import statements

#### Functionality  
- [ ] Core feature integration
- [ ] API compatibility
- [ ] Performance validation

#### Testing
- [ ] Unit tests
- [ ] Integration tests
- [ ] End-to-end validation

### Swarm Coordination
- **Coordinator**: Overall progress tracking
- **Analyst**: Technical validation
- **Tester**: Quality assurance
- **Documenter**: Documentation updates

### Progress Tracking
Updates will be posted automatically by swarm agents during implementation.

---
🤖 Generated with Claude Code
```

### Bug Report Template:
```markdown
## 🐛 Bug Report

### Problem Description
[Clear description of the issue]

### Expected Behavior
[What should happen]

### Actual Behavior  
[What actually happens]

### Reproduction Steps
1. [Step 1]
2. [Step 2]
3. [Step 3]

### Environment
- Package: [package name and version]
- Node.js: [version]
- OS: [operating system]

### Investigation Plan
- [ ] Root cause analysis
- [ ] Fix implementation
- [ ] Testing and validation
- [ ] Regression testing

### Swarm Assignment
- **Debugger**: Issue investigation
- **Coder**: Fix implementation
- **Tester**: Validation and testing

---
🤖 Generated with Claude Code
```

## Best Practices

### 1. **Swarm-Coordinated Issue Management**
- Always initialize swarm for complex issues
- Assign specialized agents based on issue type
- Use memory for progress coordination

### 2. **Automated Progress Tracking**
- Regular automated updates with swarm coordination
- Progress metrics and completion tracking
- Cross-issue dependency management

### 3. **Smart Labeling and Organization**
- Consistent labeling strategy across repositories
- Priority-based issue sorting and assignment
- Milestone integration for project coordination

### 4. **Batch Issue Operations**
- Create multiple related issues simultaneously
- Bulk updates for project-wide changes
- Coordinated cross-repository issue management

## Integration with Other Modes

### Seamless integration with:
- `/github pr-manager` - Link issues to pull requests
- `/github release-manager` - Coordinate release issues
- `/sparc orchestrator` - Complex project coordination
- `/sparc tester` - Automated testing workflows

## Metrics and Analytics

### Automatic tracking of:
- Issue creation and resolution times
- Agent productivity metrics
- Project milestone progress
- Cross-repository coordination efficiency

### Reporting features:
- Weekly progress summaries
- Agent performance analytics
- Project health metrics
- Integration success rates


---

## Source: multi-repo-swarm.md

# Multi-Repo Swarm - Cross-Repository Swarm Orchestration

## Overview
Coordinate AI swarms across multiple repositories, enabling organization-wide automation and intelligent cross-project collaboration.

## Core Features

### 1. Cross-Repo Initialization
```bash
# Initialize multi-repo swarm with gh CLI
# List organization repositories
REPOS=$(gh repo list org --limit 100 --json name,description,languages \
  --jq '.[] | select(.name | test("frontend|backend|shared"))')

# Get repository details
REPO_DETAILS=$(echo "$REPOS" | jq -r '.name' | while read -r repo; do
  gh api repos/org/$repo --jq '{name, default_branch, languages, topics}'
done | jq -s '.')

# Initialize swarm with repository context
npx ruv-swarm github multi-repo-init \
  --repo-details "$REPO_DETAILS" \
  --repos "org/frontend,org/backend,org/shared" \
  --topology hierarchical \
  --shared-memory \
  --sync-strategy eventual
```

### 2. Repository Discovery
```bash
# Auto-discover related repositories with gh CLI
# Search organization repositories
REPOS=$(gh repo list my-organization --limit 100 \
  --json name,description,languages,topics \
  --jq '.[] | select(.languages | keys | contains(["TypeScript"]))')

# Analyze repository dependencies
DEPS=$(echo "$REPOS" | jq -r '.name' | while read -r repo; do
  # Get package.json if it exists
  if gh api repos/my-organization/$repo/contents/package.json --jq '.content' 2>/dev/null; then
    gh api repos/my-organization/$repo/contents/package.json \
      --jq '.content' | base64 -d | jq '{name, dependencies, devDependencies}'
  fi
done | jq -s '.')

# Discover and analyze
npx ruv-swarm github discover-repos \
  --repos "$REPOS" \
  --dependencies "$DEPS" \
  --analyze-dependencies \
  --suggest-swarm-topology
```

### 3. Synchronized Operations
```bash
# Execute synchronized changes across repos with gh CLI
# Get matching repositories
MATCHING_REPOS=$(gh repo list org --limit 100 --json name \
  --jq '.[] | select(.name | test("-service$")) | .name')

# Execute task and create PRs
echo "$MATCHING_REPOS" | while read -r repo; do
  # Clone repo
  gh repo clone org/$repo /tmp/$repo -- --depth=1
  
  # Execute task
  cd /tmp/$repo
  npx ruv-swarm github task-execute \
    --task "update-dependencies" \
    --repo "org/$repo"
  
  # Create PR if changes exist
  if [[ -n $(git status --porcelain) ]]; then
    git checkout -b update-dependencies-$(date +%Y%m%d)
    git add -A
    git commit -m "chore: Update dependencies"
    
    # Push and create PR
    git push origin HEAD
    PR_URL=$(gh pr create \
      --title "Update dependencies" \
      --body "Automated dependency update across services" \
      --label "dependencies,automated")
    
    echo "$PR_URL" >> /tmp/created-prs.txt
  fi
  cd -
done

# Link related PRs
PR_URLS=$(cat /tmp/created-prs.txt)
npx ruv-swarm github link-prs --urls "$PR_URLS"
```

## Configuration

### Multi-Repo Config File
```yaml
# .swarm/multi-repo.yml
version: 1
organization: my-org
repositories:
  - name: frontend
    url: github.com/my-org/frontend
    role: ui
    agents: [coder, designer, tester]
    
  - name: backend
    url: github.com/my-org/backend
    role: api
    agents: [architect, coder, tester]
    
  - name: shared
    url: github.com/my-org/shared
    role: library
    agents: [analyst, coder]

coordination:
  topology: hierarchical
  communication: webhook
  memory: redis://shared-memory
  
dependencies:
  - from: frontend
    to: [backend, shared]
  - from: backend
    to: [shared]
```

### Repository Roles
```javascript
// Define repository roles and responsibilities
{
  "roles": {
    "ui": {
      "responsibilities": ["user-interface", "ux", "accessibility"],
      "default-agents": ["designer", "coder", "tester"]
    },
    "api": {
      "responsibilities": ["endpoints", "business-logic", "data"],
      "default-agents": ["architect", "coder", "security"]
    },
    "library": {
      "responsibilities": ["shared-code", "utilities", "types"],
      "default-agents": ["analyst", "coder", "documenter"]
    }
  }
}
```

## Orchestration Commands

### Dependency Management
```bash
# Update dependencies across all repos with gh CLI
# Create tracking issue first
TRACKING_ISSUE=$(gh issue create \
  --title "Dependency Update: typescript@5.0.0" \
  --body "Tracking issue for updating TypeScript across all repositories" \
  --label "dependencies,tracking" \
  --json number -q .number)

# Get all repos with TypeScript
TS_REPOS=$(gh repo list org --limit 100 --json name | jq -r '.[].name' | \
  while read -r repo; do
    if gh api repos/org/$repo/contents/package.json 2>/dev/null | \
       jq -r '.content' | base64 -d | grep -q '"typescript"'; then
      echo "$repo"
    fi
  done)

# Update each repository
echo "$TS_REPOS" | while read -r repo; do
  # Clone and update
  gh repo clone org/$repo /tmp/$repo -- --depth=1
  cd /tmp/$repo
  
  # Update dependency
  npm install --save-dev typescript@5.0.0
  
  # Test changes
  if npm test; then
    # Create PR
    git checkout -b update-typescript-5
    git add package.json package-lock.json
    git commit -m "chore: Update TypeScript to 5.0.0

Part of #$TRACKING_ISSUE"
    
    git push origin HEAD
    gh pr create \
      --title "Update TypeScript to 5.0.0" \
      --body "Updates TypeScript to version 5.0.0\n\nTracking: #$TRACKING_ISSUE" \
      --label "dependencies"
  else
    # Report failure
    gh issue comment $TRACKING_ISSUE \
      --body "❌ Failed to update $repo - tests failing"
  fi
  cd -
done
```

### Refactoring Operations
```bash
# Coordinate large-scale refactoring
npx ruv-swarm github multi-repo-refactor \
  --pattern "rename:OldAPI->NewAPI" \
  --analyze-impact \
  --create-migration-guide \
  --staged-rollout
```

### Security Updates
```bash
# Coordinate security patches
npx ruv-swarm github multi-repo-security \
  --scan-all \
  --patch-vulnerabilities \
  --verify-fixes \
  --compliance-report
```

## Communication Strategies

### 1. Webhook-Based Coordination
```javascript
// webhook-coordinator.js
const { MultiRepoSwarm } = require('ruv-swarm');

const swarm = new MultiRepoSwarm({
  webhook: {
    url: 'https://swarm-coordinator.example.com',
    secret: process.env.WEBHOOK_SECRET
  }
});

// Handle cross-repo events
swarm.on('repo:update', async (event) => {
  await swarm.propagate(event, {
    to: event.dependencies,
    strategy: 'eventual-consistency'
  });
});
```

### 2. GraphQL Federation
```graphql
# Federated schema for multi-repo queries
type Repository @key(fields: "id") {
  id: ID!
  name: String!
  swarmStatus: SwarmStatus!
  dependencies: [Repository!]!
  agents: [Agent!]!
}

type SwarmStatus {
  active: Boolean!
  topology: Topology!
  tasks: [Task!]!
  memory: JSON!
}
```

### 3. Event Streaming
```yaml
# Kafka configuration for real-time coordination
kafka:
  brokers: ['kafka1:9092', 'kafka2:9092']
  topics:
    swarm-events: 
      partitions: 10
      replication: 3
    swarm-memory:
      partitions: 5
      replication: 3
```

## Advanced Features

### 1. Distributed Task Queue
```bash
# Create distributed task queue
npx ruv-swarm github multi-repo-queue \
  --backend redis \
  --workers 10 \
  --priority-routing \
  --dead-letter-queue
```

### 2. Cross-Repo Testing
```bash
# Run integration tests across repos
npx ruv-swarm github multi-repo-test \
  --setup-test-env \
  --link-services \
  --run-e2e \
  --tear-down
```

### 3. Monorepo Migration
```bash
# Assist in monorepo migration
npx ruv-swarm github to-monorepo \
  --analyze-repos \
  --suggest-structure \
  --preserve-history \
  --create-migration-prs
```

## Monitoring & Visualization

### Multi-Repo Dashboard
```bash
# Launch monitoring dashboard
npx ruv-swarm github multi-repo-dashboard \
  --port 3000 \
  --metrics "agent-activity,task-progress,memory-usage" \
  --real-time
```

### Dependency Graph
```bash
# Visualize repo dependencies
npx ruv-swarm github dep-graph \
  --format mermaid \
  --include-agents \
  --show-data-flow
```

### Health Monitoring
```bash
# Monitor swarm health across repos
npx ruv-swarm github health-check \
  --repos "org/*" \
  --check "connectivity,memory,agents" \
  --alert-on-issues
```

## Synchronization Patterns

### 1. Eventually Consistent
```javascript
// Eventual consistency for non-critical updates
{
  "sync": {
    "strategy": "eventual",
    "max-lag": "5m",
    "retry": {
      "attempts": 3,
      "backoff": "exponential"
    }
  }
}
```

### 2. Strong Consistency
```javascript
// Strong consistency for critical operations
{
  "sync": {
    "strategy": "strong",
    "consensus": "raft",
    "quorum": 0.51,
    "timeout": "30s"
  }
}
```

### 3. Hybrid Approach
```javascript
// Mix of consistency levels
{
  "sync": {
    "default": "eventual",
    "overrides": {
      "security-updates": "strong",
      "dependency-updates": "strong",
      "documentation": "eventual"
    }
  }
}
```

## Use Cases

### 1. Microservices Coordination
```bash
# Coordinate microservices development
npx ruv-swarm github microservices \
  --services "auth,users,orders,payments" \
  --ensure-compatibility \
  --sync-contracts \
  --integration-tests
```

### 2. Library Updates
```bash
# Update shared library across consumers
npx ruv-swarm github lib-update \
  --library "org/shared-lib" \
  --version "2.0.0" \
  --find-consumers \
  --update-imports \
  --run-tests
```

### 3. Organization-Wide Changes
```bash
# Apply org-wide policy changes
npx ruv-swarm github org-policy \
  --policy "add-security-headers" \
  --repos "org/*" \
  --validate-compliance \
  --create-reports
```

## Best Practices

### 1. Repository Organization
- Clear repository roles and boundaries
- Consistent naming conventions
- Documented dependencies
- Shared configuration standards

### 2. Communication
- Use appropriate sync strategies
- Implement circuit breakers
- Monitor latency and failures
- Clear error propagation

### 3. Security
- Secure cross-repo authentication
- Encrypted communication channels
- Audit trail for all operations
- Principle of least privilege

## Performance Optimization

### Caching Strategy
```bash
# Implement cross-repo caching
npx ruv-swarm github cache-strategy \
  --analyze-patterns \
  --suggest-cache-layers \
  --implement-invalidation
```

### Parallel Execution
```bash
# Optimize parallel operations
npx ruv-swarm github parallel-optimize \
  --analyze-dependencies \
  --identify-parallelizable \
  --execute-optimal
```

### Resource Pooling
```bash
# Pool resources across repos
npx ruv-swarm github resource-pool \
  --share-agents \
  --distribute-load \
  --monitor-usage
```

## Troubleshooting

### Connectivity Issues
```bash
# Diagnose connectivity problems
npx ruv-swarm github diagnose-connectivity \
  --test-all-repos \
  --check-permissions \
  --verify-webhooks
```

### Memory Synchronization
```bash
# Debug memory sync issues
npx ruv-swarm github debug-memory \
  --check-consistency \
  --identify-conflicts \
  --repair-state
```

### Performance Bottlenecks
```bash
# Identify performance issues
npx ruv-swarm github perf-analysis \
  --profile-operations \
  --identify-bottlenecks \
  --suggest-optimizations
```

## Examples

### Full-Stack Application Update
```bash
# Update full-stack application
npx ruv-swarm github fullstack-update \
  --frontend "org/web-app" \
  --backend "org/api-server" \
  --database "org/db-migrations" \
  --coordinate-deployment
```

### Cross-Team Collaboration
```bash
# Facilitate cross-team work
npx ruv-swarm github cross-team \
  --teams "frontend,backend,devops" \
  --task "implement-feature-x" \
  --assign-by-expertise \
  --track-progress
```

See also: [swarm-pr.md](./swarm-pr.md), [project-board-sync.md](./project-board-sync.md)


---

## Source: pr-manager.md

# GitHub PR Manager

## Purpose
Comprehensive pull request management with swarm coordination for automated reviews, testing, and merge workflows.

## Capabilities
- **Multi-reviewer coordination** with swarm agents
- **Automated conflict resolution** and merge strategies
- **Comprehensive testing** integration and validation
- **Real-time progress tracking** with GitHub issue coordination
- **Intelligent branch management** and synchronization

## Usage Patterns

### 1. Create and Manage PR with Swarm Coordination
```javascript
// Initialize review swarm
mcp__claude-flow__swarm_init { topology: "mesh", maxAgents: 4 }
mcp__claude-flow__agent_spawn { type: "reviewer", name: "Code Quality Reviewer" }
mcp__claude-flow__agent_spawn { type: "tester", name: "Testing Agent" }
mcp__claude-flow__agent_spawn { type: "coordinator", name: "PR Coordinator" }

// Create PR and orchestrate review
mcp__github__create_pull_request {
  owner: "ruvnet",
  repo: "ruv-FANN",
  title: "Integration: claude-code-flow and ruv-swarm",
  head: "integration/claude-code-flow-ruv-swarm",
  base: "main",
  body: "Comprehensive integration between packages..."
}

// Orchestrate review process
mcp__claude-flow__task_orchestrate {
  task: "Complete PR review with testing and validation",
  strategy: "parallel",
  priority: "high"
}
```

### 2. Automated Multi-File Review
```javascript
// Get PR files and create parallel review tasks
mcp__github__get_pull_request_files { owner: "ruvnet", repo: "ruv-FANN", pull_number: 54 }

// Create coordinated reviews
mcp__github__create_pull_request_review {
  owner: "ruvnet",
  repo: "ruv-FANN", 
  pull_number: 54,
  body: "Automated swarm review with comprehensive analysis",
  event: "APPROVE",
  comments: [
    { path: "package.json", line: 78, body: "Dependency integration verified" },
    { path: "src/index.js", line: 45, body: "Import structure optimized" }
  ]
}
```

### 3. Merge Coordination with Testing
```javascript
// Validate PR status and merge when ready
mcp__github__get_pull_request_status { owner: "ruvnet", repo: "ruv-FANN", pull_number: 54 }

// Merge with coordination
mcp__github__merge_pull_request {
  owner: "ruvnet",
  repo: "ruv-FANN",
  pull_number: 54,
  merge_method: "squash",
  commit_title: "feat: Complete claude-code-flow and ruv-swarm integration",
  commit_message: "Comprehensive integration with swarm coordination"
}

// Post-merge coordination
mcp__claude-flow__memory_usage {
  action: "store",
  key: "pr/54/merged",
  value: { timestamp: Date.now(), status: "success" }
}
```

## Batch Operations Example

### Complete PR Lifecycle in Parallel:
```javascript
[Single Message - Complete PR Management]:
  // Initialize coordination
  mcp__claude-flow__swarm_init { topology: "hierarchical", maxAgents: 5 }
  mcp__claude-flow__agent_spawn { type: "reviewer", name: "Senior Reviewer" }
  mcp__claude-flow__agent_spawn { type: "tester", name: "QA Engineer" }
  mcp__claude-flow__agent_spawn { type: "coordinator", name: "Merge Coordinator" }
  
  // Create and manage PR using gh CLI
  Bash("gh pr create --repo :owner/:repo --title '...' --head '...' --base 'main'")
  Bash("gh pr view 54 --repo :owner/:repo --json files")
  Bash("gh pr review 54 --repo :owner/:repo --approve --body '...'")
  
  
  // Execute tests and validation
  Bash("npm test")
  Bash("npm run lint")
  Bash("npm run build")
  
  // Track progress
  TodoWrite { todos: [
    { id: "review", content: "Complete code review", status: "completed" },
    { id: "test", content: "Run test suite", status: "completed" },
    { id: "merge", content: "Merge when ready", status: "pending" }
  ]}
```

## Best Practices

### 1. **Always Use Swarm Coordination**
- Initialize swarm before complex PR operations
- Assign specialized agents for different review aspects
- Use memory for cross-agent coordination

### 2. **Batch PR Operations**
- Combine multiple GitHub API calls in single messages
- Parallel file operations for large PRs
- Coordinate testing and validation simultaneously

### 3. **Intelligent Review Strategy**
- Automated conflict detection and resolution
- Multi-agent review for comprehensive coverage
- Performance and security validation integration

### 4. **Progress Tracking**
- Use TodoWrite for PR milestone tracking
- GitHub issue integration for project coordination
- Real-time status updates through swarm memory

## Integration with Other Modes

### Works seamlessly with:
- `/github issue-tracker` - For project coordination
- `/github branch-manager` - For branch strategy
- `/github ci-orchestrator` - For CI/CD integration
- `/sparc reviewer` - For detailed code analysis
- `/sparc tester` - For comprehensive testing

## Error Handling

### Automatic retry logic for:
- Network failures during GitHub API calls
- Merge conflicts with intelligent resolution
- Test failures with automatic re-runs
- Review bottlenecks with load balancing

### Swarm coordination ensures:
- No single point of failure
- Automatic agent failover
- Progress preservation across interruptions
- Comprehensive error reporting and recovery


---

## Source: project-board-sync.md

# Project Board Sync - GitHub Projects Integration

## Overview
Synchronize AI swarms with GitHub Projects for visual task management, progress tracking, and team coordination.

## Core Features

### 1. Board Initialization
```bash
# Connect swarm to GitHub Project using gh CLI
# Get project details
PROJECT_ID=$(gh project list --owner @me --format json | \
  jq -r '.projects[] | select(.title == "Development Board") | .id')

# Initialize swarm with project
npx ruv-swarm github board-init \
  --project-id "$PROJECT_ID" \
  --sync-mode "bidirectional" \
  --create-views "swarm-status,agent-workload,priority"

# Create project fields for swarm tracking
gh project field-create $PROJECT_ID --owner @me \
  --name "Swarm Status" \
  --data-type "SINGLE_SELECT" \
  --single-select-options "pending,in_progress,completed"
```

### 2. Task Synchronization
```bash
# Sync swarm tasks with project cards
npx ruv-swarm github board-sync \
  --map-status '{
    "todo": "To Do",
    "in_progress": "In Progress",
    "review": "Review",
    "done": "Done"
  }' \
  --auto-move-cards \
  --update-metadata
```

### 3. Real-time Updates
```bash
# Enable real-time board updates
npx ruv-swarm github board-realtime \
  --webhook-endpoint "https://api.example.com/github-sync" \
  --update-frequency "immediate" \
  --batch-updates false
```

## Configuration

### Board Mapping Configuration
```yaml
# .github/board-sync.yml
version: 1
project:
  name: "AI Development Board"
  number: 1
  
mapping:
  # Map swarm task status to board columns
  status:
    pending: "Backlog"
    assigned: "Ready"
    in_progress: "In Progress"
    review: "Review"
    completed: "Done"
    blocked: "Blocked"
    
  # Map agent types to labels
  agents:
    coder: "🔧 Development"
    tester: "🧪 Testing"
    analyst: "📊 Analysis"
    designer: "🎨 Design"
    architect: "🏗️ Architecture"
    
  # Map priority to project fields
  priority:
    critical: "🔴 Critical"
    high: "🟡 High"
    medium: "🟢 Medium"
    low: "⚪ Low"
    
  # Custom fields
  fields:
    - name: "Agent Count"
      type: number
      source: task.agents.length
    - name: "Complexity"
      type: select
      source: task.complexity
    - name: "ETA"
      type: date
      source: task.estimatedCompletion
```

### View Configuration
```javascript
// Custom board views
{
  "views": [
    {
      "name": "Swarm Overview",
      "type": "board",
      "groupBy": "status",
      "filters": ["is:open"],
      "sort": "priority:desc"
    },
    {
      "name": "Agent Workload",
      "type": "table",
      "groupBy": "assignedAgent",
      "columns": ["title", "status", "priority", "eta"],
      "sort": "eta:asc"
    },
    {
      "name": "Sprint Progress",
      "type": "roadmap",
      "dateField": "eta",
      "groupBy": "milestone"
    }
  ]
}
```

## Automation Features

### 1. Auto-Assignment
```bash
# Automatically assign cards to agents
npx ruv-swarm github board-auto-assign \
  --strategy "load-balanced" \
  --consider "expertise,workload,availability" \
  --update-cards
```

### 2. Progress Tracking
```bash
# Track and visualize progress
npx ruv-swarm github board-progress \
  --show "burndown,velocity,cycle-time" \
  --time-period "sprint" \
  --export-metrics
```

### 3. Smart Card Movement
```bash
# Intelligent card state transitions
npx ruv-swarm github board-smart-move \
  --rules '{
    "auto-progress": "when:all-subtasks-done",
    "auto-review": "when:tests-pass",
    "auto-done": "when:pr-merged"
  }'
```

## Board Commands

### Create Cards from Issues
```bash
# Convert issues to project cards using gh CLI
# List issues with label
ISSUES=$(gh issue list --label "enhancement" --json number,title,body)

# Add issues to project
echo "$ISSUES" | jq -r '.[].number' | while read -r issue; do
  gh project item-add $PROJECT_ID --owner @me --url "https://github.com/$GITHUB_REPOSITORY/issues/$issue"
done

# Process with swarm
npx ruv-swarm github board-import-issues \
  --issues "$ISSUES" \
  --add-to-column "Backlog" \
  --parse-checklist \
  --assign-agents
```

### Bulk Operations
```bash
# Bulk card operations
npx ruv-swarm github board-bulk \
  --filter "status:blocked" \
  --action "add-label:needs-attention" \
  --notify-assignees
```

### Card Templates
```bash
# Create cards from templates
npx ruv-swarm github board-template \
  --template "feature-development" \
  --variables '{
    "feature": "User Authentication",
    "priority": "high",
    "agents": ["architect", "coder", "tester"]
  }' \
  --create-subtasks
```

## Advanced Synchronization

### 1. Multi-Board Sync
```bash
# Sync across multiple boards
npx ruv-swarm github multi-board-sync \
  --boards "Development,QA,Release" \
  --sync-rules '{
    "Development->QA": "when:ready-for-test",
    "QA->Release": "when:tests-pass"
  }'
```

### 2. Cross-Organization Sync
```bash
# Sync boards across organizations
npx ruv-swarm github cross-org-sync \
  --source "org1/Project-A" \
  --target "org2/Project-B" \
  --field-mapping "custom" \
  --conflict-resolution "source-wins"
```

### 3. External Tool Integration
```bash
# Sync with external tools
npx ruv-swarm github board-integrate \
  --tool "jira" \
  --mapping "bidirectional" \
  --sync-frequency "5m" \
  --transform-rules "custom"
```

## Visualization & Reporting

### Board Analytics
```bash
# Generate board analytics using gh CLI data
# Fetch project data
PROJECT_DATA=$(gh project item-list $PROJECT_ID --owner @me --format json)

# Get issue metrics
ISSUE_METRICS=$(echo "$PROJECT_DATA" | jq -r '.items[] | select(.content.type == "Issue")' | \
  while read -r item; do
    ISSUE_NUM=$(echo "$item" | jq -r '.content.number')
    gh issue view $ISSUE_NUM --json createdAt,closedAt,labels,assignees
  done)

# Generate analytics with swarm
npx ruv-swarm github board-analytics \
  --project-data "$PROJECT_DATA" \
  --issue-metrics "$ISSUE_METRICS" \
  --metrics "throughput,cycle-time,wip" \
  --group-by "agent,priority,type" \
  --time-range "30d" \
  --export "dashboard"
```

### Custom Dashboards
```javascript
// Dashboard configuration
{
  "dashboard": {
    "widgets": [
      {
        "type": "chart",
        "title": "Task Completion Rate",
        "data": "completed-per-day",
        "visualization": "line"
      },
      {
        "type": "gauge",
        "title": "Sprint Progress",
        "data": "sprint-completion",
        "target": 100
      },
      {
        "type": "heatmap",
        "title": "Agent Activity",
        "data": "agent-tasks-per-day"
      }
    ]
  }
}
```

### Reports
```bash
# Generate reports
npx ruv-swarm github board-report \
  --type "sprint-summary" \
  --format "markdown" \
  --include "velocity,burndown,blockers" \
  --distribute "slack,email"
```

## Workflow Integration

### Sprint Management
```bash
# Manage sprints with swarms
npx ruv-swarm github sprint-manage \
  --sprint "Sprint 23" \
  --auto-populate \
  --capacity-planning \
  --track-velocity
```

### Milestone Tracking
```bash
# Track milestone progress
npx ruv-swarm github milestone-track \
  --milestone "v2.0 Release" \
  --update-board \
  --show-dependencies \
  --predict-completion
```

### Release Planning
```bash
# Plan releases using board data
npx ruv-swarm github release-plan-board \
  --analyze-velocity \
  --estimate-completion \
  --identify-risks \
  --optimize-scope
```

## Team Collaboration

### Work Distribution
```bash
# Distribute work among team
npx ruv-swarm github board-distribute \
  --strategy "skills-based" \
  --balance-workload \
  --respect-preferences \
  --notify-assignments
```

### Standup Automation
```bash
# Generate standup reports
npx ruv-swarm github standup-report \
  --team "frontend" \
  --include "yesterday,today,blockers" \
  --format "slack" \
  --schedule "daily-9am"
```

### Review Coordination
```bash
# Coordinate reviews via board
npx ruv-swarm github review-coordinate \
  --board "Code Review" \
  --assign-reviewers \
  --track-feedback \
  --ensure-coverage
```

## Best Practices

### 1. Board Organization
- Clear column definitions
- Consistent labeling system
- Regular board grooming
- Automation rules

### 2. Data Integrity
- Bidirectional sync validation
- Conflict resolution strategies
- Audit trails
- Regular backups

### 3. Team Adoption
- Training materials
- Clear workflows
- Regular reviews
- Feedback loops

## Troubleshooting

### Sync Issues
```bash
# Diagnose sync problems
npx ruv-swarm github board-diagnose \
  --check "permissions,webhooks,rate-limits" \
  --test-sync \
  --show-conflicts
```

### Performance
```bash
# Optimize board performance
npx ruv-swarm github board-optimize \
  --analyze-size \
  --archive-completed \
  --index-fields \
  --cache-views
```

### Data Recovery
```bash
# Recover board data
npx ruv-swarm github board-recover \
  --backup-id "2024-01-15" \
  --restore-cards \
  --preserve-current \
  --merge-conflicts
```

## Examples

### Agile Development Board
```bash
# Setup agile board
npx ruv-swarm github agile-board \
  --methodology "scrum" \
  --sprint-length "2w" \
  --ceremonies "planning,review,retro" \
  --metrics "velocity,burndown"
```

### Kanban Flow Board
```bash
# Setup kanban board
npx ruv-swarm github kanban-board \
  --wip-limits '{
    "In Progress": 5,
    "Review": 3
  }' \
  --cycle-time-tracking \
  --continuous-flow
```

### Research Project Board
```bash
# Setup research board
npx ruv-swarm github research-board \
  --phases "ideation,research,experiment,analysis,publish" \
  --track-citations \
  --collaborate-external
```

## Metrics & KPIs

### Performance Metrics
```bash
# Track board performance
npx ruv-swarm github board-kpis \
  --metrics '[
    "average-cycle-time",
    "throughput-per-sprint",
    "blocked-time-percentage",
    "first-time-pass-rate"
  ]' \
  --dashboard-url
```

### Team Metrics
```bash
# Track team performance
npx ruv-swarm github team-metrics \
  --board "Development" \
  --per-member \
  --include "velocity,quality,collaboration" \
  --anonymous-option
```

See also: [swarm-issue.md](./swarm-issue.md), [multi-repo-swarm.md](./multi-repo-swarm.md)


---

## Source: release-manager.md

# GitHub Release Manager

## Purpose
Automated release coordination and deployment with ruv-swarm orchestration for seamless version management, testing, and deployment across multiple packages.

## Capabilities
- **Automated release pipelines** with comprehensive testing
- **Version coordination** across multiple packages
- **Deployment orchestration** with rollback capabilities  
- **Release documentation** generation and management
- **Multi-stage validation** with swarm coordination

## Usage Patterns

### 1. Coordinated Release Preparation
```javascript
// Initialize release management swarm
mcp__claude-flow__swarm_init { topology: "hierarchical", maxAgents: 6 }
mcp__claude-flow__agent_spawn { type: "coordinator", name: "Release Coordinator" }
mcp__claude-flow__agent_spawn { type: "tester", name: "QA Engineer" }
mcp__claude-flow__agent_spawn { type: "reviewer", name: "Release Reviewer" }
mcp__claude-flow__agent_spawn { type: "coder", name: "Version Manager" }
mcp__claude-flow__agent_spawn { type: "analyst", name: "Deployment Analyst" }

// Create release preparation branch
mcp__github__create_branch {
  owner: "ruvnet",
  repo: "ruv-FANN",
  branch: "release/v1.0.72",
  from_branch: "main"
}

// Orchestrate release preparation
mcp__claude-flow__task_orchestrate {
  task: "Prepare release v1.0.72 with comprehensive testing and validation",
  strategy: "sequential",
  priority: "critical"
}
```

### 2. Multi-Package Version Coordination
```javascript
// Update versions across packages
mcp__github__push_files {
  owner: "ruvnet",
  repo: "ruv-FANN", 
  branch: "release/v1.0.72",
  files: [
    {
      path: "claude-code-flow/claude-code-flow/package.json",
      content: JSON.stringify({
        name: "claude-flow",
        version: "1.0.72",
        // ... rest of package.json
      }, null, 2)
    },
    {
      path: "ruv-swarm/npm/package.json", 
      content: JSON.stringify({
        name: "ruv-swarm",
        version: "1.0.12",
        // ... rest of package.json
      }, null, 2)
    },
    {
      path: "CHANGELOG.md",
      content: `# Changelog

## [1.0.72] - ${new Date().toISOString().split('T')[0]}

### Added
- Comprehensive GitHub workflow integration
- Enhanced swarm coordination capabilities
- Advanced MCP tools suite

### Changed  
- Aligned Node.js version requirements
- Improved package synchronization
- Enhanced documentation structure

### Fixed
- Dependency resolution issues
- Integration test reliability
- Memory coordination optimization`
    }
  ],
  message: "release: Prepare v1.0.72 with GitHub integration and swarm enhancements"
}
```

### 3. Automated Release Validation
```javascript
// Comprehensive release testing
Bash("cd /workspaces/ruv-FANN/claude-code-flow/claude-code-flow && npm install")
Bash("cd /workspaces/ruv-FANN/claude-code-flow/claude-code-flow && npm run test")
Bash("cd /workspaces/ruv-FANN/claude-code-flow/claude-code-flow && npm run lint")
Bash("cd /workspaces/ruv-FANN/claude-code-flow/claude-code-flow && npm run build")

Bash("cd /workspaces/ruv-FANN/ruv-swarm/npm && npm install")
Bash("cd /workspaces/ruv-FANN/ruv-swarm/npm && npm run test:all")
Bash("cd /workspaces/ruv-FANN/ruv-swarm/npm && npm run lint")

// Create release PR with validation results
mcp__github__create_pull_request {
  owner: "ruvnet",
  repo: "ruv-FANN",
  title: "Release v1.0.72: GitHub Integration and Swarm Enhancements",
  head: "release/v1.0.72", 
  base: "main",
  body: `## 🚀 Release v1.0.72

### 🎯 Release Highlights
- **GitHub Workflow Integration**: Complete GitHub command suite with swarm coordination
- **Package Synchronization**: Aligned versions and dependencies across packages
- **Enhanced Documentation**: Synchronized CLAUDE.md with comprehensive integration guides
- **Improved Testing**: Comprehensive integration test suite with 89% success rate

### 📦 Package Updates
- **claude-flow**: v1.0.71 → v1.0.72
- **ruv-swarm**: v1.0.11 → v1.0.12

### 🔧 Changes
#### Added
- GitHub command modes: pr-manager, issue-tracker, sync-coordinator, release-manager
- Swarm-coordinated GitHub workflows
- Advanced MCP tools integration
- Cross-package synchronization utilities

#### Changed
- Node.js requirement aligned to >=20.0.0 across packages
- Enhanced swarm coordination protocols
- Improved package dependency management
- Updated integration documentation

#### Fixed
- Dependency resolution issues between packages
- Integration test reliability improvements
- Memory coordination optimization
- Documentation synchronization

### ✅ Validation Results
- [x] Unit tests: All passing
- [x] Integration tests: 89% success rate
- [x] Lint checks: Clean
- [x] Build verification: Successful
- [x] Cross-package compatibility: Verified
- [x] Documentation: Updated and synchronized

### 🐝 Swarm Coordination
This release was coordinated using ruv-swarm agents:
- **Release Coordinator**: Overall release management
- **QA Engineer**: Comprehensive testing validation
- **Release Reviewer**: Code quality and standards review
- **Version Manager**: Package version coordination
- **Deployment Analyst**: Release deployment validation

### 🎁 Ready for Deployment
This release is production-ready with comprehensive validation and testing.

---
🤖 Generated with Claude Code using ruv-swarm coordination`
}
```

## Batch Release Workflow

### Complete Release Pipeline:
```javascript
[Single Message - Complete Release Management]:
  // Initialize comprehensive release swarm
  mcp__claude-flow__swarm_init { topology: "star", maxAgents: 8 }
  mcp__claude-flow__agent_spawn { type: "coordinator", name: "Release Director" }
  mcp__claude-flow__agent_spawn { type: "tester", name: "QA Lead" }
  mcp__claude-flow__agent_spawn { type: "reviewer", name: "Senior Reviewer" }
  mcp__claude-flow__agent_spawn { type: "coder", name: "Version Controller" }
  mcp__claude-flow__agent_spawn { type: "analyst", name: "Performance Analyst" }
  mcp__claude-flow__agent_spawn { type: "researcher", name: "Compatibility Checker" }
  
  // Create release branch and prepare files using gh CLI
  Bash("gh api repos/:owner/:repo/git/refs --method POST -f ref='refs/heads/release/v1.0.72' -f sha=$(gh api repos/:owner/:repo/git/refs/heads/main --jq '.object.sha')")
  
  // Clone and update release files
  Bash("gh repo clone :owner/:repo /tmp/release-v1.0.72 -- --branch release/v1.0.72 --depth=1")
  
  // Update all release-related files
  Write("/tmp/release-v1.0.72/claude-code-flow/claude-code-flow/package.json", "[updated package.json]")
  Write("/tmp/release-v1.0.72/ruv-swarm/npm/package.json", "[updated package.json]")
  Write("/tmp/release-v1.0.72/CHANGELOG.md", "[release changelog]")
  Write("/tmp/release-v1.0.72/RELEASE_NOTES.md", "[detailed release notes]")
  
  Bash("cd /tmp/release-v1.0.72 && git add -A && git commit -m 'release: Prepare v1.0.72 with comprehensive updates' && git push")
  
  // Run comprehensive validation
  Bash("cd /workspaces/ruv-FANN/claude-code-flow/claude-code-flow && npm install && npm test && npm run lint && npm run build")
  Bash("cd /workspaces/ruv-FANN/ruv-swarm/npm && npm install && npm run test:all && npm run lint")
  
  // Create release PR using gh CLI
  Bash(`gh pr create \
    --repo :owner/:repo \
    --title "Release v1.0.72: GitHub Integration and Swarm Enhancements" \
    --head "release/v1.0.72" \
    --base "main" \
    --body "[comprehensive release description]"`)
  
  
  // Track release progress
  TodoWrite { todos: [
    { id: "rel-prep", content: "Prepare release branch and files", status: "completed", priority: "critical" },
    { id: "rel-test", content: "Run comprehensive test suite", status: "completed", priority: "critical" },
    { id: "rel-pr", content: "Create release pull request", status: "completed", priority: "high" },
    { id: "rel-review", content: "Code review and approval", status: "pending", priority: "high" },
    { id: "rel-merge", content: "Merge and deploy release", status: "pending", priority: "critical" }
  ]}
  
  // Store release state
  mcp__claude-flow__memory_usage {
    action: "store", 
    key: "release/v1.0.72/status",
    value: {
      timestamp: Date.now(),
      version: "1.0.72",
      stage: "validation_complete",
      packages: ["claude-flow", "ruv-swarm"],
      validation_passed: true,
      ready_for_review: true
    }
  }
```

## Release Strategies

### 1. **Semantic Versioning Strategy**
```javascript
const versionStrategy = {
  major: "Breaking changes or architecture overhauls",
  minor: "New features, GitHub integration, swarm enhancements", 
  patch: "Bug fixes, documentation updates, dependency updates",
  coordination: "Cross-package version alignment"
}
```

### 2. **Multi-Stage Validation**
```javascript
const validationStages = [
  "unit_tests",           // Individual package testing
  "integration_tests",    // Cross-package integration
  "performance_tests",    // Performance regression detection
  "compatibility_tests",  // Version compatibility validation
  "documentation_tests",  // Documentation accuracy verification
  "deployment_tests"      // Deployment simulation
]
```

### 3. **Rollback Strategy**
```javascript
const rollbackPlan = {
  triggers: ["test_failures", "deployment_issues", "critical_bugs"],
  automatic: ["failed_tests", "build_failures"],
  manual: ["user_reported_issues", "performance_degradation"],
  recovery: "Previous stable version restoration"
}
```

## Best Practices

### 1. **Comprehensive Testing**
- Multi-package test coordination
- Integration test validation
- Performance regression detection
- Security vulnerability scanning

### 2. **Documentation Management**
- Automated changelog generation
- Release notes with detailed changes
- Migration guides for breaking changes
- API documentation updates

### 3. **Deployment Coordination**
- Staged deployment with validation
- Rollback mechanisms and procedures
- Performance monitoring during deployment
- User communication and notifications

### 4. **Version Management**
- Semantic versioning compliance
- Cross-package version coordination
- Dependency compatibility validation
- Breaking change documentation

## Integration with CI/CD

### GitHub Actions Integration:
```yaml
name: Release Management
on:
  pull_request:
    branches: [main]
    paths: ['**/package.json', 'CHANGELOG.md']

jobs:
  release-validation:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: '20'
      - name: Install and Test
        run: |
          cd claude-code-flow/claude-code-flow && npm install && npm test
          cd ../../ruv-swarm/npm && npm install && npm test:all
      - name: Validate Release
        run: npx claude-flow release validate
```

## Monitoring and Metrics

### Release Quality Metrics:
- Test coverage percentage
- Integration success rate
- Deployment time metrics
- Rollback frequency

### Automated Monitoring:
- Performance regression detection
- Error rate monitoring
- User adoption metrics
- Feedback collection and analysis


---

## Source: release-swarm.md

# Release Swarm - Intelligent Release Automation

## Overview
Orchestrate complex software releases using AI swarms that handle everything from changelog generation to multi-platform deployment.

## Core Features

### 1. Release Planning
```bash
# Plan next release using gh CLI
# Get commit history since last release
LAST_TAG=$(gh release list --limit 1 --json tagName -q '.[0].tagName')
COMMITS=$(gh api repos/:owner/:repo/compare/${LAST_TAG}...HEAD --jq '.commits')

# Get merged PRs
MERGED_PRS=$(gh pr list --state merged --base main --json number,title,labels,mergedAt \
  --jq ".[] | select(.mergedAt > \"$(gh release view $LAST_TAG --json publishedAt -q .publishedAt)\")")  

# Plan release with commit analysis
npx ruv-swarm github release-plan \
  --commits "$COMMITS" \
  --merged-prs "$MERGED_PRS" \
  --analyze-commits \
  --suggest-version \
  --identify-breaking \
  --generate-timeline
```

### 2. Automated Versioning
```bash
# Smart version bumping
npx ruv-swarm github release-version \
  --strategy "semantic" \
  --analyze-changes \
  --check-breaking \
  --update-files
```

### 3. Release Orchestration
```bash
# Full release automation with gh CLI
# Generate changelog from PRs and commits
CHANGELOG=$(gh api repos/:owner/:repo/compare/${LAST_TAG}...HEAD \
  --jq '.commits[].commit.message' | \
  npx ruv-swarm github generate-changelog)

# Create release draft
gh release create v2.0.0 \
  --draft \
  --title "Release v2.0.0" \
  --notes "$CHANGELOG" \
  --target main

# Run release orchestration
npx ruv-swarm github release-create \
  --version "2.0.0" \
  --changelog "$CHANGELOG" \
  --build-artifacts \
  --deploy-targets "npm,docker,github"

# Publish release after validation
gh release edit v2.0.0 --draft=false

# Create announcement issue
gh issue create \
  --title "🎉 Released v2.0.0" \
  --body "$CHANGELOG" \
  --label "announcement,release"
```

## Release Configuration

### Release Config File
```yaml
# .github/release-swarm.yml
version: 1
release:
  versioning:
    strategy: semantic
    breaking-keywords: ["BREAKING", "!"]
    
  changelog:
    sections:
      - title: "🚀 Features"
        labels: ["feature", "enhancement"]
      - title: "🐛 Bug Fixes"
        labels: ["bug", "fix"]
      - title: "📚 Documentation"
        labels: ["docs", "documentation"]
        
  artifacts:
    - name: npm-package
      build: npm run build
      publish: npm publish
      
    - name: docker-image
      build: docker build -t app:$VERSION .
      publish: docker push app:$VERSION
      
    - name: binaries
      build: ./scripts/build-binaries.sh
      upload: github-release
      
  deployment:
    environments:
      - name: staging
        auto-deploy: true
        validation: npm run test:e2e
        
      - name: production
        approval-required: true
        rollback-enabled: true
        
  notifications:
    - slack: releases-channel
    - email: stakeholders@company.com
    - discord: webhook-url
```

## Release Agents

### Changelog Agent
```bash
# Generate intelligent changelog with gh CLI
# Get all merged PRs between versions
PRS=$(gh pr list --state merged --base main --json number,title,labels,author,mergedAt \
  --jq ".[] | select(.mergedAt > \"$(gh release view v1.0.0 --json publishedAt -q .publishedAt)\")")  

# Get contributors
CONTRIBUTORS=$(echo "$PRS" | jq -r '[.author.login] | unique | join(", ")')

# Get commit messages
COMMITS=$(gh api repos/:owner/:repo/compare/v1.0.0...HEAD \
  --jq '.commits[].commit.message')

# Generate categorized changelog
CHANGELOG=$(npx ruv-swarm github changelog \
  --prs "$PRS" \
  --commits "$COMMITS" \
  --contributors "$CONTRIBUTORS" \
  --from v1.0.0 \
  --to HEAD \
  --categorize \
  --add-migration-guide)

# Save changelog
echo "$CHANGELOG" > CHANGELOG.md

# Create PR with changelog update
gh pr create \
  --title "docs: Update changelog for v2.0.0" \
  --body "Automated changelog update" \
  --base main
```

**Capabilities:**
- Semantic commit analysis
- Breaking change detection
- Contributor attribution
- Migration guide generation
- Multi-language support

### Version Agent
```bash
# Determine next version
npx ruv-swarm github version-suggest \
  --current v1.2.3 \
  --analyze-commits \
  --check-compatibility \
  --suggest-pre-release
```

**Logic:**
- Analyzes commit messages
- Detects breaking changes
- Suggests appropriate bump
- Handles pre-releases
- Validates version constraints

### Build Agent
```bash
# Coordinate multi-platform builds
npx ruv-swarm github release-build \
  --platforms "linux,macos,windows" \
  --architectures "x64,arm64" \
  --parallel \
  --optimize-size
```

**Features:**
- Cross-platform compilation
- Parallel build execution
- Artifact optimization
- Dependency bundling
- Build caching

### Test Agent
```bash
# Pre-release testing
npx ruv-swarm github release-test \
  --suites "unit,integration,e2e,performance" \
  --environments "node:16,node:18,node:20" \
  --fail-fast false \
  --generate-report
```

### Deploy Agent
```bash
# Multi-target deployment
npx ruv-swarm github release-deploy \
  --targets "npm,docker,github,s3" \
  --staged-rollout \
  --monitor-metrics \
  --auto-rollback
```

## Advanced Features

### 1. Progressive Deployment
```yaml
# Staged rollout configuration
deployment:
  strategy: progressive
  stages:
    - name: canary
      percentage: 5
      duration: 1h
      metrics:
        - error-rate < 0.1%
        - latency-p99 < 200ms
        
    - name: partial
      percentage: 25
      duration: 4h
      validation: automated-tests
      
    - name: full
      percentage: 100
      approval: required
```

### 2. Multi-Repo Releases
```bash
# Coordinate releases across repos
npx ruv-swarm github multi-release \
  --repos "frontend:v2.0.0,backend:v2.1.0,cli:v1.5.0" \
  --ensure-compatibility \
  --atomic-release \
  --synchronized
```

### 3. Hotfix Automation
```bash
# Emergency hotfix process
npx ruv-swarm github hotfix \
  --issue 789 \
  --target-version v1.2.4 \
  --cherry-pick-commits \
  --fast-track-deploy
```

## Release Workflows

### Standard Release Flow
```yaml
# .github/workflows/release.yml
name: Release Workflow
on:
  push:
    tags: ['v*']

jobs:
  release-swarm:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
        with:
          fetch-depth: 0
          
      - name: Setup GitHub CLI
        run: echo "${{ secrets.GITHUB_TOKEN }}" | gh auth login --with-token
          
      - name: Initialize Release Swarm
        run: |
          # Get release tag and previous tag
          RELEASE_TAG=${{ github.ref_name }}
          PREV_TAG=$(gh release list --limit 2 --json tagName -q '.[1].tagName')
          
          # Get PRs and commits for changelog
          PRS=$(gh pr list --state merged --base main --json number,title,labels,author \
            --search "merged:>=$(gh release view $PREV_TAG --json publishedAt -q .publishedAt)")
          
          npx ruv-swarm github release-init \
            --tag $RELEASE_TAG \
            --previous-tag $PREV_TAG \
            --prs "$PRS" \
            --spawn-agents "changelog,version,build,test,deploy"
            
      - name: Generate Release Assets
        run: |
          # Generate changelog from PR data
          CHANGELOG=$(npx ruv-swarm github release-changelog \
            --format markdown)
          
          # Update release notes
          gh release edit ${{ github.ref_name }} \
            --notes "$CHANGELOG"
          
          # Generate and upload assets
          npx ruv-swarm github release-assets \
            --changelog \
            --binaries \
            --documentation
            
      - name: Upload Release Assets
        run: |
          # Upload generated assets to GitHub release
          for file in dist/*; do
            gh release upload ${{ github.ref_name }} "$file"
          done
          
      - name: Publish Release
        run: |
          # Publish to package registries
          npx ruv-swarm github release-publish \
            --platforms all
          
          # Create announcement issue
          gh issue create \
            --title "🚀 Released ${{ github.ref_name }}" \
            --body "See [release notes](https://github.com/${{ github.repository }}/releases/tag/${{ github.ref_name }})" \
            --label "announcement"
```

### Continuous Deployment
```bash
# Automated deployment pipeline
npx ruv-swarm github cd-pipeline \
  --trigger "merge-to-main" \
  --auto-version \
  --deploy-on-success \
  --rollback-on-failure
```

## Release Validation

### Pre-Release Checks
```bash
# Comprehensive validation
npx ruv-swarm github release-validate \
  --checks "
    version-conflicts,
    dependency-compatibility,
    api-breaking-changes,
    security-vulnerabilities,
    performance-regression,
    documentation-completeness
  " \
  --block-on-failure
```

### Compatibility Testing
```bash
# Test backward compatibility
npx ruv-swarm github compat-test \
  --previous-versions "v1.0,v1.1,v1.2" \
  --api-contracts \
  --data-migrations \
  --generate-report
```

### Security Scanning
```bash
# Security validation
npx ruv-swarm github release-security \
  --scan-dependencies \
  --check-secrets \
  --audit-permissions \
  --sign-artifacts
```

## Monitoring & Rollback

### Release Monitoring
```bash
# Monitor release health
npx ruv-swarm github release-monitor \
  --version v2.0.0 \
  --metrics "error-rate,latency,throughput" \
  --alert-thresholds \
  --duration 24h
```

### Automated Rollback
```bash
# Configure auto-rollback
npx ruv-swarm github rollback-config \
  --triggers '{
    "error-rate": ">5%",
    "latency-p99": ">1000ms",
    "availability": "<99.9%"
  }' \
  --grace-period 5m \
  --notify-on-rollback
```

### Release Analytics
```bash
# Analyze release performance
npx ruv-swarm github release-analytics \
  --version v2.0.0 \
  --compare-with v1.9.0 \
  --metrics "adoption,performance,stability" \
  --generate-insights
```

## Documentation

### Auto-Generated Docs
```bash
# Update documentation
npx ruv-swarm github release-docs \
  --api-changes \
  --migration-guide \
  --example-updates \
  --publish-to "docs-site,wiki"
```

### Release Notes
```markdown
<!-- Auto-generated release notes template -->
# Release v2.0.0

## 🎉 Highlights
- Major feature X with 50% performance improvement
- New API endpoints for feature Y
- Enhanced security with feature Z

## 🚀 Features
### Feature Name (#PR)
Detailed description of the feature...

## 🐛 Bug Fixes
### Fixed issue with... (#PR)
Description of the fix...

## 💥 Breaking Changes
### API endpoint renamed
- Before: `/api/old-endpoint`
- After: `/api/new-endpoint`
- Migration: Update all client calls...

## 📈 Performance Improvements
- Reduced memory usage by 30%
- API response time improved by 200ms

## 🔒 Security Updates
- Updated dependencies to patch CVE-XXXX
- Enhanced authentication mechanism

## 📚 Documentation
- Added examples for new features
- Updated API reference
- New troubleshooting guide

## 🙏 Contributors
Thanks to all contributors who made this release possible!
```

## Best Practices

### 1. Release Planning
- Regular release cycles
- Feature freeze periods
- Beta testing phases
- Clear communication

### 2. Automation
- Comprehensive CI/CD
- Automated testing
- Progressive rollouts
- Monitoring and alerts

### 3. Documentation
- Up-to-date changelogs
- Migration guides
- API documentation
- Example updates

## Integration Examples

### NPM Package Release
```bash
# NPM package release
npx ruv-swarm github npm-release \
  --version patch \
  --test-all \
  --publish-beta \
  --tag-latest-on-success
```

### Docker Image Release
```bash
# Docker multi-arch release
npx ruv-swarm github docker-release \
  --platforms "linux/amd64,linux/arm64" \
  --tags "latest,v2.0.0,stable" \
  --scan-vulnerabilities \
  --push-to "dockerhub,gcr,ecr"
```

### Mobile App Release
```bash
# Mobile app store release
npx ruv-swarm github mobile-release \
  --platforms "ios,android" \
  --build-release \
  --submit-review \
  --staged-rollout
```

## Emergency Procedures

### Hotfix Process
```bash
# Emergency hotfix
npx ruv-swarm github emergency-release \
  --severity critical \
  --bypass-checks security-only \
  --fast-track \
  --notify-all
```

### Rollback Procedure
```bash
# Immediate rollback
npx ruv-swarm github rollback \
  --to-version v1.9.9 \
  --reason "Critical bug in v2.0.0" \
  --preserve-data \
  --notify-users
```

See also: [workflow-automation.md](./workflow-automation.md), [multi-repo-swarm.md](./multi-repo-swarm.md)


---

## Source: repo-architect.md

# GitHub Repository Architect

## Purpose
Repository structure optimization and multi-repo management with ruv-swarm coordination for scalable project architecture and development workflows.

## Capabilities
- **Repository structure optimization** with best practices
- **Multi-repository coordination** and synchronization
- **Template management** for consistent project setup
- **Architecture analysis** and improvement recommendations
- **Cross-repo workflow** coordination and management

## Usage Patterns

### 1. Repository Structure Analysis and Optimization
```javascript
// Initialize architecture analysis swarm
mcp__claude-flow__swarm_init { topology: "mesh", maxAgents: 4 }
mcp__claude-flow__agent_spawn { type: "analyst", name: "Structure Analyzer" }
mcp__claude-flow__agent_spawn { type: "architect", name: "Repository Architect" }
mcp__claude-flow__agent_spawn { type: "optimizer", name: "Structure Optimizer" }
mcp__claude-flow__agent_spawn { type: "coordinator", name: "Multi-Repo Coordinator" }

// Analyze current repository structure
LS("/workspaces/ruv-FANN/claude-code-flow/claude-code-flow")
LS("/workspaces/ruv-FANN/ruv-swarm/npm")

// Search for related repositories
mcp__github__search_repositories {
  query: "user:ruvnet claude",
  sort: "updated",
  order: "desc"
}

// Orchestrate structure optimization
mcp__claude-flow__task_orchestrate {
  task: "Analyze and optimize repository structure for scalability and maintainability",
  strategy: "adaptive",
  priority: "medium"
}
```

### 2. Multi-Repository Template Creation
```javascript
// Create standardized repository template
mcp__github__create_repository {
  name: "claude-project-template",
  description: "Standardized template for Claude Code projects with ruv-swarm integration",
  private: false,
  autoInit: true
}

// Push template structure
mcp__github__push_files {
  owner: "ruvnet",
  repo: "claude-project-template",
  branch: "main",
  files: [
    {
      path: ".claude/commands/github/github-modes.md",
      content: "[GitHub modes template]"
    },
    {
      path: ".claude/commands/sparc/sparc-modes.md", 
      content: "[SPARC modes template]"
    },
    {
      path: ".claude/config.json",
      content: JSON.stringify({
        version: "1.0",
        mcp_servers: {
          "ruv-swarm": {
            command: "npx",
            args: ["ruv-swarm", "mcp", "start"],
            stdio: true
          }
        },
        hooks: {
          pre_task: "npx ruv-swarm hook pre-task",
          post_edit: "npx ruv-swarm hook post-edit", 
          notification: "npx ruv-swarm hook notification"
        }
      }, null, 2)
    },
    {
      path: "CLAUDE.md",
      content: "[Standardized CLAUDE.md template]"
    },
    {
      path: "package.json",
      content: JSON.stringify({
        name: "claude-project-template",
        version: "1.0.0",
        description: "Claude Code project with ruv-swarm integration",
        engines: { node: ">=20.0.0" },
        dependencies: {
          "ruv-swarm": "^1.0.11"
        }
      }, null, 2)
    },
    {
      path: "README.md",
      content: `# Claude Project Template

## Quick Start
\`\`\`bash
npx claude-flow init --sparc
npm install
npx claude-flow start --ui
\`\`\`

## Features
- 🧠 ruv-swarm integration
- 🎯 SPARC development modes  
- 🔧 GitHub workflow automation
- 📊 Advanced coordination capabilities

## Documentation
See CLAUDE.md for complete integration instructions.`
    }
  ],
  message: "feat: Create standardized Claude project template with ruv-swarm integration"
}
```

### 3. Cross-Repository Synchronization
```javascript
// Synchronize structure across related repositories
const repositories = [
  "claude-code-flow", 
  "ruv-swarm",
  "claude-extensions"
]

// Update common files across repositories
repositories.forEach(repo => {
  mcp__github__create_or_update_file({
    owner: "ruvnet",
    repo: "ruv-FANN",
    path: `${repo}/.github/workflows/integration.yml`,
    content: `name: Integration Tests
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3
        with: { node-version: '20' }
      - run: npm install && npm test`,
    message: "ci: Standardize integration workflow across repositories",
    branch: "structure/standardization"
  })
})
```

## Batch Architecture Operations

### Complete Repository Architecture Optimization:
```javascript
[Single Message - Repository Architecture Review]:
  // Initialize comprehensive architecture swarm
  mcp__claude-flow__swarm_init { topology: "hierarchical", maxAgents: 6 }
  mcp__claude-flow__agent_spawn { type: "architect", name: "Senior Architect" }
  mcp__claude-flow__agent_spawn { type: "analyst", name: "Structure Analyst" }
  mcp__claude-flow__agent_spawn { type: "optimizer", name: "Performance Optimizer" }
  mcp__claude-flow__agent_spawn { type: "researcher", name: "Best Practices Researcher" }
  mcp__claude-flow__agent_spawn { type: "coordinator", name: "Multi-Repo Coordinator" }
  
  // Analyze current repository structures
  LS("/workspaces/ruv-FANN/claude-code-flow/claude-code-flow")
  LS("/workspaces/ruv-FANN/ruv-swarm/npm") 
  Read("/workspaces/ruv-FANN/claude-code-flow/claude-code-flow/package.json")
  Read("/workspaces/ruv-FANN/ruv-swarm/npm/package.json")
  
  // Search for architectural patterns using gh CLI
  ARCH_PATTERNS=$(Bash(`gh search repos "language:javascript template architecture" \
    --limit 10 \
    --json fullName,description,stargazersCount \
    --sort stars \
    --order desc`))
  
  // Create optimized structure files
  mcp__github__push_files {
    branch: "architecture/optimization",
    files: [
      {
        path: "claude-code-flow/claude-code-flow/.github/ISSUE_TEMPLATE/integration.yml",
        content: "[Integration issue template]"
      },
      {
        path: "claude-code-flow/claude-code-flow/.github/PULL_REQUEST_TEMPLATE.md",
        content: "[Standardized PR template]"
      },
      {
        path: "claude-code-flow/claude-code-flow/docs/ARCHITECTURE.md",
        content: "[Architecture documentation]"
      },
      {
        path: "ruv-swarm/npm/.github/workflows/cross-package-test.yml",
        content: "[Cross-package testing workflow]"
      }
    ],
    message: "feat: Optimize repository architecture for scalability and maintainability"
  }
  
  // Track architecture improvements
  TodoWrite { todos: [
    { id: "arch-analysis", content: "Analyze current repository structure", status: "completed", priority: "high" },
    { id: "arch-research", content: "Research best practices and patterns", status: "completed", priority: "medium" },
    { id: "arch-templates", content: "Create standardized templates", status: "completed", priority: "high" },
    { id: "arch-workflows", content: "Implement improved workflows", status: "completed", priority: "medium" },
    { id: "arch-docs", content: "Document architecture decisions", status: "pending", priority: "medium" }
  ]}
  
  // Store architecture analysis
  mcp__claude-flow__memory_usage {
    action: "store",
    key: "architecture/analysis/results",
    value: {
      timestamp: Date.now(),
      repositories_analyzed: ["claude-code-flow", "ruv-swarm"],
      optimization_areas: ["structure", "workflows", "templates", "documentation"],
      recommendations: ["standardize_structure", "improve_workflows", "enhance_templates"],
      implementation_status: "in_progress"
    }
  }
```

## Architecture Patterns

### 1. **Monorepo Structure Pattern**
```
ruv-FANN/
├── packages/
│   ├── claude-code-flow/
│   │   ├── src/
│   │   ├── .claude/
│   │   └── package.json
│   ├── ruv-swarm/
│   │   ├── src/
│   │   ├── wasm/
│   │   └── package.json
│   └── shared/
│       ├── types/
│       ├── utils/
│       └── config/
├── tools/
│   ├── build/
│   ├── test/
│   └── deploy/
├── docs/
│   ├── architecture/
│   ├── integration/
│   └── examples/
└── .github/
    ├── workflows/
    ├── templates/
    └── actions/
```

### 2. **Command Structure Pattern**
```
.claude/
├── commands/
│   ├── github/
│   │   ├── github-modes.md
│   │   ├── pr-manager.md
│   │   ├── issue-tracker.md
│   │   └── sync-coordinator.md
│   ├── sparc/
│   │   ├── sparc-modes.md
│   │   ├── coder.md
│   │   └── tester.md
│   └── swarm/
│       ├── coordination.md
│       └── orchestration.md
├── templates/
│   ├── issue.md
│   ├── pr.md
│   └── project.md
└── config.json
```

### 3. **Integration Pattern**
```javascript
const integrationPattern = {
  packages: {
    "claude-code-flow": {
      role: "orchestration_layer",
      dependencies: ["ruv-swarm"],
      provides: ["CLI", "workflows", "commands"]
    },
    "ruv-swarm": {
      role: "coordination_engine", 
      dependencies: [],
      provides: ["MCP_tools", "neural_networks", "memory"]
    }
  },
  communication: "MCP_protocol",
  coordination: "swarm_based",
  state_management: "persistent_memory"
}
```

## Best Practices

### 1. **Structure Optimization**
- Consistent directory organization across repositories
- Standardized configuration files and formats
- Clear separation of concerns and responsibilities
- Scalable architecture for future growth

### 2. **Template Management**
- Reusable project templates for consistency
- Standardized issue and PR templates
- Workflow templates for common operations
- Documentation templates for clarity

### 3. **Multi-Repository Coordination**
- Cross-repository dependency management
- Synchronized version and release management
- Consistent coding standards and practices
- Automated cross-repo validation

### 4. **Documentation Architecture**
- Comprehensive architecture documentation
- Clear integration guides and examples
- Maintainable and up-to-date documentation
- User-friendly onboarding materials

## Monitoring and Analysis

### Architecture Health Metrics:
- Repository structure consistency score
- Documentation coverage percentage
- Cross-repository integration success rate
- Template adoption and usage statistics

### Automated Analysis:
- Structure drift detection
- Best practices compliance checking
- Performance impact analysis
- Scalability assessment and recommendations

## Integration with Development Workflow

### Seamless integration with:
- `/github sync-coordinator` - For cross-repo synchronization
- `/github release-manager` - For coordinated releases
- `/sparc architect` - For detailed architecture design
- `/sparc optimizer` - For performance optimization

### Workflow Enhancement:
- Automated structure validation
- Continuous architecture improvement
- Best practices enforcement
- Documentation generation and maintenance


---

## Source: swarm-issue.md

# Swarm Issue - Issue-Based Swarm Coordination

## Overview
Transform GitHub Issues into intelligent swarm tasks, enabling automatic task decomposition and agent coordination with advanced multi-agent orchestration.

## Core Features

### 1. Issue-to-Swarm Conversion
```bash
# Create swarm from issue using gh CLI
# Get issue details
ISSUE_DATA=$(gh issue view 456 --json title,body,labels,assignees,comments)

# Create swarm from issue
npx ruv-swarm github issue-to-swarm 456 \
  --issue-data "$ISSUE_DATA" \
  --auto-decompose \
  --assign-agents

# Batch process multiple issues
ISSUES=$(gh issue list --label "swarm-ready" --json number,title,body,labels)
npx ruv-swarm github issues-batch \
  --issues "$ISSUES" \
  --parallel

# Update issues with swarm status
echo "$ISSUES" | jq -r '.[].number' | while read -r num; do
  gh issue edit $num --add-label "swarm-processing"
done
```

### 2. Issue Comment Commands
Execute swarm operations via issue comments:

```markdown
<!-- In issue comment -->
/swarm analyze
/swarm decompose 5
/swarm assign @agent-coder
/swarm estimate
/swarm start
```

### 3. Issue Templates for Swarms

```markdown
<!-- .github/ISSUE_TEMPLATE/swarm-task.yml -->
name: Swarm Task
description: Create a task for AI swarm processing
body:
  - type: dropdown
    id: topology
    attributes:
      label: Swarm Topology
      options:
        - mesh
        - hierarchical
        - ring
        - star
  - type: input
    id: agents
    attributes:
      label: Required Agents
      placeholder: "coder, tester, analyst"
  - type: textarea
    id: tasks
    attributes:
      label: Task Breakdown
      placeholder: |
        1. Task one description
        2. Task two description
```

## Issue Label Automation

### Auto-Label Based on Content
```javascript
// .github/swarm-labels.json
{
  "rules": [
    {
      "keywords": ["bug", "error", "broken"],
      "labels": ["bug", "swarm-debugger"],
      "agents": ["debugger", "tester"]
    },
    {
      "keywords": ["feature", "implement", "add"],
      "labels": ["enhancement", "swarm-feature"],
      "agents": ["architect", "coder", "tester"]
    },
    {
      "keywords": ["slow", "performance", "optimize"],
      "labels": ["performance", "swarm-optimizer"],
      "agents": ["analyst", "optimizer"]
    }
  ]
}
```

### Dynamic Agent Assignment
```bash
# Assign agents based on issue content
npx ruv-swarm github issue-analyze 456 \
  --suggest-agents \
  --estimate-complexity \
  --create-subtasks
```

## Issue Swarm Commands

### Initialize from Issue
```bash
# Create swarm with full issue context using gh CLI
# Get complete issue data
ISSUE=$(gh issue view 456 --json title,body,labels,assignees,comments,projectItems)

# Get referenced issues and PRs
REFERENCES=$(gh issue view 456 --json body --jq '.body' | \
  grep -oE '#[0-9]+' | while read -r ref; do
    NUM=${ref#\#}
    gh issue view $NUM --json number,title,state 2>/dev/null || \
    gh pr view $NUM --json number,title,state 2>/dev/null
  done | jq -s '.')

# Initialize swarm
npx ruv-swarm github issue-init 456 \
  --issue-data "$ISSUE" \
  --references "$REFERENCES" \
  --load-comments \
  --analyze-references \
  --auto-topology

# Add swarm initialization comment
gh issue comment 456 --body "🐝 Swarm initialized for this issue"
```

### Task Decomposition
```bash
# Break down issue into subtasks with gh CLI
# Get issue body
ISSUE_BODY=$(gh issue view 456 --json body --jq '.body')

# Decompose into subtasks
SUBTASKS=$(npx ruv-swarm github issue-decompose 456 \
  --body "$ISSUE_BODY" \
  --max-subtasks 10 \
  --assign-priorities)

# Update issue with checklist
CHECKLIST=$(echo "$SUBTASKS" | jq -r '.tasks[] | "- [ ] " + .description')
UPDATED_BODY="$ISSUE_BODY

## Subtasks
$CHECKLIST"

gh issue edit 456 --body "$UPDATED_BODY"

# Create linked issues for major subtasks
echo "$SUBTASKS" | jq -r '.tasks[] | select(.priority == "high")' | while read -r task; do
  TITLE=$(echo "$task" | jq -r '.title')
  BODY=$(echo "$task" | jq -r '.description')
  
  gh issue create \
    --title "$TITLE" \
    --body "$BODY

Parent issue: #456" \
    --label "subtask"
done
```

### Progress Tracking
```bash
# Update issue with swarm progress using gh CLI
# Get current issue state
CURRENT=$(gh issue view 456 --json body,labels)

# Get swarm progress
PROGRESS=$(npx ruv-swarm github issue-progress 456)

# Update checklist in issue body
UPDATED_BODY=$(echo "$CURRENT" | jq -r '.body' | \
  npx ruv-swarm github update-checklist --progress "$PROGRESS")

# Edit issue with updated body
gh issue edit 456 --body "$UPDATED_BODY"

# Post progress summary as comment
SUMMARY=$(echo "$PROGRESS" | jq -r '
"## 📊 Progress Update

**Completion**: \(.completion)%
**ETA**: \(.eta)

### Completed Tasks
\(.completed | map("- ✅ " + .) | join("\n"))

### In Progress
\(.in_progress | map("- 🔄 " + .) | join("\n"))

### Remaining
\(.remaining | map("- ⏳ " + .) | join("\n"))

---
🤖 Automated update by swarm agent"')

gh issue comment 456 --body "$SUMMARY"

# Update labels based on progress
if [[ $(echo "$PROGRESS" | jq -r '.completion') -eq 100 ]]; then
  gh issue edit 456 --add-label "ready-for-review" --remove-label "in-progress"
fi
```

## Advanced Features

### 1. Issue Dependencies
```bash
# Handle issue dependencies
npx ruv-swarm github issue-deps 456 \
  --resolve-order \
  --parallel-safe \
  --update-blocking
```

### 2. Epic Management
```bash
# Coordinate epic-level swarms
npx ruv-swarm github epic-swarm \
  --epic 123 \
  --child-issues "456,457,458" \
  --orchestrate
```

### 3. Issue Templates
```bash
# Generate issue from swarm analysis
npx ruv-swarm github create-issues \
  --from-analysis \
  --template "bug-report" \
  --auto-assign
```

## Workflow Integration

### GitHub Actions for Issues
```yaml
# .github/workflows/issue-swarm.yml
name: Issue Swarm Handler
on:
  issues:
    types: [opened, labeled, commented]

jobs:
  swarm-process:
    runs-on: ubuntu-latest
    steps:
      - name: Process Issue
        uses: ruvnet/swarm-action@v1
        with:
          command: |
            if [[ "${{ github.event.label.name }}" == "swarm-ready" ]]; then
              npx ruv-swarm github issue-init ${{ github.event.issue.number }}
            fi
```

### Issue Board Integration
```bash
# Sync with project board
npx ruv-swarm github issue-board-sync \
  --project "Development" \
  --column-mapping '{
    "To Do": "pending",
    "In Progress": "active",
    "Done": "completed"
  }'
```

## Issue Types & Strategies

### Bug Reports
```bash
# Specialized bug handling
npx ruv-swarm github bug-swarm 456 \
  --reproduce \
  --isolate \
  --fix \
  --test
```

### Feature Requests
```bash
# Feature implementation swarm
npx ruv-swarm github feature-swarm 456 \
  --design \
  --implement \
  --document \
  --demo
```

### Technical Debt
```bash
# Refactoring swarm
npx ruv-swarm github debt-swarm 456 \
  --analyze-impact \
  --plan-migration \
  --execute \
  --validate
```

## Automation Examples

### Auto-Close Stale Issues
```bash
# Process stale issues with swarm using gh CLI
# Find stale issues
STALE_DATE=$(date -d '30 days ago' --iso-8601)
STALE_ISSUES=$(gh issue list --state open --json number,title,updatedAt,labels \
  --jq ".[] | select(.updatedAt < \"$STALE_DATE\")")

# Analyze each stale issue
echo "$STALE_ISSUES" | jq -r '.number' | while read -r num; do
  # Get full issue context
  ISSUE=$(gh issue view $num --json title,body,comments,labels)
  
  # Analyze with swarm
  ACTION=$(npx ruv-swarm github analyze-stale \
    --issue "$ISSUE" \
    --suggest-action)
  
  case "$ACTION" in
    "close")
      # Add stale label and warning comment
      gh issue comment $num --body "This issue has been inactive for 30 days and will be closed in 7 days if there's no further activity."
      gh issue edit $num --add-label "stale"
      ;;
    "keep")
      # Remove stale label if present
      gh issue edit $num --remove-label "stale" 2>/dev/null || true
      ;;
    "needs-info")
      # Request more information
      gh issue comment $num --body "This issue needs more information. Please provide additional context or it may be closed as stale."
      gh issue edit $num --add-label "needs-info"
      ;;
  esac
done

# Close issues that have been stale for 37+ days
gh issue list --label stale --state open --json number,updatedAt \
  --jq ".[] | select(.updatedAt < \"$(date -d '37 days ago' --iso-8601)\") | .number" | \
  while read -r num; do
    gh issue close $num --comment "Closing due to inactivity. Feel free to reopen if this is still relevant."
  done
```

### Issue Triage
```bash
# Automated triage system
npx ruv-swarm github triage \
  --unlabeled \
  --analyze-content \
  --suggest-labels \
  --assign-priority
```

### Duplicate Detection
```bash
# Find duplicate issues
npx ruv-swarm github find-duplicates \
  --threshold 0.8 \
  --link-related \
  --close-duplicates
```

## Integration Patterns

### 1. Issue-PR Linking
```bash
# Link issues to PRs automatically
npx ruv-swarm github link-pr \
  --issue 456 \
  --pr 789 \
  --update-both
```

### 2. Milestone Coordination
```bash
# Coordinate milestone swarms
npx ruv-swarm github milestone-swarm \
  --milestone "v2.0" \
  --parallel-issues \
  --track-progress
```

### 3. Cross-Repo Issues
```bash
# Handle issues across repositories
npx ruv-swarm github cross-repo \
  --issue "org/repo#456" \
  --related "org/other-repo#123" \
  --coordinate
```

## Metrics & Analytics

### Issue Resolution Time
```bash
# Analyze swarm performance
npx ruv-swarm github issue-metrics \
  --issue 456 \
  --metrics "time-to-close,agent-efficiency,subtask-completion"
```

### Swarm Effectiveness
```bash
# Generate effectiveness report
npx ruv-swarm github effectiveness \
  --issues "closed:>2024-01-01" \
  --compare "with-swarm,without-swarm"
```

## Best Practices

### 1. Issue Templates
- Include swarm configuration options
- Provide task breakdown structure
- Set clear acceptance criteria
- Include complexity estimates

### 2. Label Strategy
- Use consistent swarm-related labels
- Map labels to agent types
- Priority indicators for swarm
- Status tracking labels

### 3. Comment Etiquette
- Clear command syntax
- Progress updates in threads
- Summary comments for decisions
- Link to relevant PRs

## Security & Permissions

1. **Command Authorization**: Validate user permissions before executing commands
2. **Rate Limiting**: Prevent spam and abuse of issue commands
3. **Audit Logging**: Track all swarm operations on issues
4. **Data Privacy**: Respect private repository settings

## Examples

### Complex Bug Investigation
```bash
# Issue #789: Memory leak in production
npx ruv-swarm github issue-init 789 \
  --topology hierarchical \
  --agents "debugger,analyst,tester,monitor" \
  --priority critical \
  --reproduce-steps
```

### Feature Implementation
```bash
# Issue #234: Add OAuth integration
npx ruv-swarm github issue-init 234 \
  --topology mesh \
  --agents "architect,coder,security,tester" \
  --create-design-doc \
  --estimate-effort
```

### Documentation Update
```bash
# Issue #567: Update API documentation
npx ruv-swarm github issue-init 567 \
  --topology ring \
  --agents "researcher,writer,reviewer" \
  --check-links \
  --validate-examples
```

## Swarm Coordination Features

### Multi-Agent Issue Processing
```bash
# Initialize issue-specific swarm with optimal topology
mcp__claude-flow__swarm_init { topology: "hierarchical", maxAgents: 8 }
mcp__claude-flow__agent_spawn { type: "coordinator", name: "Issue Coordinator" }
mcp__claude-flow__agent_spawn { type: "analyst", name: "Issue Analyzer" }
mcp__claude-flow__agent_spawn { type: "coder", name: "Solution Developer" }
mcp__claude-flow__agent_spawn { type: "tester", name: "Validation Engineer" }

# Store issue context in swarm memory
mcp__claude-flow__memory_usage {
  action: "store",
  key: "issue/#{issue_number}/context",
  value: { title: "issue_title", labels: ["labels"], complexity: "high" }
}

# Orchestrate issue resolution workflow
mcp__claude-flow__task_orchestrate {
  task: "Coordinate multi-agent issue resolution with progress tracking",
  strategy: "adaptive",
  priority: "high"
}
```

### Automated Swarm Hooks Integration
```javascript
// Pre-hook: Issue Analysis and Swarm Setup
const preHook = async (issue) => {
  // Initialize swarm with issue-specific topology
  const topology = determineTopology(issue.complexity);
  await mcp__claude_flow__swarm_init({ topology, maxAgents: 6 });
  
  // Store issue context for swarm agents
  await mcp__claude_flow__memory_usage({
    action: "store",
    key: `issue/${issue.number}/metadata`,
    value: { issue, analysis: await analyzeIssue(issue) }
  });
};

// Post-hook: Progress Updates and Coordination
const postHook = async (results) => {
  // Update issue with swarm progress
  await updateIssueProgress(results);
  
  // Generate follow-up tasks
  await createFollowupTasks(results.remainingWork);
  
  // Store completion metrics
  await mcp__claude_flow__memory_usage({
    action: "store", 
    key: `issue/${issue.number}/completion`,
    value: { metrics: results.metrics, timestamp: Date.now() }
  });
};
```

See also: [swarm-pr.md](./swarm-pr.md), [sync-coordinator.md](./sync-coordinator.md), [workflow-automation.md](./workflow-automation.md)


---

## Source: swarm-pr.md

# Swarm PR - Managing Swarms through Pull Requests

## Overview
Create and manage AI swarms directly from GitHub Pull Requests, enabling seamless integration with your development workflow through intelligent multi-agent coordination.

## Core Features

### 1. PR-Based Swarm Creation
```bash
# Create swarm from PR description using gh CLI
gh pr view 123 --json body,title,labels,files | npx ruv-swarm swarm create-from-pr

# Auto-spawn agents based on PR labels
gh pr view 123 --json labels | npx ruv-swarm swarm auto-spawn

# Create swarm with PR context
gh pr view 123 --json body,labels,author,assignees | \
  npx ruv-swarm swarm init --from-pr-data
```

### 2. PR Comment Commands
Execute swarm commands via PR comments:

```markdown
<!-- In PR comment -->
/swarm init mesh 6
/swarm spawn coder "Implement authentication"
/swarm spawn tester "Write unit tests"
/swarm status
```

### 3. Automated PR Workflows

```yaml
# .github/workflows/swarm-pr.yml
name: Swarm PR Handler
on:
  pull_request:
    types: [opened, labeled]
  issue_comment:
    types: [created]

jobs:
  swarm-handler:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Handle Swarm Command
        run: |
          if [[ "${{ github.event.comment.body }}" == /swarm* ]]; then
            npx ruv-swarm github handle-comment \
              --pr ${{ github.event.pull_request.number }} \
              --comment "${{ github.event.comment.body }}"
          fi
```

## PR Label Integration

### Automatic Agent Assignment
Map PR labels to agent types:

```json
{
  "label-mapping": {
    "bug": ["debugger", "tester"],
    "feature": ["architect", "coder", "tester"],
    "refactor": ["analyst", "coder"],
    "docs": ["researcher", "writer"],
    "performance": ["analyst", "optimizer"]
  }
}
```

### Label-Based Topology
```bash
# Small PR (< 100 lines): ring topology
# Medium PR (100-500 lines): mesh topology  
# Large PR (> 500 lines): hierarchical topology
npx ruv-swarm github pr-topology --pr 123
```

## PR Swarm Commands

### Initialize from PR
```bash
# Create swarm with PR context using gh CLI
PR_DIFF=$(gh pr diff 123)
PR_INFO=$(gh pr view 123 --json title,body,labels,files,reviews)

npx ruv-swarm github pr-init 123 \
  --auto-agents \
  --pr-data "$PR_INFO" \
  --diff "$PR_DIFF" \
  --analyze-impact
```

### Progress Updates
```bash
# Post swarm progress to PR using gh CLI
PROGRESS=$(npx ruv-swarm github pr-progress 123 --format markdown)

gh pr comment 123 --body "$PROGRESS"

# Update PR labels based on progress
if [[ $(echo "$PROGRESS" | grep -o '[0-9]\+%' | sed 's/%//') -gt 90 ]]; then
  gh pr edit 123 --add-label "ready-for-review"
fi
```

### Code Review Integration
```bash
# Create review agents with gh CLI integration
PR_FILES=$(gh pr view 123 --json files --jq '.files[].path')

# Run swarm review
REVIEW_RESULTS=$(npx ruv-swarm github pr-review 123 \
  --agents "security,performance,style" \
  --files "$PR_FILES")

# Post review comments using gh CLI
echo "$REVIEW_RESULTS" | jq -r '.comments[]' | while read -r comment; do
  FILE=$(echo "$comment" | jq -r '.file')
  LINE=$(echo "$comment" | jq -r '.line')
  BODY=$(echo "$comment" | jq -r '.body')
  
  gh pr review 123 --comment --body "$BODY"
done
```

## Advanced Features

### 1. Multi-PR Swarm Coordination
```bash
# Coordinate swarms across related PRs
npx ruv-swarm github multi-pr \
  --prs "123,124,125" \
  --strategy "parallel" \
  --share-memory
```

### 2. PR Dependency Analysis
```bash
# Analyze PR dependencies
npx ruv-swarm github pr-deps 123 \
  --spawn-agents \
  --resolve-conflicts
```

### 3. Automated PR Fixes
```bash
# Auto-fix PR issues
npx ruv-swarm github pr-fix 123 \
  --issues "lint,test-failures" \
  --commit-fixes
```

## Best Practices

### 1. PR Templates
```markdown
<!-- .github/pull_request_template.md -->
## Swarm Configuration
- Topology: [mesh/hierarchical/ring/star]
- Max Agents: [number]
- Auto-spawn: [yes/no]
- Priority: [high/medium/low]

## Tasks for Swarm
- [ ] Task 1 description
- [ ] Task 2 description
```

### 2. Status Checks
```yaml
# Require swarm completion before merge
required_status_checks:
  contexts:
    - "swarm/tasks-complete"
    - "swarm/tests-pass"
    - "swarm/review-approved"
```

### 3. PR Merge Automation
```bash
# Auto-merge when swarm completes using gh CLI
# Check swarm completion status
SWARM_STATUS=$(npx ruv-swarm github pr-status 123)

if [[ "$SWARM_STATUS" == "complete" ]]; then
  # Check review requirements
  REVIEWS=$(gh pr view 123 --json reviews --jq '.reviews | length')
  
  if [[ $REVIEWS -ge 2 ]]; then
    # Enable auto-merge
    gh pr merge 123 --auto --squash
  fi
fi
```

## Webhook Integration

### Setup Webhook Handler
```javascript
// webhook-handler.js
const { createServer } = require('http');
const { execSync } = require('child_process');

createServer((req, res) => {
  if (req.url === '/github-webhook') {
    const event = JSON.parse(body);
    
    if (event.action === 'opened' && event.pull_request) {
      execSync(`npx ruv-swarm github pr-init ${event.pull_request.number}`);
    }
    
    res.writeHead(200);
    res.end('OK');
  }
}).listen(3000);
```

## Examples

### Feature Development PR
```bash
# PR #456: Add user authentication
npx ruv-swarm github pr-init 456 \
  --topology hierarchical \
  --agents "architect,coder,tester,security" \
  --auto-assign-tasks
```

### Bug Fix PR
```bash
# PR #789: Fix memory leak
npx ruv-swarm github pr-init 789 \
  --topology mesh \
  --agents "debugger,analyst,tester" \
  --priority high
```

### Documentation PR
```bash
# PR #321: Update API docs
npx ruv-swarm github pr-init 321 \
  --topology ring \
  --agents "researcher,writer,reviewer" \
  --validate-links
```

## Metrics & Reporting

### PR Swarm Analytics
```bash
# Generate PR swarm report
npx ruv-swarm github pr-report 123 \
  --metrics "completion-time,agent-efficiency,token-usage" \
  --format markdown
```

### Dashboard Integration
```bash
# Export to GitHub Insights
npx ruv-swarm github export-metrics \
  --pr 123 \
  --to-insights
```

## Security Considerations

1. **Token Permissions**: Ensure GitHub tokens have appropriate scopes
2. **Command Validation**: Validate all PR comments before execution
3. **Rate Limiting**: Implement rate limits for PR operations
4. **Audit Trail**: Log all swarm operations for compliance

## Integration with Claude Code

When using with Claude Code:
1. Claude Code reads PR diff and context
2. Swarm coordinates approach based on PR type
3. Agents work in parallel on different aspects
4. Progress updates posted to PR automatically
5. Final review performed before marking ready

## Advanced Swarm PR Coordination

### Multi-Agent PR Analysis
```bash
# Initialize PR-specific swarm with intelligent topology selection
mcp__claude-flow__swarm_init { topology: "mesh", maxAgents: 8 }
mcp__claude-flow__agent_spawn { type: "coordinator", name: "PR Coordinator" }
mcp__claude-flow__agent_spawn { type: "reviewer", name: "Code Reviewer" }
mcp__claude-flow__agent_spawn { type: "tester", name: "Test Engineer" }
mcp__claude-flow__agent_spawn { type: "analyst", name: "Impact Analyzer" }
mcp__claude-flow__agent_spawn { type: "optimizer", name: "Performance Optimizer" }

# Store PR context for swarm coordination
mcp__claude-flow__memory_usage {
  action: "store",
  key: "pr/#{pr_number}/analysis",
  value: { 
    diff: "pr_diff_content", 
    files_changed: ["file1.js", "file2.py"],
    complexity_score: 8.5,
    risk_assessment: "medium"
  }
}

# Orchestrate comprehensive PR workflow
mcp__claude-flow__task_orchestrate {
  task: "Execute multi-agent PR review and validation workflow",
  strategy: "parallel",
  priority: "high",
  dependencies: ["diff_analysis", "test_validation", "security_review"]
}
```

### Swarm-Coordinated PR Lifecycle
```javascript
// Pre-hook: PR Initialization and Swarm Setup
const prPreHook = async (prData) => {
  // Analyze PR complexity for optimal swarm configuration
  const complexity = await analyzePRComplexity(prData);
  const topology = complexity > 7 ? "hierarchical" : "mesh";
  
  // Initialize swarm with PR-specific configuration
  await mcp__claude_flow__swarm_init({ topology, maxAgents: 8 });
  
  // Store comprehensive PR context
  await mcp__claude_flow__memory_usage({
    action: "store",
    key: `pr/${prData.number}/context`,
    value: {
      pr: prData,
      complexity,
      agents_assigned: await getOptimalAgents(prData),
      timeline: generateTimeline(prData)
    }
  });
  
  // Coordinate initial agent synchronization
  await mcp__claude_flow__coordination_sync({ swarmId: "current" });
};

// Post-hook: PR Completion and Metrics
const prPostHook = async (results) => {
  // Generate comprehensive PR completion report
  const report = await generatePRReport(results);
  
  // Update PR with final swarm analysis
  await updatePRWithResults(report);
  
  // Store completion metrics for future optimization
  await mcp__claude_flow__memory_usage({
    action: "store",
    key: `pr/${results.number}/completion`,
    value: {
      completion_time: results.duration,
      agent_efficiency: results.agentMetrics,
      quality_score: results.qualityAssessment,
      lessons_learned: results.insights
    }
  });
};
```

### Intelligent PR Merge Coordination
```bash
# Coordinate merge decision with swarm consensus
mcp__claude-flow__coordination_sync { swarmId: "pr-review-swarm" }

# Analyze merge readiness with multiple agents
mcp__claude-flow__task_orchestrate {
  task: "Evaluate PR merge readiness with comprehensive validation",
  strategy: "sequential",
  priority: "critical"
}

# Store merge decision context
mcp__claude-flow__memory_usage {
  action: "store",
  key: "pr/merge_decisions/#{pr_number}",
  value: {
    ready_to_merge: true,
    validation_passed: true,
    agent_consensus: "approved",
    final_review_score: 9.2
  }
}
```

See also: [swarm-issue.md](./swarm-issue.md), [sync-coordinator.md](./sync-coordinator.md), [workflow-automation.md](./workflow-automation.md)


---

## Source: sync-coordinator.md

# GitHub Sync Coordinator

## Purpose
Multi-package synchronization and version alignment with ruv-swarm coordination for seamless integration between claude-code-flow and ruv-swarm packages through intelligent multi-agent orchestration.

## Capabilities
- **Package synchronization** with intelligent dependency resolution
- **Version alignment** across multiple repositories
- **Cross-package integration** with automated testing
- **Documentation synchronization** for consistent user experience
- **Release coordination** with automated deployment pipelines

## Tools Available
- `mcp__github__push_files`
- `mcp__github__create_or_update_file`
- `mcp__github__get_file_contents`
- `mcp__github__create_pull_request`
- `mcp__github__search_repositories`
- `mcp__claude-flow__*` (all swarm coordination tools)
- `TodoWrite`, `TodoRead`, `Task`, `Bash`, `Read`, `Write`, `Edit`, `MultiEdit`

## Usage Patterns

### 1. Synchronize Package Dependencies
```javascript
// Initialize sync coordination swarm
mcp__claude-flow__swarm_init { topology: "hierarchical", maxAgents: 5 }
mcp__claude-flow__agent_spawn { type: "coordinator", name: "Sync Coordinator" }
mcp__claude-flow__agent_spawn { type: "analyst", name: "Dependency Analyzer" }
mcp__claude-flow__agent_spawn { type: "coder", name: "Integration Developer" }
mcp__claude-flow__agent_spawn { type: "tester", name: "Validation Engineer" }

// Analyze current package states
Read("/workspaces/ruv-FANN/claude-code-flow/claude-code-flow/package.json")
Read("/workspaces/ruv-FANN/ruv-swarm/npm/package.json")

// Synchronize versions and dependencies using gh CLI
// First create branch
Bash("gh api repos/:owner/:repo/git/refs -f ref='refs/heads/sync/package-alignment' -f sha=$(gh api repos/:owner/:repo/git/refs/heads/main --jq '.object.sha')")

// Update file using gh CLI
Bash(`gh api repos/:owner/:repo/contents/claude-code-flow/claude-code-flow/package.json \
  --method PUT \
  -f message="feat: Align Node.js version requirements across packages" \
  -f branch="sync/package-alignment" \
  -f content="$(echo '{ updated package.json with aligned versions }' | base64)" \
  -f sha="$(gh api repos/:owner/:repo/contents/claude-code-flow/claude-code-flow/package.json?ref=sync/package-alignment --jq '.sha')")`)

// Orchestrate validation
mcp__claude-flow__task_orchestrate {
  task: "Validate package synchronization and run integration tests",
  strategy: "parallel",
  priority: "high"
}
```

### 2. Documentation Synchronization
```javascript
// Synchronize CLAUDE.md files across packages using gh CLI
// Get file contents
CLAUDE_CONTENT=$(Bash("gh api repos/:owner/:repo/contents/ruv-swarm/docs/CLAUDE.md --jq '.content' | base64 -d"))

// Update claude-code-flow CLAUDE.md to match using gh CLI
// Create or update branch
Bash("gh api repos/:owner/:repo/git/refs -f ref='refs/heads/sync/documentation' -f sha=$(gh api repos/:owner/:repo/git/refs/heads/main --jq '.object.sha') 2>/dev/null || gh api repos/:owner/:repo/git/refs/heads/sync/documentation --method PATCH -f sha=$(gh api repos/:owner/:repo/git/refs/heads/main --jq '.object.sha')")

// Update file
Bash(`gh api repos/:owner/:repo/contents/claude-code-flow/claude-code-flow/CLAUDE.md \
  --method PUT \
  -f message="docs: Synchronize CLAUDE.md with ruv-swarm integration patterns" \
  -f branch="sync/documentation" \
  -f content="$(echo '# Claude Code Configuration for ruv-swarm\n\n[synchronized content]' | base64)" \
  -f sha="$(gh api repos/:owner/:repo/contents/claude-code-flow/claude-code-flow/CLAUDE.md?ref=sync/documentation --jq '.sha' 2>/dev/null || echo '')")`)

// Store sync state in memory
mcp__claude-flow__memory_usage {
  action: "store",
  key: "sync/documentation/status",
  value: { timestamp: Date.now(), status: "synchronized", files: ["CLAUDE.md"] }
}
```

### 3. Cross-Package Feature Integration
```javascript
// Coordinate feature implementation across packages
mcp__github__push_files {
  owner: "ruvnet",
  repo: "ruv-FANN",
  branch: "feature/github-commands",
  files: [
    {
      path: "claude-code-flow/claude-code-flow/.claude/commands/github/github-modes.md",
      content: "[GitHub modes documentation]"
    },
    {
      path: "claude-code-flow/claude-code-flow/.claude/commands/github/pr-manager.md", 
      content: "[PR manager documentation]"
    },
    {
      path: "ruv-swarm/npm/src/github-coordinator/claude-hooks.js",
      content: "[GitHub coordination hooks]"
    }
  ],
  message: "feat: Add comprehensive GitHub workflow integration"
}

// Create coordinated pull request using gh CLI
Bash(`gh pr create \
  --repo :owner/:repo \
  --title "Feature: GitHub Workflow Integration with Swarm Coordination" \
  --head "feature/github-commands" \
  --base "main" \
  --body "## 🚀 GitHub Workflow Integration

### Features Added
- ✅ Comprehensive GitHub command modes
- ✅ Swarm-coordinated PR management  
- ✅ Automated issue tracking
- ✅ Cross-package synchronization

### Integration Points
- Claude-code-flow: GitHub command modes in .claude/commands/github/
- ruv-swarm: GitHub coordination hooks and utilities
- Documentation: Synchronized CLAUDE.md instructions

### Testing
- [x] Package dependency verification
- [x] Integration test suite
- [x] Documentation validation
- [x] Cross-package compatibility

### Swarm Coordination
This integration uses ruv-swarm agents for:
- Multi-agent GitHub workflow management
- Automated testing and validation
- Progress tracking and coordination
- Memory-based state management

---
🤖 Generated with Claude Code using ruv-swarm coordination`
}
```

## Batch Synchronization Example

### Complete Package Sync Workflow:
```javascript
[Single Message - Complete Synchronization]:
  // Initialize comprehensive sync swarm
  mcp__claude-flow__swarm_init { topology: "mesh", maxAgents: 6 }
  mcp__claude-flow__agent_spawn { type: "coordinator", name: "Master Sync Coordinator" }
  mcp__claude-flow__agent_spawn { type: "analyst", name: "Package Analyzer" }
  mcp__claude-flow__agent_spawn { type: "coder", name: "Integration Coder" }
  mcp__claude-flow__agent_spawn { type: "tester", name: "Validation Tester" }
  mcp__claude-flow__agent_spawn { type: "reviewer", name: "Quality Reviewer" }
  
  // Read current state of both packages
  Read("/workspaces/ruv-FANN/claude-code-flow/claude-code-flow/package.json")
  Read("/workspaces/ruv-FANN/ruv-swarm/npm/package.json")
  Read("/workspaces/ruv-FANN/claude-code-flow/claude-code-flow/CLAUDE.md")
  Read("/workspaces/ruv-FANN/ruv-swarm/docs/CLAUDE.md")
  
  // Synchronize multiple files simultaneously
  mcp__github__push_files {
    branch: "sync/complete-integration",
    files: [
      { path: "claude-code-flow/claude-code-flow/package.json", content: "[aligned package.json]" },
      { path: "claude-code-flow/claude-code-flow/CLAUDE.md", content: "[synchronized CLAUDE.md]" },
      { path: "claude-code-flow/claude-code-flow/.claude/commands/github/github-modes.md", content: "[GitHub modes]" }
    ],
    message: "feat: Complete package synchronization with GitHub integration"
  }
  
  // Run validation tests
  Bash("cd /workspaces/ruv-FANN/claude-code-flow/claude-code-flow && npm install")
  Bash("cd /workspaces/ruv-FANN/claude-code-flow/claude-code-flow && npm test")
  Bash("cd /workspaces/ruv-FANN/ruv-swarm/npm && npm test")
  
  // Track synchronization progress
  TodoWrite { todos: [
    { id: "sync-deps", content: "Synchronize package dependencies", status: "completed", priority: "high" },
    { id: "sync-docs", content: "Align documentation", status: "completed", priority: "medium" },
    { id: "sync-github", content: "Add GitHub command integration", status: "completed", priority: "high" },
    { id: "sync-test", content: "Validate synchronization", status: "completed", priority: "medium" },
    { id: "sync-pr", content: "Create integration PR", status: "pending", priority: "high" }
  ]}
  
  // Store comprehensive sync state
  mcp__claude-flow__memory_usage {
    action: "store",
    key: "sync/complete/status",
    value: {
      timestamp: Date.now(),
      packages_synced: ["claude-code-flow", "ruv-swarm"],
      version_alignment: "completed",
      documentation_sync: "completed",
      github_integration: "completed",
      validation_status: "passed"
    }
  }
```

## Synchronization Strategies

### 1. **Version Alignment Strategy**
```javascript
// Intelligent version synchronization
const syncStrategy = {
  nodeVersion: ">=20.0.0",  // Align to highest requirement
  dependencies: {
    "better-sqlite3": "^12.2.0",  // Use latest stable
    "ws": "^8.14.2"  // Maintain compatibility
  },
  engines: {
    aligned: true,
    strategy: "highest_common"
  }
}
```

### 2. **Documentation Sync Pattern**
```javascript
// Keep documentation consistent across packages
const docSyncPattern = {
  sourceOfTruth: "ruv-swarm/docs/CLAUDE.md",
  targets: [
    "claude-code-flow/claude-code-flow/CLAUDE.md",
    "CLAUDE.md"  // Root level
  ],
  customSections: {
    "claude-code-flow": "GitHub Commands Integration",
    "ruv-swarm": "MCP Tools Reference"
  }
}
```

### 3. **Integration Testing Matrix**
```javascript
// Comprehensive testing across synchronized packages
const testMatrix = {
  packages: ["claude-code-flow", "ruv-swarm"],
  tests: [
    "unit_tests",
    "integration_tests", 
    "cross_package_tests",
    "mcp_integration_tests",
    "github_workflow_tests"
  ],
  validation: "parallel_execution"
}
```

## Best Practices

### 1. **Atomic Synchronization**
- Use batch operations for related changes
- Maintain consistency across all sync operations
- Implement rollback mechanisms for failed syncs

### 2. **Version Management**
- Semantic versioning alignment
- Dependency compatibility validation
- Automated version bump coordination

### 3. **Documentation Consistency**
- Single source of truth for shared concepts
- Package-specific customizations
- Automated documentation validation

### 4. **Testing Integration**
- Cross-package test validation
- Integration test automation
- Performance regression detection

## Monitoring and Metrics

### Sync Quality Metrics:
- Package version alignment percentage
- Documentation consistency score
- Integration test success rate
- Synchronization completion time

### Automated Reporting:
- Weekly sync status reports
- Dependency drift detection
- Documentation divergence alerts
- Integration health monitoring

## Advanced Swarm Synchronization Features

### Multi-Agent Coordination Architecture
```bash
# Initialize comprehensive synchronization swarm
mcp__claude-flow__swarm_init { topology: "hierarchical", maxAgents: 10 }
mcp__claude-flow__agent_spawn { type: "coordinator", name: "Master Sync Coordinator" }
mcp__claude-flow__agent_spawn { type: "analyst", name: "Dependency Analyzer" }
mcp__claude-flow__agent_spawn { type: "coder", name: "Integration Developer" }
mcp__claude-flow__agent_spawn { type: "tester", name: "Validation Engineer" }
mcp__claude-flow__agent_spawn { type: "reviewer", name: "Quality Assurance" }
mcp__claude-flow__agent_spawn { type: "monitor", name: "Sync Monitor" }

# Orchestrate complex synchronization workflow
mcp__claude-flow__task_orchestrate {
  task: "Execute comprehensive multi-repository synchronization with validation",
  strategy: "adaptive",
  priority: "critical",
  dependencies: ["version_analysis", "dependency_resolution", "integration_testing"]
}

# Load balance synchronization tasks across agents
mcp__claude-flow__load_balance {
  swarmId: "sync-coordination-swarm",
  tasks: [
    "package_json_sync",
    "documentation_alignment", 
    "version_compatibility_check",
    "integration_test_execution"
  ]
}
```

### Intelligent Conflict Resolution
```javascript
// Advanced conflict detection and resolution
const syncConflictResolver = async (conflicts) => {
  // Initialize conflict resolution swarm
  await mcp__claude_flow__swarm_init({ topology: "mesh", maxAgents: 6 });
  
  // Spawn specialized conflict resolution agents
  await mcp__claude_flow__agent_spawn({ type: "analyst", name: "Conflict Analyzer" });
  await mcp__claude_flow__agent_spawn({ type: "coder", name: "Resolution Developer" });
  await mcp__claude_flow__agent_spawn({ type: "reviewer", name: "Solution Validator" });
  
  // Store conflict context in swarm memory
  await mcp__claude_flow__memory_usage({
    action: "store",
    key: "sync/conflicts/current",
    value: {
      conflicts,
      resolution_strategy: "automated_with_validation",
      priority_order: conflicts.sort((a, b) => b.impact - a.impact)
    }
  });
  
  // Coordinate conflict resolution workflow
  return await mcp__claude_flow__task_orchestrate({
    task: "Resolve synchronization conflicts with multi-agent validation",
    strategy: "sequential",
    priority: "high"
  });
};
```

### Comprehensive Synchronization Metrics
```bash
# Store detailed synchronization metrics
mcp__claude-flow__memory_usage {
  action: "store",
  key: "sync/metrics/session",
  value: {
    packages_synchronized: ["claude-code-flow", "ruv-swarm"],
    version_alignment_score: 98.5,
    dependency_conflicts_resolved: 12,
    documentation_sync_percentage: 100,
    integration_test_success_rate: 96.8,
    total_sync_time: "23.4 minutes",
    agent_efficiency_scores: {
      "Master Sync Coordinator": 9.2,
      "Dependency Analyzer": 8.7,
      "Integration Developer": 9.0,
      "Validation Engineer": 8.9
    }
  }
}
```

## Error Handling and Recovery

### Swarm-Coordinated Error Recovery
```bash
# Initialize error recovery swarm
mcp__claude-flow__swarm_init { topology: "star", maxAgents: 5 }
mcp__claude-flow__agent_spawn { type: "monitor", name: "Error Monitor" }
mcp__claude-flow__agent_spawn { type: "analyst", name: "Failure Analyzer" }
mcp__claude-flow__agent_spawn { type: "coder", name: "Recovery Developer" }

# Coordinate recovery procedures
mcp__claude-flow__coordination_sync { swarmId: "error-recovery-swarm" }

# Store recovery state
mcp__claude-flow__memory_usage {
  action: "store",
  key: "sync/recovery/state",
  value: {
    error_type: "version_conflict",
    recovery_strategy: "incremental_rollback",
    agent_assignments: {
      "conflict_resolution": "Recovery Developer",
      "validation": "Failure Analyzer",
      "monitoring": "Error Monitor"
    }
  }
}
```

### Automatic handling of:
- Version conflict resolution with swarm consensus
- Merge conflict detection and multi-agent resolution
- Test failure recovery with adaptive strategies
- Documentation sync conflicts with intelligent merging

### Recovery procedures:
- Swarm-coordinated automated rollback on critical failures
- Multi-agent incremental sync retry mechanisms
- Intelligent intervention points for complex conflicts
- Persistent state preservation across sync operations with memory coordination


---

## Source: workflow-automation.md

# Workflow Automation - GitHub Actions Integration

## Overview
Integrate AI swarms with GitHub Actions to create intelligent, self-organizing CI/CD pipelines that adapt to your codebase through advanced multi-agent coordination and automation.

## Core Features

### 1. Swarm-Powered Actions
```yaml
# .github/workflows/swarm-ci.yml
name: Intelligent CI with Swarms
on: [push, pull_request]

jobs:
  swarm-analysis:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Initialize Swarm
        uses: ruvnet/swarm-action@v1
        with:
          topology: mesh
          max-agents: 6
          
      - name: Analyze Changes
        run: |
          npx ruv-swarm actions analyze \
            --commit ${{ github.sha }} \
            --suggest-tests \
            --optimize-pipeline
```

### 2. Dynamic Workflow Generation
```bash
# Generate workflows based on code analysis
npx ruv-swarm actions generate-workflow \
  --analyze-codebase \
  --detect-languages \
  --create-optimal-pipeline
```

### 3. Intelligent Test Selection
```yaml
# Smart test runner
- name: Swarm Test Selection
  run: |
    npx ruv-swarm actions smart-test \
      --changed-files ${{ steps.files.outputs.all }} \
      --impact-analysis \
      --parallel-safe
```

## Workflow Templates

### Multi-Language Detection
```yaml
# .github/workflows/polyglot-swarm.yml
name: Polyglot Project Handler
on: push

jobs:
  detect-and-build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Detect Languages
        id: detect
        run: |
          npx ruv-swarm actions detect-stack \
            --output json > stack.json
            
      - name: Dynamic Build Matrix
        run: |
          npx ruv-swarm actions create-matrix \
            --from stack.json \
            --parallel-builds
```

### Adaptive Security Scanning
```yaml
# .github/workflows/security-swarm.yml
name: Intelligent Security Scan
on:
  schedule:
    - cron: '0 0 * * *'
  workflow_dispatch:

jobs:
  security-swarm:
    runs-on: ubuntu-latest
    steps:
      - name: Security Analysis Swarm
        run: |
          # Use gh CLI for issue creation
          SECURITY_ISSUES=$(npx ruv-swarm actions security \
            --deep-scan \
            --format json)
          
          # Create issues for complex security problems
          echo "$SECURITY_ISSUES" | jq -r '.issues[]? | @base64' | while read -r issue; do
            _jq() {
              echo ${issue} | base64 --decode | jq -r ${1}
            }
            gh issue create \
              --title "$(_jq '.title')" \
              --body "$(_jq '.body')" \
              --label "security,critical"
          done
```

## Action Commands

### Pipeline Optimization
```bash
# Optimize existing workflows
npx ruv-swarm actions optimize \
  --workflow ".github/workflows/ci.yml" \
  --suggest-parallelization \
  --reduce-redundancy \
  --estimate-savings
```

### Failure Analysis
```bash
# Analyze failed runs using gh CLI
gh run view ${{ github.run_id }} --json jobs,conclusion | \
  npx ruv-swarm actions analyze-failure \
    --suggest-fixes \
    --auto-retry-flaky

# Create issue for persistent failures
if [ $? -ne 0 ]; then
  gh issue create \
    --title "CI Failure: Run ${{ github.run_id }}" \
    --body "Automated analysis detected persistent failures" \
    --label "ci-failure"
fi
```

### Resource Management
```bash
# Optimize resource usage
npx ruv-swarm actions resources \
  --analyze-usage \
  --suggest-runners \
  --cost-optimize
```

## Advanced Workflows

### 1. Self-Healing CI/CD
```yaml
# Auto-fix common CI failures
name: Self-Healing Pipeline
on: workflow_run

jobs:
  heal-pipeline:
    if: ${{ github.event.workflow_run.conclusion == 'failure' }}
    runs-on: ubuntu-latest
    steps:
      - name: Diagnose and Fix
        run: |
          npx ruv-swarm actions self-heal \
            --run-id ${{ github.event.workflow_run.id }} \
            --auto-fix-common \
            --create-pr-complex
```

### 2. Progressive Deployment
```yaml
# Intelligent deployment strategy
name: Smart Deployment
on:
  push:
    branches: [main]

jobs:
  progressive-deploy:
    runs-on: ubuntu-latest
    steps:
      - name: Analyze Risk
        id: risk
        run: |
          npx ruv-swarm actions deploy-risk \
            --changes ${{ github.sha }} \
            --history 30d
            
      - name: Choose Strategy
        run: |
          npx ruv-swarm actions deploy-strategy \
            --risk ${{ steps.risk.outputs.level }} \
            --auto-execute
```

### 3. Performance Regression Detection
```yaml
# Automatic performance testing
name: Performance Guard
on: pull_request

jobs:
  perf-swarm:
    runs-on: ubuntu-latest
    steps:
      - name: Performance Analysis
        run: |
          npx ruv-swarm actions perf-test \
            --baseline main \
            --threshold 10% \
            --auto-profile-regression
```

## Custom Actions

### Swarm Action Development
```javascript
// action.yml
name: 'Swarm Custom Action'
description: 'Custom swarm-powered action'
inputs:
  task:
    description: 'Task for swarm'
    required: true
runs:
  using: 'node16'
  main: 'dist/index.js'

// index.js
const { SwarmAction } = require('ruv-swarm');

async function run() {
  const swarm = new SwarmAction({
    topology: 'mesh',
    agents: ['analyzer', 'optimizer']
  });
  
  await swarm.execute(core.getInput('task'));
}
```

## Matrix Strategies

### Dynamic Test Matrix
```yaml
# Generate test matrix from code analysis
jobs:
  generate-matrix:
    outputs:
      matrix: ${{ steps.set-matrix.outputs.matrix }}
    steps:
      - id: set-matrix
        run: |
          MATRIX=$(npx ruv-swarm actions test-matrix \
            --detect-frameworks \
            --optimize-coverage)
          echo "matrix=${MATRIX}" >> $GITHUB_OUTPUT
  
  test:
    needs: generate-matrix
    strategy:
      matrix: ${{fromJson(needs.generate-matrix.outputs.matrix)}}
```

### Intelligent Parallelization
```bash
# Determine optimal parallelization
npx ruv-swarm actions parallel-strategy \
  --analyze-dependencies \
  --time-estimates \
  --cost-aware
```

## Monitoring & Insights

### Workflow Analytics
```bash
# Analyze workflow performance
npx ruv-swarm actions analytics \
  --workflow "ci.yml" \
  --period 30d \
  --identify-bottlenecks \
  --suggest-improvements
```

### Cost Optimization
```bash
# Optimize GitHub Actions costs
npx ruv-swarm actions cost-optimize \
  --analyze-usage \
  --suggest-caching \
  --recommend-self-hosted
```

### Failure Patterns
```bash
# Identify failure patterns
npx ruv-swarm actions failure-patterns \
  --period 90d \
  --classify-failures \
  --suggest-preventions
```

## Integration Examples

### 1. PR Validation Swarm
```yaml
name: PR Validation Swarm
on: pull_request

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - name: Multi-Agent Validation
        run: |
          # Get PR details using gh CLI
          PR_DATA=$(gh pr view ${{ github.event.pull_request.number }} --json files,labels)
          
          # Run validation with swarm
          RESULTS=$(npx ruv-swarm actions pr-validate \
            --spawn-agents "linter,tester,security,docs" \
            --parallel \
            --pr-data "$PR_DATA")
          
          # Post results as PR comment
          gh pr comment ${{ github.event.pull_request.number }} \
            --body "$RESULTS"
```

### 2. Release Automation
```yaml
name: Intelligent Release
on:
  push:
    tags: ['v*']

jobs:
  release:
    runs-on: ubuntu-latest
    steps:
      - name: Release Swarm
        run: |
          npx ruv-swarm actions release \
            --analyze-changes \
            --generate-notes \
            --create-artifacts \
            --publish-smart
```

### 3. Documentation Updates
```yaml
name: Auto Documentation
on:
  push:
    paths: ['src/**']

jobs:
  docs:
    runs-on: ubuntu-latest
    steps:
      - name: Documentation Swarm
        run: |
          npx ruv-swarm actions update-docs \
            --analyze-changes \
            --update-api-docs \
            --check-examples
```

## Best Practices

### 1. Workflow Organization
- Use reusable workflows for swarm operations
- Implement proper caching strategies
- Set appropriate timeouts
- Use workflow dependencies wisely

### 2. Security
- Store swarm configs in secrets
- Use OIDC for authentication
- Implement least-privilege principles
- Audit swarm operations

### 3. Performance
- Cache swarm dependencies
- Use appropriate runner sizes
- Implement early termination
- Optimize parallel execution

## Advanced Features

### Predictive Failures
```bash
# Predict potential failures
npx ruv-swarm actions predict \
  --analyze-history \
  --identify-risks \
  --suggest-preventive
```

### Workflow Recommendations
```bash
# Get workflow recommendations
npx ruv-swarm actions recommend \
  --analyze-repo \
  --suggest-workflows \
  --industry-best-practices
```

### Automated Optimization
```bash
# Continuously optimize workflows
npx ruv-swarm actions auto-optimize \
  --monitor-performance \
  --apply-improvements \
  --track-savings
```

## Debugging & Troubleshooting

### Debug Mode
```yaml
- name: Debug Swarm
  run: |
    npx ruv-swarm actions debug \
      --verbose \
      --trace-agents \
      --export-logs
```

### Performance Profiling
```bash
# Profile workflow performance
npx ruv-swarm actions profile \
  --workflow "ci.yml" \
  --identify-slow-steps \
  --suggest-optimizations
```

## Advanced Swarm Workflow Automation

### Multi-Agent Pipeline Orchestration
```bash
# Initialize comprehensive workflow automation swarm
mcp__claude-flow__swarm_init { topology: "mesh", maxAgents: 12 }
mcp__claude-flow__agent_spawn { type: "coordinator", name: "Workflow Coordinator" }
mcp__claude-flow__agent_spawn { type: "architect", name: "Pipeline Architect" }
mcp__claude-flow__agent_spawn { type: "coder", name: "Workflow Developer" }
mcp__claude-flow__agent_spawn { type: "tester", name: "CI/CD Tester" }
mcp__claude-flow__agent_spawn { type: "optimizer", name: "Performance Optimizer" }
mcp__claude-flow__agent_spawn { type: "monitor", name: "Automation Monitor" }
mcp__claude-flow__agent_spawn { type: "analyst", name: "Workflow Analyzer" }

# Create intelligent workflow automation rules
mcp__claude-flow__automation_setup {
  rules: [
    {
      trigger: "pull_request",
      conditions: ["files_changed > 10", "complexity_high"],
      actions: ["spawn_review_swarm", "parallel_testing", "security_scan"]
    },
    {
      trigger: "push_to_main",
      conditions: ["all_tests_pass", "security_cleared"],
      actions: ["deploy_staging", "performance_test", "notify_stakeholders"]
    }
  ]
}

# Orchestrate adaptive workflow management
mcp__claude-flow__task_orchestrate {
  task: "Manage intelligent CI/CD pipeline with continuous optimization",
  strategy: "adaptive",
  priority: "high",
  dependencies: ["code_analysis", "test_optimization", "deployment_strategy"]
}
```

### Intelligent Performance Monitoring
```bash
# Generate comprehensive workflow performance reports
mcp__claude-flow__performance_report {
  format: "detailed",
  timeframe: "30d"
}

# Analyze workflow bottlenecks with swarm intelligence
mcp__claude-flow__bottleneck_analyze {
  component: "github_actions_workflow",
  metrics: ["build_time", "test_duration", "deployment_latency", "resource_utilization"]
}

# Store performance insights in swarm memory
mcp__claude-flow__memory_usage {
  action: "store",
  key: "workflow/performance/analysis",
  value: {
    bottlenecks_identified: ["slow_test_suite", "inefficient_caching"],
    optimization_opportunities: ["parallel_matrix", "smart_caching"],
    performance_trends: "improving",
    cost_optimization_potential: "23%"
  }
}
```

### Dynamic Workflow Generation
```javascript
// Swarm-powered workflow creation
const createIntelligentWorkflow = async (repoContext) => {
  // Initialize workflow generation swarm
  await mcp__claude_flow__swarm_init({ topology: "hierarchical", maxAgents: 8 });
  
  // Spawn specialized workflow agents
  await mcp__claude_flow__agent_spawn({ type: "architect", name: "Workflow Architect" });
  await mcp__claude_flow__agent_spawn({ type: "coder", name: "YAML Generator" });
  await mcp__claude_flow__agent_spawn({ type: "optimizer", name: "Performance Optimizer" });
  await mcp__claude_flow__agent_spawn({ type: "tester", name: "Workflow Validator" });
  
  // Create adaptive workflow based on repository analysis
  const workflow = await mcp__claude_flow__workflow_create({
    name: "Intelligent CI/CD Pipeline",
    steps: [
      {
        name: "Smart Code Analysis",
        agents: ["analyzer", "security_scanner"],
        parallel: true
      },
      {
        name: "Adaptive Testing",
        agents: ["unit_tester", "integration_tester", "e2e_tester"],
        strategy: "based_on_changes"
      },
      {
        name: "Intelligent Deployment",
        agents: ["deployment_manager", "rollback_coordinator"],
        conditions: ["all_tests_pass", "security_approved"]
      }
    ],
    triggers: [
      "pull_request",
      "push_to_main",
      "scheduled_optimization"
    ]
  });
  
  // Store workflow configuration in memory
  await mcp__claude_flow__memory_usage({
    action: "store",
    key: `workflow/${repoContext.name}/config`,
    value: {
      workflow,
      generated_at: Date.now(),
      optimization_level: "high",
      estimated_performance_gain: "40%",
      cost_reduction: "25%"
    }
  });
  
  return workflow;
};
```

### Continuous Learning and Optimization
```bash
# Implement continuous workflow learning
mcp__claude-flow__memory_usage {
  action: "store",
  key: "workflow/learning/patterns",
  value: {
    successful_patterns: [
      "parallel_test_execution",
      "smart_dependency_caching",
      "conditional_deployment_stages"
    ],
    failure_patterns: [
      "sequential_heavy_operations",
      "inefficient_docker_builds",
      "missing_error_recovery"
    ],
    optimization_history: {
      "build_time_reduction": "45%",
      "resource_efficiency": "60%",
      "failure_rate_improvement": "78%"
    }
  }
}

# Generate workflow optimization recommendations
mcp__claude-flow__task_orchestrate {
  task: "Analyze workflow performance and generate optimization recommendations",
  strategy: "parallel",
  priority: "medium"
}
```

See also: [swarm-pr.md](./swarm-pr.md), [swarm-issue.md](./swarm-issue.md), [sync-coordinator.md](./sync-coordinator.md)
