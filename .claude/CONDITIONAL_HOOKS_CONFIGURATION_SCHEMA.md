# Conditional Hooks Configuration Schema

## Environment Variables Configuration

### Full Variable Reference

#### Core Control Variables

##### `CLAUDE_FLOW_CONDITIONAL_HOOKS`
- **Type:** `boolean` (string: "true"/"false")
- **Default:** `"true"`
- **Purpose:** Master switch for conditional hook execution
- **Values:**
  - `"true"` - Enable intelligent hook execution
  - `"false"` - Disable all conditional logic, run hooks always
- **Usage:** `export CLAUDE_FLOW_CONDITIONAL_HOOKS=true`
- **Impact:** Controls entire conditional hook system
- **Compatibility:** Works with all hook types

##### `CLAUDE_FLOW_DISABLE_SIMPLE_HOOKS`
- **Type:** `boolean` (string: "true"/"false")
- **Default:** `"false"`
- **Purpose:** Override complexity detection for simple operations
- **Values:**
  - `"true"` - Skip hooks entirely for simple operations
  - `"false"` - Run lightweight tracking for simple operations
- **Usage:** `export CLAUDE_FLOW_DISABLE_SIMPLE_HOOKS=true`
- **Impact:** Affects simple bash commands and non-critical files
- **Performance:** Additional 10-20% reduction if enabled

#### Complexity Configuration

##### `CLAUDE_FLOW_COMPLEXITY_THRESHOLD`
- **Type:** `number` (string representation)
- **Default:** `"3"`
- **Purpose:** Operation count threshold for complexity classification
- **Values:** Integer >= 1
  - `"1"` - Very strict, most ops treated as complex
  - `"3"` - Balanced (default)
  - `"5"` - Lenient, more ops treated as simple
  - `"10"` - Very lenient
- **Usage:** `export CLAUDE_FLOW_COMPLEXITY_THRESHOLD=3`
- **Impact:** Affects classification of operation sequences
- **Recommendation:** Keep at default (3)

#### Critical File Patterns

##### `CLAUDE_FLOW_CRITICAL_FILE_PATTERNS`
- **Type:** `string` (comma-separated patterns)
- **Default:** `"src/,tests/,config/,.claude/,package.json,CLAUDE.md,tsconfig.json"`
- **Purpose:** Specify which files always receive full hook processing
- **Format:** Comma-separated patterns (bash glob style)
- **Patterns:**
  - Directory patterns: `src/`, `config/`, `tests/`
  - File patterns: `*.json`, `CLAUDE.md`
  - Regex patterns: `src/.*\.ts$` (if regex enabled)
- **Usage:**
  ```bash
  export CLAUDE_FLOW_CRITICAL_FILE_PATTERNS="src/,tests/,config/,.claude/"
  ```
- **Impact:** Determines which files get enhanced processing
- **Examples:**
  ```
  Default:  "src/,tests/,config/,.claude/,package.json,CLAUDE.md,tsconfig.json"
  Minimal:  "src/,package.json,.claude/"
  Strict:   "src/,tests/,config/,.claude/,package.json,CLAUDE.md,tsconfig.json,docs/,scripts/"
  ```

#### Performance Tracking

##### `CLAUDE_FLOW_HOOK_PERFORMANCE_TRACKING`
- **Type:** `boolean` (string: "true"/"false")
- **Default:** `"true"`
- **Purpose:** Enable/disable performance metrics collection
- **Values:**
  - `"true"` - Collect and store performance data
  - `"false"` - Skip performance data collection
- **Usage:** `export CLAUDE_FLOW_HOOK_PERFORMANCE_TRACKING=true`
- **Output:** `.claude/metrics/hook-performance.json`
- **Impact:** Minimal (metrics collection has <1% overhead)
- **Recommendation:** Keep enabled for analysis

##### `CLAUDE_FLOW_OVERHEAD_REDUCTION_TARGET`
- **Type:** `number` (string representation, percentage)
- **Default:** `"60"`
- **Purpose:** Target overhead reduction percentage for optimization goals
- **Values:** 0-100 (percentage)
  - `"0"` - No optimization target (informational only)
  - `"50"` - Target 50% reduction
  - `"60"` - Target 60% reduction (default, achievable)
  - `"80"` - Target 80% reduction (aggressive)
- **Usage:** `export CLAUDE_FLOW_OVERHEAD_REDUCTION_TARGET=60`
- **Impact:** Used for performance analysis and reporting
- **Note:** This is a goal metric, not a hard limit

### Configuration in settings.json

#### Environment Variables Section

```json
{
  "env": {
    "CLAUDE_FLOW_AUTO_COMMIT": "false",
    "CLAUDE_FLOW_AUTO_PUSH": "false",
    "CLAUDE_FLOW_HOOKS_ENABLED": "true",
    "CLAUDE_FLOW_TELEMETRY_ENABLED": "true",
    "CLAUDE_FLOW_REMOTE_EXECUTION": "true",
    "CLAUDE_FLOW_CHECKPOINTS_ENABLED": "true",
    "CLAUDE_FLOW_CONDITIONAL_HOOKS": "true",
    "CLAUDE_FLOW_DISABLE_SIMPLE_HOOKS": "false",
    "CLAUDE_FLOW_COMPLEXITY_THRESHOLD": "3",
    "CLAUDE_FLOW_CRITICAL_FILE_PATTERNS": "src/,tests/,config/,.claude/,package.json,CLAUDE.md,tsconfig.json",
    "CLAUDE_FLOW_HOOK_PERFORMANCE_TRACKING": "true",
    "CLAUDE_FLOW_OVERHEAD_REDUCTION_TARGET": "60"
  }
}
```

## Hook Configuration Structure

### PreToolUse Hook - Bash (Conditional)

**Matcher:** `"Bash"`
**Condition:** Command complexity detection
**Decision Logic:**
```
IF command_length < 50
   AND matches(command, SIMPLE_PATTERNS)
   AND CLAUDE_FLOW_DISABLE_SIMPLE_HOOKS != "true"
THEN
  Skip hook execution (minimal overhead)
ELSE
  Execute: npx claude-flow@alpha hooks pre-command \
    --command "$CMD" \
    --validate-safety true \
    --prepare-resources true \
    --complexity-level moderate
END
```

**Simple Patterns:**
- `pwd` - Current directory
- `which` - Find command
- `ls` - List files
- `git status` - Show status
- `git log` - Show history

**Status Message:** `"Conditional hook: Validating bash command complexity"`

### PreToolUse Hook - Write/Edit/MultiEdit (Conditional)

**Matcher:** `"Write|Edit|MultiEdit"`
**Condition:** File pattern matching
**Decision Logic:**
```
IF file_path matches CRITICAL_FILE_PATTERNS
   AND CLAUDE_FLOW_CONDITIONAL_HOOKS == "true"
THEN
  Execute: npx claude-flow@alpha hooks pre-edit \
    --file "$FILE" \
    --auto-assign-agents true \
    --load-context true \
    --file-priority critical
ELSE
  Skip hook execution
END
```

**Critical File Patterns:** (Configurable)
```
src/.*\.(ts|js)$
tests/.*\.test\.(ts|js)$
config/.*
\.claude/.*
package\.json$
CLAUDE\.md$
tsconfig\.json$
```

**Status Message:** `"Conditional hook: Checking file criticality"`

### PostToolUse Hook - Bash (Conditional)

**Matcher:** `"Bash"`
**Condition:** Command complexity detection
**Decision Logic:**
```
IF command_length < 50
   AND matches(command, SIMPLE_PATTERNS)
THEN
  IF CLAUDE_FLOW_HOOK_PERFORMANCE_TRACKING == "true"
    Log: "Lightweight tracking only for simple operation"
ELSE
  Execute: npx claude-flow@alpha hooks post-command \
    --command "$CMD" \
    --track-metrics true \
    --store-results true \
    --complexity-level moderate
END
```

**Status Message:** `"Conditional hook: Post-command metrics tracking"`

### PostToolUse Hook - Write/Edit/MultiEdit (Conditional)

**Matcher:** `"Write|Edit|MultiEdit"`
**Condition:** File pattern and size-based logic
**Decision Logic:**
```
FILE_SIZE = line_count(file)
UPDATE_MEMORY = (FILE_SIZE > 100) ? true : false

IF file_path matches CRITICAL_FILE_PATTERNS
   AND CLAUDE_FLOW_CONDITIONAL_HOOKS == "true"
THEN
  Execute: npx claude-flow@alpha hooks post-edit \
    --file "$FILE" \
    --format true \
    --update-memory $UPDATE_MEMORY \
    --file-priority critical
ELSE IF FILE_SIZE > 500
  Log: "Non-critical large file: Minimal tracking only"
ELSE
  Skip hook execution
END
```

**Status Message:** `"Conditional hook: Post-edit processing"`

### SessionStart Hook (Multi-Agent Detection)

**Matcher:** `"auto"`
**Trigger:** Session initialization
**Condition:**
```
IF CLAUDE_FLOW_CONDITIONAL_HOOKS == "true"
THEN
  Execute: npx claude-flow@alpha hooks session-restore \
    --detect-agents true \
    --enable-coordination true \
    --memory-namespace swarm/session
ELSE
  Skip execution
END
```

**Status Message:** `"Multi-agent session detection and initialization"`
**Memory Namespace:** `swarm/session`
**Purpose:** Initialize multi-agent coordination

### SessionEnd Hook (Performance Collection)

**Matcher:** `"auto"`
**Trigger:** Session termination
**Condition:**
```
IF CLAUDE_FLOW_HOOK_PERFORMANCE_TRACKING == "true"
THEN
  Execute: npx claude-flow@alpha hooks session-end \
    --generate-summary true \
    --persist-state true \
    --export-metrics true \
    --store-performance true \
    --memory-namespace swarm/metrics
ELSE
  Skip execution
END
```

**Status Message:** `"Collecting session metrics and performance data"`
**Memory Namespace:** `swarm/metrics`
**Output:** `.claude/metrics/hook-performance.json`

## Hook Decision Tree

### Operation Classification

```
Operation Request
  ├─ Tool Type: Bash
  │   ├─ Command Length < 50 chars?
  │   │   ├─ Yes → Matches Simple Pattern?
  │   │   │   ├─ Yes → SKIP HOOKS (60% reduction)
  │   │   │   └─ No → RUN FULL HOOKS
  │   │   └─ No → RUN FULL HOOKS
  │
  ├─ Tool Type: Write/Edit/MultiEdit
  │   ├─ File Path matches Critical Pattern?
  │   │   ├─ Yes → RUN FULL HOOKS
  │   │   └─ No → File Size > 500 lines?
  │   │       ├─ Yes → MINIMAL TRACKING
  │   │       └─ No → SKIP HOOKS (60% reduction)
```

## Configuration Precedence

1. **Runtime Environment Variables** (Highest Priority)
   ```bash
   export CLAUDE_FLOW_CONDITIONAL_HOOKS=true
   ```

2. **settings.json Environment Section**
   ```json
   "env": { "CLAUDE_FLOW_CONDITIONAL_HOOKS": "true" }
   ```

3. **Default Values** (Lowest Priority)
   - Built-in defaults if not specified

## Validation Rules

### Critical File Patterns
- Must be comma-separated
- Can include wildcards (`*`) and path separators (`/`)
- Optional regex support (pattern dependent)
- Empty string disables critical file detection

### Complexity Threshold
- Must be positive integer >= 1
- Values > 10 may have unintended effects
- Recommended: 3 (default)

### Overhead Target
- Must be 0-100 (percentage)
- For monitoring/analysis only
- Doesn't affect actual execution

### Conditional Hooks Flag
- Must be "true" or "false" (case-sensitive in shell)
- JSON uses lowercase: `true`, `false`
- Bash uses string: `"true"`, `"false"`

## Performance Impact by Configuration

| Configuration | Simple Ops | Complex Ops | Overall |
|---------------|-----------|-----------|---------|
| All Default | 60% ↓ | 0% | ~50% |
| Disable Simple Hooks | 80% ↓ | 0% | ~60% |
| Strict Critical Files | 40% ↓ | 0% | ~35% |
| Custom Patterns | Variable | 0% | Variable |

## Monitoring and Debugging

### Check Configuration
```bash
# Verify settings.json
grep CLAUDE_FLOW .claude/settings.json

# Check environment variables
env | grep CLAUDE_FLOW
```

### View Performance Metrics
```bash
# After session completes
cat .claude/metrics/hook-performance.json

# Pretty print
python -m json.tool .claude/metrics/hook-performance.json
```

### Enable Verbose Logging
```bash
# In shell before running claude
export DEBUG=claude-flow:*
```

## Common Configurations

### Development (Default)
```json
"CLAUDE_FLOW_CONDITIONAL_HOOKS": "true",
"CLAUDE_FLOW_DISABLE_SIMPLE_HOOKS": "false",
"CLAUDE_FLOW_CRITICAL_FILE_PATTERNS": "src/,tests/,config/,.claude/,package.json,CLAUDE.md,tsconfig.json",
"CLAUDE_FLOW_HOOK_PERFORMANCE_TRACKING": "true"
```

### High Performance
```json
"CLAUDE_FLOW_CONDITIONAL_HOOKS": "true",
"CLAUDE_FLOW_DISABLE_SIMPLE_HOOKS": "true",
"CLAUDE_FLOW_CRITICAL_FILE_PATTERNS": "src/,package.json",
"CLAUDE_FLOW_HOOK_PERFORMANCE_TRACKING": "false"
```

### Maximum Safety (Full Hooks)
```json
"CLAUDE_FLOW_CONDITIONAL_HOOKS": "false",
"CLAUDE_FLOW_DISABLE_SIMPLE_HOOKS": "false",
"CLAUDE_FLOW_HOOK_PERFORMANCE_TRACKING": "true"
```

### Strict Compliance
```json
"CLAUDE_FLOW_CONDITIONAL_HOOKS": "true",
"CLAUDE_FLOW_DISABLE_SIMPLE_HOOKS": "false",
"CLAUDE_FLOW_CRITICAL_FILE_PATTERNS": "src/,tests/,config/,.claude/,docs/,scripts/,package.json,CLAUDE.md,tsconfig.json,.env",
"CLAUDE_FLOW_HOOK_PERFORMANCE_TRACKING": "true"
```

## Troubleshooting Configuration Issues

### Issue: Hooks not running for critical files
**Solution:** Check file path matches pattern exactly
```bash
# Verify pattern
grep CRITICAL_FILE .claude/settings.json

# Test pattern matching
[[ "src/server.ts" =~ src/ ]] && echo "matches" || echo "no match"
```

### Issue: Performance tracking disabled
**Solution:** Enable in settings.json
```json
"CLAUDE_FLOW_HOOK_PERFORMANCE_TRACKING": "true"
```

### Issue: Simple commands still run full hooks
**Solution:** Disable conditional hooks temporarily
```bash
export CLAUDE_FLOW_CONDITIONAL_HOOKS=false
```

---

**Version:** 1.0
**Updated:** 2026-01-06
**Format:** JSON + Bash Configuration
