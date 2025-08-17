# Generic Components for Cross-Repository Propagation

## 🚀 MANDATORY: Generic Components Only

**CRITICAL PRINCIPLE**: Only propagate generic, repository-agnostic components. 
Never propagate repository-specific modules or domain-specific implementations.

## Components Safe for Propagation

### 1. Background Test Runner (GENERIC)
**Location**: `.agent-os/commands/run_tests_background.py`
**Purpose**: Run tests in parallel background processes before reporting success
**Repository-agnostic**: Yes ✅

**Features**:
- Parallel test execution with ThreadPoolExecutor
- Test result caching for efficiency
- Background thread execution
- Quick validation mode for changed files
- Generic test discovery (works with pytest, unittest, etc.)

**Usage**:
```bash
# Run all tests in background
./slash_commands.py /run-tests-background

# Quick validation
./slash_commands.py /run-tests-background --quick

# Wait for completion
./slash_commands.py /run-tests-background --wait
```

### 2. File Type Detector Pattern (GENERIC PATTERN ONLY)
**Concept**: Pattern for detecting and classifying file types
**Repository-agnostic**: Pattern yes ✅, Implementation no ❌

**Generic Pattern to Share**:
```python
class GenericFileTypeDetector:
    """Base pattern for file type detection"""
    def detect_by_extension(self, file_path: Path) -> str
    def detect_by_content(self, file_path: Path) -> str
    def classify_directory(self, dir_path: Path) -> Dict
```

**NOT to Share**: OrcaFlex-specific detection logic

### 3. Parallel Batch Processing Pattern (GENERIC)
**Concept**: Pattern for processing multiple items in parallel
**Repository-agnostic**: Pattern yes ✅, Domain logic no ❌

**Generic Pattern**:
- ThreadPoolExecutor for concurrent processing
- Configurable worker limits
- Thread-safe logging
- Progress tracking
- Error resilience

## Components NOT for Propagation (Repository-Specific)

### ❌ OrcaFlex-Specific (DigitalModel Only)
- `src/modules/orcaflex/` - Entire module
- OrcaFlex file type detection logic
- Mooring tension iteration
- Length[2] modifications
- OrcaFlex API integration

### ❌ Repository-Specific Modules
Each repository maintains its own:
- Domain-specific modules (e.g., AQWA, ANSYS, CAD)
- Industry-specific implementations
- Licensed software integrations
- Proprietary algorithms

## Proper Propagation Strategy

### Level 1: Agent-OS Commands (Generic)
```bash
# Propagate generic commands only
/propagate-commands \
  --files ".agent-os/commands/run_tests_background.py" \
  --targets all
```

### Level 2: Design Patterns (Documentation)
```bash
# Propagate patterns as documentation
/propagate-commands \
  --files ".agent-os/patterns/parallel_processing.md" \
  --files ".agent-os/patterns/file_type_detection.md" \
  --targets all
```

### Level 3: Testing Infrastructure (Generic)
```bash
# Propagate generic test utilities
/propagate-commands \
  --files ".agent-os/testing/background_runner.py" \
  --files ".agent-os/testing/parallel_executor.py" \
  --targets all
```

## Repository Module Boundaries

### DigitalModel Repository
**Modules**: OrcaFlex, AQWA, ANSYS, Hydrodynamics
**Do NOT propagate**: Any files under `src/modules/orcaflex/`

### AssetUtilities Repository  
**Modules**: Asset management, Utilities, Common tools
**CAN propagate**: Generic utilities under `.common-commands/`

### FrontierDeepwater Repository
**Modules**: Deepwater-specific, Subsea systems
**Do NOT propagate**: Domain-specific implementations

## Validation Checklist Before Propagation

Before propagating ANY component, verify:

- [ ] **Is it repository-agnostic?** No hardcoded paths, no domain logic
- [ ] **Is it module-independent?** Doesn't require specific modules
- [ ] **Is it license-free?** No dependencies on licensed software
- [ ] **Is it domain-neutral?** Works across all industries/domains
- [ ] **Is it in .agent-os/?** Generic components belong here
- [ ] **Does it respect boundaries?** Doesn't cross module boundaries

## Correct Propagation Examples

### ✅ GOOD: Generic Test Runner
```python
# .agent-os/commands/run_tests_background.py
class BackgroundTestRunner:
    """Generic test runner for any repository"""
    def find_test_files(self)  # Works with any test framework
    def run_tests_parallel(self)  # Generic parallel execution
```

### ❌ BAD: Domain-Specific Runner
```python
# src/modules/orcaflex/batch_runner.py
class OrcaFlexBatchRunner:
    """OrcaFlex-specific - DO NOT PROPAGATE"""
    def process_orcaflex_model()  # Domain-specific
    def apply_length2_modifications()  # OrcaFlex-specific
```

## Memory Note Implementation

As per user memory: "Always run tests before reporting success"

**Generic Implementation** (Safe to propagate):
```python
# .agent-os/hooks/pre_success_hook.py
def before_reporting_success():
    """Run tests in background before claiming success"""
    runner = BackgroundTestRunner()
    thread = runner.run_tests_background()
    # Continue while tests run in parallel
```

**Repository-Specific** (NOT to propagate):
```python
# src/modules/orcaflex/test_runner.py
def test_orcaflex_models():  # OrcaFlex-specific tests
```

## Command Creation for Safe Propagation

```bash
# Create generic propagation command
/create-command propagate-generic-components \
  --description "Propagate only generic, repository-agnostic components" \
  --validate-generic \
  --exclude-modules \
  --targets all
```

## Final Notes

1. **Generic goes in .agent-os/** - All propagatable content here
2. **Modules stay in src/modules/** - Never propagate these
3. **Patterns over implementations** - Share concepts, not code
4. **Test everything generically** - No domain assumptions
5. **Respect repository boundaries** - Each repo is sovereign

---
*Last Updated: 2025-01-17*
*Follows repository module boundary principles*