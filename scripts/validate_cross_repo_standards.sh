#!/usr/bin/env bash
# ABOUTME: Cross-repository standards validation script
# ABOUTME: Scans all repositories in workspace-hub and validates against mandatory/recommended standards

set -euo pipefail

# ============================================================================
# Configuration
# ============================================================================

WORKSPACE_ROOT="${WORKSPACE_ROOT:-/d/workspace-hub}"
REPORT_DIR="${REPORT_DIR:-/d/workspace-hub/digitalmodel/reports}"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
REPORT_FILE="${REPORT_DIR}/standards_compliance_${TIMESTAMP}.md"

# Colors for output
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Exit codes
EXIT_SUCCESS=0
EXIT_VALIDATION_FAILED=1

# ============================================================================
# Helper Functions
# ============================================================================

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[PASS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[FAIL]${NC} $1"
}

# ============================================================================
# Repository Discovery
# ============================================================================

find_repositories() {
    log_info "Scanning for repositories in: $WORKSPACE_ROOT"

    local repos=()

    # Find all directories with .git folder, excluding common ignore patterns
    while IFS= read -r -d '' git_dir; do
        repo_dir=$(dirname "$git_dir")

        # Skip if in excluded directories
        if [[ "$repo_dir" == *"/node_modules/"* ]] || \
           [[ "$repo_dir" == *"/.venv/"* ]] || \
           [[ "$repo_dir" == *"/__pycache__/"* ]] || \
           [[ "$repo_dir" == *"/.git/"* ]]; then
            continue
        fi

        repos+=("$repo_dir")
    done < <(find "$WORKSPACE_ROOT" -type d -name ".git" -print0 2>/dev/null)

    log_success "Found ${#repos[@]} repositories"
    printf '%s\n' "${repos[@]}"
}

# ============================================================================
# Standard Validation Functions
# ============================================================================

# STD-ORG-001: Repository Organization Pattern
validate_org_001() {
    local repo_path="$1"
    local violations=()
    local score=0
    local max_score=4

    # Check for src/ directory
    if [[ -d "$repo_path/src" ]]; then
        ((score++))
    else
        violations+=("Missing src/ directory")
    fi

    # Check for tests/ directory
    if [[ -d "$repo_path/tests" ]]; then
        ((score++))
    else
        violations+=("Missing tests/ directory")
    fi

    # Check for module structure in src/
    if [[ -d "$repo_path/src" ]]; then
        local has_module_structure=false

        # Look for <group>/modules/<module>/ pattern
        while IFS= read -r -d '' modules_dir; do
            has_module_structure=true
            break
        done < <(find "$repo_path/src" -type d -path "*/modules/*" -print0 2>/dev/null | head -1)

        if $has_module_structure; then
            ((score++))
        else
            violations+=("No <group>/modules/<module>/ structure found in src/")
        fi
    fi

    # Check for working files in root (excluding allowed configs)
    local root_violations=()
    while IFS= read -r file; do
        local basename=$(basename "$file")

        # Skip allowed files
        if [[ "$basename" =~ ^(README|CHANGELOG|LICENSE|CONTRIBUTING)\.md$ ]] || \
           [[ "$basename" =~ \.(toml|yaml|yml|json|txt|gitignore|env\.example)$ ]] || \
           [[ "$basename" =~ ^(pyproject\.toml|package\.json|setup\.py|Makefile|requirements\.txt)$ ]]; then
            continue
        fi

        # Skip directories
        if [[ -d "$file" ]]; then
            continue
        fi

        root_violations+=("$basename")
    done < <(find "$repo_path" -maxdepth 1 -type f 2>/dev/null)

    if [[ ${#root_violations[@]} -eq 0 ]]; then
        ((score++))
    else
        violations+=("Working files in root: ${root_violations[*]}")
    fi

    # Calculate percentage
    local percentage=$((score * 100 / max_score))

    if [[ $percentage -eq 100 ]]; then
        echo "PASS|100|"
    elif [[ $percentage -ge 75 ]]; then
        echo "WARN|$percentage|${violations[*]}"
    else
        echo "FAIL|$percentage|${violations[*]}"
    fi
}

# STD-BEHAVE-001: Anti-Sycophancy Protocol
validate_behave_001() {
    local repo_path="$1"
    local violations=()
    local found=false

    # Check for anti-sycophancy instructions in CLAUDE.md files
    local claude_files=(
        "$repo_path/CLAUDE.md"
        "$repo_path/.claude/CLAUDE.md"
        "$repo_path/CLAUDE.local.md"
    )

    local patterns=(
        "ask clarifying questions"
        "MUST ask"
        "never assume"
        "wait for confirmation"
        "explicit approval"
        "anti-sycophancy"
        "sycophancy"
    )

    for claude_file in "${claude_files[@]}"; do
        if [[ -f "$claude_file" ]]; then
            local matches=0
            for pattern in "${patterns[@]}"; do
                if grep -qi "$pattern" "$claude_file" 2>/dev/null; then
                    ((matches++))
                fi
            done

            if [[ $matches -ge 2 ]]; then
                found=true
                break
            fi
        fi
    done

    if $found; then
        echo "PASS|100|"
    else
        violations+=("No anti-sycophancy instructions found in CLAUDE.md files")
        echo "FAIL|0|${violations[*]}"
    fi
}

# STD-FILE-001: File Management Rules
validate_file_001() {
    local repo_path="$1"
    local violations=()
    local score=0
    local max_score=3

    # Check 1: No working files in root (checked in ORG-001, reuse logic)
    local root_violations=()
    while IFS= read -r file; do
        local basename=$(basename "$file")

        # Skip allowed files
        if [[ "$basename" =~ ^(README|CHANGELOG|LICENSE|CONTRIBUTING)\.md$ ]] || \
           [[ "$basename" =~ \.(toml|yaml|yml|json|txt|gitignore|env\.example)$ ]] || \
           [[ "$basename" =~ ^(pyproject\.toml|package\.json|setup\.py|Makefile|requirements\.txt)$ ]]; then
            continue
        fi

        # Skip directories
        if [[ -d "$file" ]]; then
            continue
        fi

        root_violations+=("$basename")
    done < <(find "$repo_path" -maxdepth 1 -type f 2>/dev/null)

    if [[ ${#root_violations[@]} -eq 0 ]]; then
        ((score++))
    else
        violations+=("Working files in root: ${root_violations[*]}")
    fi

    # Check 2: User input directory exists if user files present
    if [[ -d "$repo_path/inputs/user_provided" ]] || [[ -d "$repo_path/data/user_inputs" ]]; then
        ((score++))
    else
        # Check if any user_* files exist that should be in these directories
        local user_files_count=$(find "$repo_path" -type f -name "user_*" 2>/dev/null | wc -l)
        if [[ $user_files_count -eq 0 ]]; then
            # No user files, so this check passes
            ((score++))
        else
            violations+=("User files exist but no inputs/user_provided/ or data/user_inputs/ directory")
        fi
    fi

    # Check 3: User files follow naming convention
    local bad_user_files=()
    while IFS= read -r file; do
        local basename=$(basename "$file")
        # Check if follows user_<descriptor>_YYYYMMDD.<ext> pattern
        if [[ ! "$basename" =~ ^user_[a-z0-9_]+_[0-9]{8}\.[a-z0-9]+$ ]]; then
            bad_user_files+=("$basename")
        fi
    done < <(find "$repo_path" -type f -name "user_*" 2>/dev/null)

    if [[ ${#bad_user_files[@]} -eq 0 ]]; then
        ((score++))
    else
        violations+=("User files not following naming convention: ${bad_user_files[*]}")
    fi

    # Calculate percentage
    local percentage=$((score * 100 / max_score))

    if [[ $percentage -eq 100 ]]; then
        echo "PASS|100|"
    elif [[ $percentage -ge 75 ]]; then
        echo "WARN|$percentage|${violations[*]}"
    else
        echo "FAIL|$percentage|${violations[*]}"
    fi
}

# STD-REPORT-001: HTML Reporting Requirements
validate_report_001() {
    local repo_path="$1"
    local violations=()
    local score=0
    local max_score=3

    # Check 1: Reports directory exists
    if [[ -d "$repo_path/reports" ]]; then
        ((score++))
    else
        violations+=("No reports/ directory")
    fi

    # Check 2: Interactive plotting libraries in use
    local has_interactive=false
    local plot_files=$(find "$repo_path/src" -type f \( -name "*.py" -o -name "*.js" -o -name "*.ts" \) 2>/dev/null)

    if [[ -n "$plot_files" ]]; then
        if echo "$plot_files" | xargs grep -l -E "(plotly|bokeh|altair|d3\.js)" 2>/dev/null | head -1 > /dev/null; then
            has_interactive=true
            ((score++))
        else
            violations+=("No interactive plotting libraries found (plotly, bokeh, altair, d3.js)")
        fi
    else
        # No source files, skip this check
        ((score++))
    fi

    # Check 3: No matplotlib static exports (anti-pattern)
    if [[ -n "$plot_files" ]]; then
        if echo "$plot_files" | xargs grep -l "plt\.savefig" 2>/dev/null | head -1 > /dev/null; then
            violations+=("Found matplotlib static exports (plt.savefig) - should use interactive plots")
        else
            ((score++))
        fi
    else
        ((score++))
    fi

    # Calculate percentage
    local percentage=$((score * 100 / max_score))

    if [[ $percentage -eq 100 ]]; then
        echo "PASS|100|"
    elif [[ $percentage -ge 75 ]]; then
        echo "WARN|$percentage|${violations[*]}"
    else
        echo "WARN|$percentage|${violations[*]}"  # RECOMMENDED standard, so WARN not FAIL
    fi
}

# STD-TDD-001: Test-Driven Development
validate_tdd_001() {
    local repo_path="$1"
    local violations=()
    local score=0
    local max_score=3

    # Check 1: Tests directory exists
    if [[ -d "$repo_path/tests" ]]; then
        ((score++))
    else
        violations+=("No tests/ directory")
    fi

    # Check 2: Test configuration exists
    if [[ -f "$repo_path/pytest.ini" ]] || \
       [[ -f "$repo_path/pyproject.toml" ]] || \
       [[ -f "$repo_path/jest.config.js" ]] || \
       [[ -f "$repo_path/vitest.config.ts" ]]; then
        ((score++))
    else
        violations+=("No test configuration file (pytest.ini, jest.config.js, etc.)")
    fi

    # Check 3: Test files exist for source modules
    if [[ -d "$repo_path/src" ]] && [[ -d "$repo_path/tests" ]]; then
        local src_modules=$(find "$repo_path/src" -type f \( -name "*.py" -o -name "*.js" -o -name "*.ts" \) 2>/dev/null | wc -l)
        local test_files=$(find "$repo_path/tests" -type f \( -name "test_*.py" -o -name "*test.py" -o -name "*.test.js" -o -name "*.test.ts" \) 2>/dev/null | wc -l)

        if [[ $src_modules -gt 0 ]] && [[ $test_files -gt 0 ]]; then
            ((score++))
        else
            violations+=("Source modules exist but no test files found")
        fi
    else
        # Can't check, give benefit of doubt
        ((score++))
    fi

    # Calculate percentage
    local percentage=$((score * 100 / max_score))

    if [[ $percentage -eq 100 ]]; then
        echo "PASS|100|"
    elif [[ $percentage -ge 75 ]]; then
        echo "WARN|$percentage|${violations[*]}"
    else
        echo "WARN|$percentage|${violations[*]}"  # RECOMMENDED standard, so WARN not FAIL
    fi
}

# ============================================================================
# Report Generation
# ============================================================================

generate_report_header() {
    cat <<EOF
# Cross-Repository Standards Compliance Report

**Generated:** $(date +"%Y-%m-%d %H:%M:%S")
**Workspace:** $WORKSPACE_ROOT
**Total Repositories:** $1

---

## Summary

EOF
}

generate_report_table() {
    cat <<EOF

## Detailed Compliance Matrix

| Repository | ORG-001 | BEHAVE-001 | FILE-001 | REPORT-001 | TDD-001 | Overall | Status |
|-----------|---------|------------|----------|------------|---------|---------|--------|
EOF
}

calculate_overall_score() {
    local org_pct="$1"
    local behave_pct="$2"
    local file_pct="$3"
    local report_pct="$4"
    local tdd_pct="$5"

    # Mandatory standards (ORG, BEHAVE, FILE) weighted 60%
    # Recommended standards (REPORT, TDD) weighted 40%
    local mandatory_avg=$(( (org_pct + behave_pct + file_pct) / 3 ))
    local recommended_avg=$(( (report_pct + tdd_pct) / 2 ))
    local overall=$(( (mandatory_avg * 60 + recommended_avg * 40) / 100 ))

    echo "$overall"
}

get_status_emoji() {
    local score="$1"

    if [[ $score -eq 100 ]]; then
        echo "✅"
    elif [[ $score -ge 75 ]]; then
        echo "⚠️"
    else
        echo "❌"
    fi
}

# ============================================================================
# Main Validation Loop
# ============================================================================

validate_all_repositories() {
    local repos=("$@")
    local total_repos=${#repos[@]}

    # Create report directory
    mkdir -p "$REPORT_DIR"

    # Generate report header
    generate_report_header "$total_repos" > "$REPORT_FILE"

    # Track overall statistics
    local total_pass=0
    local total_warn=0
    local total_fail=0

    # Summary data
    declare -A repo_scores

    log_info "Validating $total_repos repositories..."
    echo ""

    # Validate each repository
    for repo in "${repos[@]}"; do
        local repo_name=$(basename "$repo")
        log_info "Validating: $repo_name"

        # Run all validations
        local org_result=$(validate_org_001 "$repo")
        local behave_result=$(validate_behave_001 "$repo")
        local file_result=$(validate_file_001 "$repo")
        local report_result=$(validate_report_001 "$repo")
        local tdd_result=$(validate_tdd_001 "$repo")

        # Parse results
        IFS='|' read -r org_status org_pct org_violations <<< "$org_result"
        IFS='|' read -r behave_status behave_pct behave_violations <<< "$behave_result"
        IFS='|' read -r file_status file_pct file_violations <<< "$file_result"
        IFS='|' read -r report_status report_pct report_violations <<< "$report_result"
        IFS='|' read -r tdd_status tdd_pct tdd_violations <<< "$tdd_result"

        # Calculate overall score
        local overall_score=$(calculate_overall_score "$org_pct" "$behave_pct" "$file_pct" "$report_pct" "$tdd_pct")
        local overall_emoji=$(get_status_emoji "$overall_score")

        # Track statistics
        if [[ "$org_status" == "FAIL" ]] || [[ "$behave_status" == "FAIL" ]] || [[ "$file_status" == "FAIL" ]]; then
            ((total_fail++))
        elif [[ "$org_status" == "WARN" ]] || [[ "$behave_status" == "WARN" ]] || [[ "$file_status" == "WARN" ]] || \
             [[ "$report_status" == "WARN" ]] || [[ "$tdd_status" == "WARN" ]]; then
            ((total_warn++))
        else
            ((total_pass++))
        fi

        # Store for summary
        repo_scores["$repo_name"]="$overall_score|$org_status|$behave_status|$file_status|$report_status|$tdd_status"

        # Output to console
        if [[ "$overall_emoji" == "✅" ]]; then
            log_success "$repo_name: ${overall_score}%"
        elif [[ "$overall_emoji" == "⚠️" ]]; then
            log_warning "$repo_name: ${overall_score}%"
        else
            log_error "$repo_name: ${overall_score}%"
        fi

        # Print violations if any
        [[ -n "$org_violations" ]] && echo "  ORG-001: $org_violations"
        [[ -n "$behave_violations" ]] && echo "  BEHAVE-001: $behave_violations"
        [[ -n "$file_violations" ]] && echo "  FILE-001: $file_violations"
        [[ -n "$report_violations" ]] && echo "  REPORT-001: $report_violations"
        [[ -n "$tdd_violations" ]] && echo "  TDD-001: $tdd_violations"
        echo ""
    done

    # Generate summary section
    cat <<EOF >> "$REPORT_FILE"
**Fully Compliant (✅):** $total_pass repositories
**Partial Compliance (⚠️):** $total_warn repositories
**Non-Compliant (❌):** $total_fail repositories

**Compliance Rate:** $(( (total_pass * 100) / total_repos ))%

---

EOF

    # Generate table
    generate_report_table >> "$REPORT_FILE"

    # Add rows
    for repo in "${repos[@]}"; do
        local repo_name=$(basename "$repo")
        IFS='|' read -r overall_score org_status behave_status file_status report_status tdd_status <<< "${repo_scores[$repo_name]}"

        local overall_emoji=$(get_status_emoji "$overall_score")

        echo "| $repo_name | $org_status | $behave_status | $file_status | $report_status | $tdd_status | ${overall_score}% | $overall_emoji |" >> "$REPORT_FILE"
    done

    # Add footer
    cat <<EOF >> "$REPORT_FILE"

---

## Legend

- **✅ PASS** - Fully compliant (100%)
- **⚠️ WARN** - Partial compliance (75-99%)
- **❌ FAIL** - Non-compliant (<75%)

## Standard Codes

- **ORG-001**: Repository Organization Pattern (MANDATORY)
- **BEHAVE-001**: Anti-Sycophancy Protocol (MANDATORY)
- **FILE-001**: File Management Rules (MANDATORY)
- **REPORT-001**: HTML Reporting Requirements (RECOMMENDED)
- **TDD-001**: Test-Driven Development (RECOMMENDED)

---

## Next Steps

1. Review violations for failed repositories
2. Prioritize MANDATORY standard fixes (ORG, BEHAVE, FILE)
3. Address RECOMMENDED standards (REPORT, TDD) for complete compliance
4. Re-run validation after fixes: \`bash scripts/validate_cross_repo_standards.sh\`

For detailed standard definitions, see: \`docs/standards/STANDARDS_INDEX.md\`

EOF

    log_success "Report generated: $REPORT_FILE"

    # Summary output
    echo ""
    echo "============================================"
    echo "VALIDATION COMPLETE"
    echo "============================================"
    echo "Total Repositories: $total_repos"
    echo "✅ Fully Compliant: $total_pass"
    echo "⚠️  Partial Compliance: $total_warn"
    echo "❌ Non-Compliant: $total_fail"
    echo "Compliance Rate: $(( (total_pass * 100) / total_repos ))%"
    echo ""
    echo "Full report: $REPORT_FILE"
    echo "============================================"
}

# ============================================================================
# Main Entry Point
# ============================================================================

main() {
    log_info "Cross-Repository Standards Validation"
    log_info "======================================"
    echo ""

    # Find all repositories
    mapfile -t repositories < <(find_repositories)

    if [[ ${#repositories[@]} -eq 0 ]]; then
        log_error "No repositories found in $WORKSPACE_ROOT"
        exit $EXIT_VALIDATION_FAILED
    fi

    echo ""

    # Validate all repositories
    validate_all_repositories "${repositories[@]}"

    echo ""
    log_success "Validation complete!"

    exit $EXIT_SUCCESS
}

# Run main
main "$@"
