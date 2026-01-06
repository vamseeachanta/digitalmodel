#!/usr/bin/env bash
#
# OrcaWave Benchmark Execution Helper
#
# ABOUTME: Interactive script to help choose and execute the best method
# for running the 180-case OrcaWave benchmark given the API hanging issue.

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Directories
BENCHMARK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EXAMPLES_DIR="${BENCHMARK_DIR}/../examples/L01_default_vessel"
CONFIG_FILE="${BENCHMARK_DIR}/orcawave_001_ship_raos_rev2.yml"

# Banner
echo -e "${BLUE}============================================================${NC}"
echo -e "${BLUE}  OrcaWave Benchmark Execution Helper${NC}"
echo -e "${BLUE}============================================================${NC}"
echo -e ""
echo -e "Problem: 180-case benchmark (20 periods × 9 headings)"
echo -e "Issue: Python API hangs/timeouts"
echo -e "Solutions: GUI, Batch, or Diagnostic"
echo -e ""

# Check if config file exists
if [ ! -f "$CONFIG_FILE" ]; then
    echo -e "${RED}[ERROR]${NC} Configuration file not found:"
    echo -e "        $CONFIG_FILE"
    exit 1
fi

# Check if improved script exists
IMPROVED_SCRIPT="${EXAMPLES_DIR}/run_orcawave_diffraction_improved.py"
DIAGNOSTIC_SCRIPT="${EXAMPLES_DIR}/diagnose_orcawave_api.py"

if [ ! -f "$IMPROVED_SCRIPT" ]; then
    echo -e "${RED}[ERROR]${NC} Improved script not found:"
    echo -e "        $IMPROVED_SCRIPT"
    exit 1
fi

# Main menu
echo -e "${BLUE}============================================================${NC}"
echo -e "${BLUE}  Choose Execution Method${NC}"
echo -e "${BLUE}============================================================${NC}"
echo -e ""
echo -e "  ${GREEN}1)${NC} GUI Execution (MOST RELIABLE)"
echo -e "     • Proven to work"
echo -e "     • Time: ~1 hour"
echo -e "     • Output: Single .owr file"
echo -e "     • Method: Manual (opens GUI)"
echo -e ""
echo -e "  ${GREEN}2)${NC} Batch Execution (FASTEST AUTOMATED)"
echo -e "     • Automated via Python"
echo -e "     • Time: ~90 seconds"
echo -e "     • Output: 9 .owr files (one per heading)"
echo -e "     • Method: Automatic (batch processing)"
echo -e ""
echo -e "  ${GREEN}3)${NC} Run Diagnostic (INVESTIGATION)"
echo -e "     • Test incremental problem sizes"
echo -e "     • Analyze mesh quality"
echo -e "     • Time: ~5-10 minutes"
echo -e "     • Output: Diagnostic report"
echo -e ""
echo -e "  ${GREEN}4)${NC} View Documentation"
echo -e "     • Root cause analysis"
echo -e "     • Quick reference guide"
echo -e "     • Investigation summary"
echo -e ""
echo -e "  ${GREEN}5)${NC} Exit"
echo -e ""
echo -ne "${BLUE}Enter choice [1-5]:${NC} "
read -r choice

case $choice in
    1)
        # GUI Execution
        echo -e ""
        echo -e "${BLUE}============================================================${NC}"
        echo -e "${BLUE}  GUI Execution${NC}"
        echo -e "${BLUE}============================================================${NC}"
        echo -e ""
        echo -e "${YELLOW}[INFO]${NC} Opening OrcaWave GUI..."
        echo -e ""
        echo -e "${GREEN}Instructions:${NC}"
        echo -e "  1. OrcaWave GUI will open"
        echo -e "  2. File → Open → Select config file:"
        echo -e "     ${CONFIG_FILE}"
        echo -e "  3. Calculation → Calculate"
        echo -e "  4. Set threads: 60 (or leave default)"
        echo -e "  5. Click OK and wait ~1 hour"
        echo -e "  6. Results saved automatically as .owr file"
        echo -e ""
        echo -ne "${BLUE}Press Enter to launch OrcaWave GUI...${NC}"
        read -r

        # Find OrcaWave executable
        ORCAWAVE_PATHS=(
            "/c/Program Files (x86)/Orcina/OrcaFlex/11.6/OrcaWave64.exe"
            "/c/Program Files (x86)/Orcina/OrcaFlex/11.6/OrcaWave.exe"
            "/c/Program Files/Orcina/OrcaFlex/11.6/OrcaWave64.exe"
            "/c/Program Files/Orcina/OrcaFlex/11.6/OrcaWave.exe"
        )

        ORCAWAVE_EXE=""
        for path in "${ORCAWAVE_PATHS[@]}"; do
            if [ -f "$path" ]; then
                ORCAWAVE_EXE="$path"
                break
            fi
        done

        if [ -z "$ORCAWAVE_EXE" ]; then
            echo -e "${RED}[ERROR]${NC} OrcaWave executable not found!"
            echo -e "Searched paths:"
            for path in "${ORCAWAVE_PATHS[@]}"; do
                echo -e "  - $path"
            done
            exit 1
        fi

        echo -e "${GREEN}[LAUNCHING]${NC} $ORCAWAVE_EXE"
        "$ORCAWAVE_EXE" &
        echo -e ""
        echo -e "${YELLOW}[NOTE]${NC} OrcaWave launched in background"
        echo -e "       Follow instructions above to load config and run"
        ;;

    2)
        # Batch Execution
        echo -e ""
        echo -e "${BLUE}============================================================${NC}"
        echo -e "${BLUE}  Batch Execution${NC}"
        echo -e "${BLUE}============================================================${NC}"
        echo -e ""
        echo -e "${YELLOW}[INFO]${NC} Running batch execution..."
        echo -e "       Splitting 180 cases into 9 batches (1 heading each)"
        echo -e "       Each batch: 20 periods × 1 heading = 20 cases"
        echo -e "       Expected time: ~90 seconds total"
        echo -e ""
        echo -ne "${BLUE}Press Enter to start batch execution...${NC}"
        read -r

        cd "$EXAMPLES_DIR"
        python run_orcawave_diffraction_improved.py \
            "$CONFIG_FILE" \
            --force-batch \
            --threads 12

        echo -e ""
        echo -e "${GREEN}[COMPLETE]${NC} Batch execution finished"
        echo -e ""
        echo -e "${YELLOW}[NOTE]${NC} Results saved as 9 separate .owr files"
        echo -e "       Location: ${BENCHMARK_DIR}/batch_output_*/"
        echo -e "       Each file contains results for one heading"
        echo -e ""
        echo -e "${YELLOW}[TODO]${NC} Merge results (not yet automated):"
        echo -e "       Manual merging required to combine into single file"
        ;;

    3)
        # Run Diagnostic
        echo -e ""
        echo -e "${BLUE}============================================================${NC}"
        echo -e "${BLUE}  Diagnostic Tests${NC}"
        echo -e "${BLUE}============================================================${NC}"
        echo -e ""
        echo -e "Choose diagnostic mode:"
        echo -e ""
        echo -e "  ${GREEN}a)${NC} Mesh Quality Analysis Only (~30 seconds)"
        echo -e "  ${GREEN}b)${NC} Incremental Problem Size Test (~5-10 minutes)"
        echo -e "  ${GREEN}c)${NC} Full Diagnostic Suite (~10-15 minutes)"
        echo -e ""
        echo -ne "${BLUE}Enter choice [a-c]:${NC} "
        read -r diag_choice

        case $diag_choice in
            a)
                echo -e ""
                echo -e "${YELLOW}[INFO]${NC} Running mesh quality analysis..."
                cd "$EXAMPLES_DIR"
                python diagnose_orcawave_api.py "$CONFIG_FILE" --mesh-only
                ;;
            b)
                echo -e ""
                echo -e "${YELLOW}[INFO]${NC} Running incremental problem size test..."
                echo -e "       Testing: 4, 16, 36, 90, 180 cases"
                echo -e "       This will identify the breaking point"
                echo -e ""
                cd "$EXAMPLES_DIR"
                python diagnose_orcawave_api.py "$CONFIG_FILE" --incremental
                ;;
            c)
                echo -e ""
                echo -e "${YELLOW}[INFO]${NC} Running full diagnostic suite..."
                cd "$EXAMPLES_DIR"
                python diagnose_orcawave_api.py "$CONFIG_FILE"
                ;;
            *)
                echo -e "${RED}[ERROR]${NC} Invalid choice"
                exit 1
                ;;
        esac

        echo -e ""
        echo -e "${GREEN}[COMPLETE]${NC} Diagnostic finished"
        echo -e "       Output directory: diagnostic_output/"
        ;;

    4)
        # View Documentation
        echo -e ""
        echo -e "${BLUE}============================================================${NC}"
        echo -e "${BLUE}  Documentation${NC}"
        echo -e "${BLUE}============================================================${NC}"
        echo -e ""
        echo -e "Available documentation:"
        echo -e ""
        echo -e "  ${GREEN}1)${NC} ROOT_CAUSE_ANALYSIS.md"
        echo -e "     Comprehensive technical deep-dive"
        echo -e "     3 root causes, 5 solutions, test results"
        echo -e ""
        echo -e "  ${GREEN}2)${NC} QUICK_REFERENCE_API_FIXES.md"
        echo -e "     Quick-start guide with commands"
        echo -e "     Workflow diagrams, FAQ section"
        echo -e ""
        echo -e "  ${GREEN}3)${NC} API_INVESTIGATION_SUMMARY.md"
        echo -e "     High-level overview and recommendations"
        echo -e "     Key findings and next actions"
        echo -e ""
        echo -ne "${BLUE}Enter choice [1-3] or Enter to skip:${NC} "
        read -r doc_choice

        case $doc_choice in
            1)
                if command -v less &> /dev/null; then
                    less "${BENCHMARK_DIR}/ROOT_CAUSE_ANALYSIS.md"
                else
                    cat "${BENCHMARK_DIR}/ROOT_CAUSE_ANALYSIS.md"
                fi
                ;;
            2)
                if command -v less &> /dev/null; then
                    less "${BENCHMARK_DIR}/QUICK_REFERENCE_API_FIXES.md"
                else
                    cat "${BENCHMARK_DIR}/QUICK_REFERENCE_API_FIXES.md"
                fi
                ;;
            3)
                if command -v less &> /dev/null; then
                    less "${BENCHMARK_DIR}/API_INVESTIGATION_SUMMARY.md"
                else
                    cat "${BENCHMARK_DIR}/API_INVESTIGATION_SUMMARY.md"
                fi
                ;;
            "")
                echo -e "${YELLOW}[INFO]${NC} Skipping documentation view"
                ;;
            *)
                echo -e "${RED}[ERROR]${NC} Invalid choice"
                exit 1
                ;;
        esac
        ;;

    5)
        # Exit
        echo -e ""
        echo -e "${BLUE}Exiting...${NC}"
        exit 0
        ;;

    *)
        echo -e "${RED}[ERROR]${NC} Invalid choice"
        exit 1
        ;;
esac

echo -e ""
echo -e "${BLUE}============================================================${NC}"
echo -e "${BLUE}  Additional Resources${NC}"
echo -e "${BLUE}============================================================${NC}"
echo -e ""
echo -e "Documentation files:"
echo -e "  • ${BENCHMARK_DIR}/ROOT_CAUSE_ANALYSIS.md"
echo -e "  • ${BENCHMARK_DIR}/QUICK_REFERENCE_API_FIXES.md"
echo -e "  • ${BENCHMARK_DIR}/API_INVESTIGATION_SUMMARY.md"
echo -e ""
echo -e "Tool scripts:"
echo -e "  • ${EXAMPLES_DIR}/run_orcawave_diffraction_improved.py"
echo -e "  • ${EXAMPLES_DIR}/diagnose_orcawave_api.py"
echo -e ""
echo -e "${GREEN}[SUCCESS]${NC} Operation complete!"
echo -e ""
