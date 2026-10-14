# Shared early help guard for the top-level CFD shell tools.
case "${1:-}" in
  -h|--help)
    awk 'NR <= 2 { next } /^#/ { sub(/^# ?/, ""); print; next } { exit }' "${BASH_SOURCE[1]}"
    exit 0
    ;;
esac
