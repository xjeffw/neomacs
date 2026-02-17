#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$script_dir/oracle-emacs-path.sh"
expected="$hardcoded_oracle_emacs"

check_script_path() {
  local file="$1"

  if [[ ! -f "$file" ]]; then
    echo "missing script: $file" >&2
    exit 1
  fi

  if ! grep -q "oracle-emacs-path.sh" "$file"; then
    echo "missing oracle path include in $file" >&2
    exit 1
  fi
}

report_path() {
  echo "oracle default path checks passed: $expected"
}

check_script_path "$script_dir/run-oracle.sh"
check_script_path "$script_dir/run-ert-allowlist.sh"
report_path
