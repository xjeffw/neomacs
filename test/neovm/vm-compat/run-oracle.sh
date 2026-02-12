#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <forms-file>" >&2
  exit 2
fi

forms_file="$1"
if [[ ! -f "$forms_file" ]]; then
  echo "forms file not found: $forms_file" >&2
  exit 2
fi

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
oracle_el="$script_dir/oracle_eval.el"

if ! command -v emacs >/dev/null 2>&1; then
  echo "emacs binary not found in PATH" >&2
  exit 127
fi

NEOVM_FORMS_FILE="$forms_file" emacs --batch -Q -l "$oracle_el" 2>/dev/null
