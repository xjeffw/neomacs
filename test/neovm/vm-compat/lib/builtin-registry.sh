#!/usr/bin/env bash

# Collect all entries from DISPATCH_BUILTIN_NAMES into OUT_FILE.
collect_dispatch_builtin_names() {
  local registry_file="$1"
  local out_file="$2"

  awk '
    /const DISPATCH_BUILTIN_NAMES:/ && /=[[:space:]]*&\[/ { in_table=1; next }
    in_table && /^[[:space:]]*\];/ { in_table=0; exit }
    in_table {
      if (match($0, /^[[:space:]]*"([^"]+)",[[:space:]]*$/, m)) {
        print m[1]
      }
    }
  ' "$registry_file" > "$out_file"
}

# Collect core entries (excluding NeoVM extension names).
collect_core_dispatch_builtin_names() {
  local all_names_file="$1"
  local out_file="$2"
  grep -v '^neovm-' "$all_names_file" > "$out_file"
}

# Collect extension entries (NeoVM-prefixed names only), sorted unique.
collect_extension_dispatch_builtin_names() {
  local all_names_file="$1"
  local out_file="$2"
  grep '^neovm-' "$all_names_file" | sort -u > "$out_file" || true
}
