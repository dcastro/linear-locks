#!/usr/bin/env bash
set -euo pipefail

# This script looks for doctests missing a pipe `|`
# Such tests will be ignored by `doctest`, so we report an error reminding us to add the missing pipe.

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

while IFS= read -r -d '' file; do
  # Split the grepped line by the character ':' into line number and content
  while IFS=: read -r line match; do
    if [[ -z "${line}" ]]; then
      continue
    fi

    prev_line_number=$((line - 1))
    if (( prev_line_number >= 1 )); then
      prev_line=$(sed -n "${prev_line_number}p" "$file")
      # The pattern `-- >>>` is allowed if it's preceded by a line with `$setup`
      if [[ $prev_line =~ ^[[:space:]]*--[[:space:]]*\$setup[[:space:]]*$ ]]; then
        continue
      fi
      # The pattern `-- >>>` is allowed if it's preceded by a line with another `-- >>>`
      if [[ $prev_line =~ ^[[:space:]]*--[[:space:]]*\>\>\> ]]; then
        continue
      fi
    fi

    echo "Invalid doctest marker in $file:" >&2
    echo "${line}:${match}" >&2
    echo "This test will be ignored by doctest, replace with \`-- | >>>\`" >&2
    exit 1
    # Scan for lines starting with `-- >>>`
  done < <(grep -nE '^[[:space:]]*--[[:space:]]+>>>' "$file" || true)
# Scan .hs files
done < <(find "$repo_root" -name '*.hs' -print0)

exit 0
