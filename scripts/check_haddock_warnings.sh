#!/usr/bin/env bash
set -euo pipefail

# This script runs `cabal haddock <target>` and checks for any warnings in the output.
# It filters out expected warnings defined in `scripts/expected_haddock_warnings.yaml`
# and fails if any unexpected warnings are found.

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <cabal-target>" >&2
  echo "Example: $0 lib:linear-locks" >&2
  exit 2
fi

target="$1"
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
expected_file="$repo_root/scripts/expected_haddock_warnings.yaml"

if [[ ! -f "$expected_file" ]]; then
  echo "Expected warnings file not found: $expected_file" >&2
  exit 2
fi

parse_expected_warnings() {
  local yaml_file="$1"
  local collecting=false
  local block=""
  local block_indent=-1
  local leading_ws=""
  local leading_len=0

  while IFS= read -r line || [[ -n "$line" ]]; do
    if [[ "$line" =~ ^[[:space:]]*-[[:space:]]*\|[-+]?[[:space:]]*$ ]]; then
      if [[ -n "$block" ]]; then
        printf '%s\0' "${block%$'\n'}"
        block=""
      fi
      collecting=true
      block_indent=-1
      continue
    fi

    if [[ "$collecting" == true ]]; then
      if [[ "$line" =~ ^[[:space:]]*$ ]]; then
        if (( block_indent >= 0 )); then
          block+=$'\n'
        fi
      else
        leading_ws="${line%%[^[:space:]]*}"
        leading_len=${#leading_ws}

        if (( block_indent < 0 )); then
          block_indent=$leading_len
        fi

        if (( leading_len >= block_indent )); then
          block+="${line:block_indent}"
          block+=$'\n'
        else
          if [[ -n "$block" ]]; then
            printf '%s\0' "${block%$'\n'}"
            block=""
          fi
          block_indent=-1
          collecting=false
        fi
      fi
    fi
  done < "$yaml_file"

  if [[ -n "$block" ]]; then
    printf '%s\0' "${block%$'\n'}"
  fi
}

stdout_file="$(mktemp)"
trap 'rm -f "$stdout_file"' EXIT

set +e
cabal haddock "$target" > "$stdout_file"
cabal_status=$?
set -e

full_output="$(cat "$stdout_file")"
normalized_output="$(printf '%s\n' "$full_output" | expand -t 8 | sed 's/[[:space:]]\+$//')"
filtered_output="$normalized_output"

# If Haddock produced documentation, always print that section (line and everything after it).
if sed -n '/^Documentation created:/,${p;}' "$stdout_file" | grep -q '.'; then
  sed -n '/^Documentation created:/,${p;}' "$stdout_file"
fi

while IFS= read -r -d '' expected_warning; do
  normalized_warning="$(printf '%s\n' "$expected_warning" | expand -t 8 | sed 's/[[:space:]]\+$//')"
  filtered_output="$(printf '%s' "$filtered_output" | EXPECTED_WARNING="$normalized_warning" perl -0pe 's/\Q$ENV{EXPECTED_WARNING}\E//g')"
done < <(parse_expected_warnings "$expected_file")

if grep -qE '^Warning: ' <<< "$filtered_output"; then
  printf '%s\n' "$filtered_output"
  echo ">>> Haddock warnings found"
  exit 1
fi

if (( cabal_status != 0 )); then
  printf '%s\n' "$filtered_output"
  echo ">>> 'cabal haddock' failed"
  exit "$cabal_status"
fi

echo ">>> Haddock warning check passed for target: $target"
