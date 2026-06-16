#!/usr/bin/env bash
# test-changed-smart.sh
# run tests for changed R files AND any files that depend on them

set -e

# get changed R files in R/ directory
changed_r_files=$(git diff --name-only HEAD | grep '^R/.*\.R$' || true)

if [ -z "$changed_r_files" ]; then
  echo "No R files changed in R/ directory"
  exit 0
fi

echo "Changed R files:"
echo "$changed_r_files"
echo ""

# extract function names from changed files and find tests that use them
test_patterns=""
dependent_tests=""

for rfile in $changed_r_files; do
  basename=$(basename "$rfile" .R)
  test_patterns="${test_patterns}|${basename}"
  
  # extract exported function names from the R file
  # look for function definitions: function_name <- function(
  funcs=$(grep -E '^[a-zA-Z_][a-zA-Z0-9_.]*\s*<-\s*function\s*\(' "R/$basename.R" 2>/dev/null | \
          sed -E 's/^([a-zA-Z_][a-zA-Z0-9_.]*).*/\1/' || true)
  
  if [ -n "$funcs" ]; then
    echo "Functions in $basename.R:"
    echo "$funcs" | sed 's/^/  - /'
    
    # search test files for usage of these functions
    for func in $funcs; do
      # find test files that call this function
      matching_tests=$(grep -l "${func}(" tests/testthat/test-*.R 2>/dev/null || true)
      
      if [ -n "$matching_tests" ]; then
        for test_file in $matching_tests; do
          test_base=$(basename "$test_file" .R | sed 's/^test-//')
          # avoid duplicates
          if [[ ! "$test_patterns" =~ "$test_base" ]]; then
            dependent_tests="${dependent_tests}|${test_base}"
          fi
        done
      fi
    done
  fi
done

# combine direct and dependent test patterns
all_patterns="${test_patterns}${dependent_tests}"
all_patterns=${all_patterns#|}

if [ -z "$all_patterns" ]; then
  echo "No matching test files found"
  exit 0
fi

echo ""
echo "Running tests for changed files AND their dependents:"
echo "Pattern: $all_patterns"
echo ""

# run matching tests
R -q -e "devtools::test(filter = '$all_patterns')"
