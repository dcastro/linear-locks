# Just list all recipes by default
default:
    just --list

checks:
    ./scripts/check_doctest.sh
    just doctest
    just test
    # Build with `-Werror`
    stack clean && stack build --fast --test --bench --no-run-tests --no-run-benchmarks --ghc-options "-Werror"

# To run with a file watcher: just test --file-watch
test *ARGS:
    stack test linear-locks:test:linear-locks-test {{ ARGS }}

doctest:
    stack build doctest
    stack exec doctest -- $(find src examples/src \( -name '*.lhs' -o -name '*.hs' \) -print) \
        -XGHC2024 -XBlockArguments -XDuplicateRecordFields -XOverloadedRecordDot -XTypeFamilies

docs:
    stack haddock linear-locks:lib
