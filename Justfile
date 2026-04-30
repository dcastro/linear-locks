# Just list all recipes by default
default:
    just --list

checks:
    just doctest
    just test
    # Build with `-Werror`
    stack clean && stack build --fast --test --bench --no-run-tests --no-run-benchmarks --ghc-options "-Werror"

# To run with a file watcher: just test --file-watch
test *ARGS:
    stack test --fast linear-locks:test:linear-locks-test {{ ARGS }}

doctest:
    ./scripts/check_doctest.sh
    stack build doctest
    stack exec doctest -- $(find src test examples/src \( -name '*.lhs' -o -name '*.hs' \) -print) \
        -XGHC2024 -XBlockArguments -XDuplicateRecordFields -XOverloadedRecordDot -XTypeFamilies -XQualifiedDo

docs:
    stack haddock linear-locks:lib
