# Just list all recipes by default
default:
    just --list

checks:
    just doctest
    just test
    just haddock
    xrefcheck
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

# Note: `stack haddock` fails to create hyperlinks to definitions in other packages (e.g. `Ur` from `linear-base`)

haddock:
    ./scripts/check_haddock_warnings.sh lib:linear-locks

# Run haddock in "file watch" mode
haddock-fw:
    watchexec --clear --exts hs -- just haddock

haddock-hackage *ARGS:
    cabal update
    cabal haddock lib:linear-locks --haddock-for-hackage {{ ARGS }}

pandoc:
    ./scripts/run_pandoc.sh
