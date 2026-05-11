# Just list all recipes by default
default:
    just --list

checks:
    just doctest
    just test
    just haddock
    just pandoc
    # check markdown links
    xrefcheck --ignore "release/**/*"
    # Build with `-Werror`
    stack clean && stack build --fast --test --bench --no-run-tests --no-run-benchmarks --ghc-options "-Werror"
    # Build with the lowest supported version of each dependency.
    cabal clean && just min-deps

# Build the project with the lowest supported version of each dependency.
min-deps:
    cabal build all \
        --constraint='linear-base ==0.4.0' \
        --constraint='stm-containers ==1.2.1' \
        --constraint='focus ==1.0.3.2' \
        --constraint='atomic-primops ==0.8.4' \
        --constraint='vector ==0.13.1.0' \
        --constraint='vector-algorithms ==0.9.0.1' \
        --constraint='containers ==0.6.8' \
        --constraint='deepseq ==1.5.0.0' \
        --constraint='concurrent-extra ==0.7.0.12' \
        --ghc-options="-Werror" \
        --with-compiler=ghc-9.10.3

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

############################################################################
## Release
############################################################################

publish-candidate:
    just checks

    rm -rf dist-newstyle
    rm -rf release && mkdir release

    cabal sdist --builddir release
    cabal upload release/sdist/*.tar.gz

publish-candidate-docs *ARGS:
    just checks

    rm -rf release/docs
    mkdir -p release/docs
    cabal update
    cabal haddock lib:linear-locks --haddock-for-hackage --builddir release/docs
    cabal upload --documentation {{ ARGS }} release/docs/*-docs.tar.gz

publish-final-docs:
    just publish-candidate-docs --publish
