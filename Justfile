# Just list all recipes by default
default:
    just --list

docs:
    stack haddock linear-locks:lib

doctest:
    stack build doctest
    stack exec doctest -- $(find src \( -name '*.lhs' -o -name '*.hs' \) -print) \
        -XGHC2024 -XBlockArguments -XDuplicateRecordFields -XOverloadedRecordDot -XTypeFamilies
