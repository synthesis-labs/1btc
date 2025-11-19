cabal run --disable-profiling -- tx \
    +RTS \
    -A32m \
    -n4m \
    -qg \
    -I0 \
    -RTS