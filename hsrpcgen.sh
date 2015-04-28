#!/usr/bin/env bash

D=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
runhaskell -i$D $D/HsRPCGen.hs "$@"

