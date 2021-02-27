echo "=====START Hackage Installation====="
cabal update
cabal install Cabal cabal-install
cabal install gloss
cabal install JuicyPixels
cabal install gloss-juicy
echo "=====END Hackage Installation====="
echo "=====START MAIN PROGRAM====="
cabal build
cabal run