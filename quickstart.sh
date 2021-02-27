echo "=====START Hackage Installation====="
cabal update
cabal install Cabal cabal-install
cabal install JuicyPixels
echo "=====END Hackage Installation====="
echo "=====START MAIN PROGRAM====="
cabal build
cabal run