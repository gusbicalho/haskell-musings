#! /bin/bash
cabal clean &> /dev/null
cabal build \
    -O2 -fexpose-all-unfoldings -fspecialise-aggressively -flate-specialise \
    lib:Freedoms exe:Freedoms

time cabal run -O2 Freedoms -- counter IOFreeData
time cabal run -O2 Freedoms -- counter StateFreeData
time cabal run -O2 Freedoms -- counter IOFreeChurch
time cabal run -O2 Freedoms -- counter StateFreeChurch
time cabal run -O2 Freedoms -- counter IOFreeFinalReader
time cabal run -O2 Freedoms -- counter StateFreeFinalReader
time cabal run -O2 Freedoms -- counter IOFreeFinalClassy
time cabal run -O2 Freedoms -- counter StateFreeFinalClassy
time cabal run -O2 Freedoms -- counter IOFreerData
time cabal run -O2 Freedoms -- counter StateFreerData
time cabal run -O2 Freedoms -- counter IOFreerChurch
time cabal run -O2 Freedoms -- counter StateFreerChurch
time cabal run -O2 Freedoms -- counter IOFreerFinalReader
time cabal run -O2 Freedoms -- counter StateFreerFinalReader
time cabal run -O2 Freedoms -- counter IOFreerFinalClassy
time cabal run -O2 Freedoms -- counter StateFreerFinalClassy
