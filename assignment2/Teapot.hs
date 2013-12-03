import Haskore
import Data.Ratio
import AutoComp

--------------------------------------------------------------------------------
-- Helper functions (stolen from Haskore tutorial)

-- Note updaters for mappings
fd d n = n d v
vol n = n v
v = [Volume 80]
lmap f l = line (map f l)

-- Repeat something n times
times 1 m = m
times n m = m :+: (times (n-1) m)

--------------------------------------------------------------------------------
-- Melody
-- ?
{- part1 = a1 :+: a2 :+: a3 :+: a4 :+: a5 :+: a6 :+: a7 :+: a8 :+: a9-}
part1 = line [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a1, a2, a3, a4, a11, enr, a12, a13, a14, a15]
a1 = lmap (fd en) [c 5, d 5, e 5, f 5]
a2 = lmap (fd qn) [g 5, c 6]
a3 = lmap (fd qn) [a 5, c 6]
a4 = (fd hn) (g 5)
a5 = (fd qn) (f 5)
a6 = lmap (fd en) [f 5, f 5]
a7 = lmap (fd qn) [e 5, e 5]
a8 = (fd qn) (d 5)
a9 = lmap (fd en) [d 5, d 5]
a10 = (fd hn) (c 5)
a11 = (fd qn) (c 6)
a12 = lmap (fd en) [a 5, g 5, g 5]
a13 = (fd qn) (f 5)
a14 = lmap (fd qn) [e 5, d 5]
a15 = (fd hn) (c 5)

{- part2 = b1 :+: b2 :+: b1 :+: b2-}
{- b1 = lmap (fd qn) [g 5, g 5, f 5, f 5, e 5, e 5]-}
{- b2 = (fd hn) (d 5)-}

-- twinkleMelody = Tempo (2%1) $ Instr "piano" (part1 :+: part2 :+: part1)
teapotMelody = Instr "piano" (part1)

--------------------------------------------------------------------------------
-- Chords

teapotChords :: ChordProgression
teapotChords = c ++ fc ++ fc ++ gc ++ c ++ fc ++ c ++ gc
    where
      c  = [(C, 1%1)]
      fc = [(F, 1%2), (C, 1%2)]
      gc = [(G, 1%2), (C, 1%2)]

teapotBasic  = Tempo (2%1) (teapotMelody :=: autoComp basic (C, Major) teapotChords)
teapotCalypso  = Tempo (2%1) (teapotMelody :=: autoComp calypso (C, Major) teapotChords)
teapotBoogie  = Tempo (2%1) (teapotMelody :=: autoComp boogie (C, Major) teapotChords)

main = do
    putStrLn "Choose basic/calypso/boogie: "
    choice <- getLine
    if choice == "basic"
        then test teapotBasic
        else if choice == "calypso"
            then test teapotCalypso
            else test teapotBoogie
