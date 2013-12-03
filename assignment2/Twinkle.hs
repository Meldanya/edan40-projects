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

part1 = a1 :+: a2 :+: a3 :+: a4
a1 = lmap (fd qn) [c 5, c 5, g 5, g 5, a 5, a 5]
a2 = (fd hn) (g 5)
a3 = lmap (fd qn) [f 5, f 5, e 5, e 5, d 5, d 5]
a4 = (fd hn) (c 5)

part2 = b1 :+: b2 :+: b1 :+: b2
b1 = lmap (fd qn) [g 5, g 5, f 5, f 5, e 5, e 5]
b2 = (fd hn) (d 5)

twinkleMelody = Instr "piano" (part1 :+: part2 :+: part1)


--------------------------------------------------------------------------------
-- Chords

chordC = (C, 1%1)
chordGC = [(G, 1%2), (C, 1%2)]
chordCG = [(C, 1%2), (G, 1%2)]
chordFC = [(F, 1%2), (C, 1%2)]

firstLine = [chordC] ++ chordFC ++ chordGC ++ chordGC
secondLine = chordCG ++ chordCG ++ chordCG ++ chordCG

-- TODO: Comment back in when AutoComp compiles:
twinkleChords :: ChordProgression
twinkleChords = firstLine ++ secondLine ++ firstLine

-- Debug
tbb = Tempo (2%1) (twinkleMelody :=: autoBass basic (C, Major) twinkleChords)
tcb = Tempo (2%1) (twinkleMelody :=: autoBass calypso (C, Major) twinkleChords)
tbob  = Tempo (2%1) (twinkleMelody :=: autoBass boogie (C, Major) twinkleChords)

tbc = Tempo (2%1) (twinkleMelody :=: autoChord (C, Major) twinkleChords)
tcc = Tempo (2%1) (twinkleMelody :=: autoChord (C, Major) twinkleChords)
tboc  = Tempo (2%1) (twinkleMelody :=: autoChord (C, Major) twinkleChords)

twinkleBasic  = Tempo (2%1) (twinkleMelody :=: autoComp basic (C, Major) twinkleChords)
twinkleCalypso  = Tempo (2%1) (twinkleMelody :=: autoComp calypso (C, Major) twinkleChords)
twinkleBoogie  = Tempo (2%1) (twinkleMelody :=: autoComp boogie (C, Major) twinkleChords)

main = do
    putStrLn "Choose basic/calypso/boogie: "
    choice <- getLine
    if choice == "basic"
        then test twinkleBasic
        else if choice == "calypso"
            then test twinkleCalypso
            else test twinkleBoogie
