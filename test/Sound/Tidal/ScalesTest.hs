{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.ScalesTest where

import TestUtils
import Test.Microspec

import Prelude hiding ((<*), (*>))

import Sound.Tidal.Scales
import Sound.Tidal.Pattern
import Sound.Tidal.ParseBP

run :: Microspec ()
run =
  describe "Sound.Tidal.Scales" $ do
    describe "scale" $ do
        describe "5 note scales" $ do
            let twoOctavesOf5NoteScale = "0 1 2 3 4 5 6 7 8 9"
            it "can transform notes correctly over 2 octaves - minPent" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "minPent" twoOctavesOf5NoteScale)
                    ("0 3 5 7 10 12 15 17 19 22"::Pattern Int)
            it "can transform notes correctly over 2 octaves - majPent" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "majPent" twoOctavesOf5NoteScale)
                    ("0 2 4 7 9 12 14 16 19 21"::Pattern Int)
            it "can transform notes correctly over 2 octaves - ritusen" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "ritusen" twoOctavesOf5NoteScale)
                    ("0 2 5 7 9 12 14 17 19 21"::Pattern Int)
            it "can transform notes correctly over 2 octaves - egyptian" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "egyptian" twoOctavesOf5NoteScale)
                    ("0 2 5 7 10 12 14 17 19 22"::Pattern Int)
            it "can transform notes correctly over 2 octaves - kumai" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "kumai" twoOctavesOf5NoteScale)
                    ("0 2 3 7 9 12 14 15 19 21"::Pattern Int)
            it "can transform notes correctly over 2 octaves - hirajoshi" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "hirajoshi" twoOctavesOf5NoteScale)
                    ("0 2 3 7 8 12 14 15 19 20"::Pattern Int)
            it "can transform notes correctly over 2 octaves - iwato" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "iwato" twoOctavesOf5NoteScale)
                    ("0 1 5 6 10 12 13 17 18 22"::Pattern Int)
            it "can transform notes correctly over 2 octaves - chinese" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "chinese" twoOctavesOf5NoteScale)
                    ("0 4 6 7 11 12 16 18 19 23"::Pattern Int)
            it "can transform notes correctly over 2 octaves - indian" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "indian" twoOctavesOf5NoteScale)
                    ("0 4 5 7 10 12 16 17 19 22"::Pattern Int)
            it "can transform notes correctly over 2 octaves - pelog" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "pelog" twoOctavesOf5NoteScale)
                    ("0 1 3 7 8 12 13 15 19 20"::Pattern Int)
            it "can transform notes correctly over 2 octaves - prometheus" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "prometheus" twoOctavesOf5NoteScale)
                    ("0 2 4 6 11 12 14 16 18 23"::Pattern Int)
            it "can transform notes correctly over 2 octaves - scriabin" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "scriabin" twoOctavesOf5NoteScale)
                    ("0 1 4 7 9 12 13 16 19 21"::Pattern Int)
            it "can transform notes correctly over 2 octaves - gong" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "gong" twoOctavesOf5NoteScale)
                    ("0 2 4 7 9 12 14 16 19 21"::Pattern Int)
            it "can transform notes correctly over 2 octaves - shang" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "shang" twoOctavesOf5NoteScale)
                    ("0 2 5 7 10 12 14 17 19 22"::Pattern Int)
            it "can transform notes correctly over 2 octaves - jiao" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "jiao" twoOctavesOf5NoteScale)
                    ("0 3 5 8 10 12 15 17 20 22"::Pattern Int)
            it "can transform notes correctly over 2 octaves - zhi" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "zhi" twoOctavesOf5NoteScale)
                    ("0 2 5 7 9 12 14 17 19 21"::Pattern Int)
            it "can transform notes correctly over 2 octaves - yu" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "yu" twoOctavesOf5NoteScale)
                    ("0 3 5 7 10 12 15 17 19 22"::Pattern Int)
        describe "6 note scales" $ do
            let twoOctavesOf6NoteScale = "0 1 2 3 4 5 6 7 8 9 10 11"
            it "can transform notes correctly over 2 octaves - whole" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "whole" twoOctavesOf6NoteScale)
                    ("0 2 4 6 8 10 12 14 16 18 20 22"::Pattern Int)
            it "can transform notes correctly over 2 octaves - wholetone" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "wholetone" twoOctavesOf6NoteScale)
                    (Sound.Tidal.Scales.scale "whole" twoOctavesOf6NoteScale :: Pattern Int)
            it "can transform notes correctly over 2 octaves - augmented" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "augmented" twoOctavesOf6NoteScale)
                    ("0 3 4 7 8 11 12 15 16 19 20 23"::Pattern Int)
            it "can transform notes correctly over 2 octaves - augmented2" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "augmented2" twoOctavesOf6NoteScale)
                    ("0 1 4 5 8 9 12 13 16 17 20 21"::Pattern Int)
            it "can transform notes correctly over 2 octaves - hexMajor7" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "hexMajor7" twoOctavesOf6NoteScale)
                    ("0 2 4 7 9 11 12 14 16 19 21 23"::Pattern Int)
            it "can transform notes correctly over 2 octaves - hexPhrygian" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "hexPhrygian" twoOctavesOf6NoteScale)
                    ("0 1 3 5 8 10 12 13 15 17 20 22"::Pattern Int)
            it "can transform notes correctly over 2 octaves - hexDorian" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "hexDorian" twoOctavesOf6NoteScale)
                    ("0 2 3 5 7 10 12 14 15 17 19 22"::Pattern Int)
            it "can transform notes correctly over 2 octaves - hexSus" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "hexSus" twoOctavesOf6NoteScale)
                    ("0 2 5 7 9 10 12 14 17 19 21 22"::Pattern Int)
            it "can transform notes correctly over 2 octaves - hexMajor6" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "hexMajor6" twoOctavesOf6NoteScale)
                    ("0 2 4 5 7 9 12 14 16 17 19 21"::Pattern Int)
            it "can transform notes correctly over 2 octaves - hexAeolian" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "hexAeolian" twoOctavesOf6NoteScale)
                    ("0 3 5 7 8 10 12 15 17 19 20 22"::Pattern Int)
        describe "7 note scales" $ do
            let twoOctavesOf7NoteScale = "0 1 2 3 4 5 6 7 8 9 10 11 12 13"
            it "can transform notes correctly over 2 octaves - major" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "major" twoOctavesOf7NoteScale)
                    ("0 2 4 5 7 9 11 12 14 16 17 19 21 23"::Pattern Int)
            it "can transform notes correctly over 2 octaves - ionian" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "ionian" twoOctavesOf7NoteScale)
                    (Sound.Tidal.Scales.scale "major" twoOctavesOf7NoteScale :: Pattern Int)
            it "can transform notes correctly over 2 octaves - dorian" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "dorian" twoOctavesOf7NoteScale)
                    ("0 2 3 5 7 9 10 12 14 15 17 19 21 22"::Pattern Int)
            it "can transform notes correctly over 2 octaves - aeolian" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "aeolian" twoOctavesOf7NoteScale)
                    ("0 2 3 5 7 8 10 12 14 15 17 19 20 22"::Pattern Int)
            it "can transform notes correctly over 2 octaves - aeolian" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "minor" twoOctavesOf7NoteScale)
                    (Sound.Tidal.Scales.scale "aeolian" twoOctavesOf7NoteScale::Pattern Int)
            it "can transform notes correctly over 2 octaves - minor" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "minor" twoOctavesOf7NoteScale)
                    (Sound.Tidal.Scales.scale "aeolian" twoOctavesOf7NoteScale::Pattern Int)
            it "can transform notes correctly over 2 octaves - locrian" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "locrian" twoOctavesOf7NoteScale)
                    ("0 1 3 5 6 8 10 12 13 15 17 18 20 22"::Pattern Int)
            it "can transform notes correctly over 2 octaves - harmonicMinor" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "harmonicMinor" twoOctavesOf7NoteScale)
                    ("0 2 3 5 7 8 11 12 14 15 17 19 20 23"::Pattern Int)
            it "can transform notes correctly over 2 octaves - harmonicMajor" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "harmonicMajor" twoOctavesOf7NoteScale)
                    ("0 2 4 5 7 8 11 12 14 16 17 19 20 23"::Pattern Int)
            it "can transform notes correctly over 2 octaves - melodicMinor" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "melodicMinor" twoOctavesOf7NoteScale)
                    ("0 2 3 5 7 9 11 12 14 15 17 19 21 23"::Pattern Int)
            it "can transform notes correctly over 2 octaves - melodicMinorDesc" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "melodicMinorDesc" twoOctavesOf7NoteScale)
                    (Sound.Tidal.Scales.scale "minor" twoOctavesOf7NoteScale::Pattern Int)
            it "can transform notes correctly over 2 octaves - melodicMajor" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "melodicMajor" twoOctavesOf7NoteScale)
                    ("0 2 4 5 7 8 10 12 14 16 17 19 20 22"::Pattern Int)
            it "can transform notes correctly over 2 octaves - bartok" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "bartok" twoOctavesOf7NoteScale)
                    (Sound.Tidal.Scales.scale "melodicMajor" twoOctavesOf7NoteScale::Pattern Int)
            it "can transform notes correctly over 2 octaves - hindu" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "hindu" twoOctavesOf7NoteScale)
                    (Sound.Tidal.Scales.scale "melodicMajor" twoOctavesOf7NoteScale::Pattern Int)
            it "can transform notes correctly over 2 octaves - todi" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "todi" twoOctavesOf7NoteScale)
                    ("0 1 3 6 7 8 11 12 13 15 18 19 20 23"::Pattern Int)
            it "can transform notes correctly over 2 octaves - purvi" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "purvi" twoOctavesOf7NoteScale)
                    ("0 1 4 6 7 8 11 12 13 16 18 19 20 23"::Pattern Int)
            it "can transform notes correctly over 2 octaves - marva" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "marva" twoOctavesOf7NoteScale)
                    ("0 1 4 6 7 9 11 12 13 16 18 19 21 23"::Pattern Int)
            it "can transform notes correctly over 2 octaves - bhairav" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "bhairav" twoOctavesOf7NoteScale)
                    ("0 1 4 5 7 8 11 12 13 16 17 19 20 23"::Pattern Int)
            it "can transform notes correctly over 2 octaves - ahirbhairav" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "ahirbhairav" twoOctavesOf7NoteScale)
                    ("0 1 4 5 7 9 10 12 13 16 17 19 21 22"::Pattern Int)
            it "can transform notes correctly over 2 octaves - superLocrian" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "superLocrian" twoOctavesOf7NoteScale)
                    ("0 1 3 4 6 8 10 12 13 15 16 18 20 22"::Pattern Int)
            it "can transform notes correctly over 2 octaves - romanianMinor" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "romanianMinor" twoOctavesOf7NoteScale)
                    ("0 2 3 6 7 9 10 12 14 15 18 19 21 22"::Pattern Int)
            it "can transform notes correctly over 2 octaves - hungarianMinor" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "hungarianMinor" twoOctavesOf7NoteScale)
                    ("0 2 3 6 7 8 11 12 14 15 18 19 20 23"::Pattern Int)
            it "can transform notes correctly over 2 octaves - neapolitanMinor" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "neapolitanMinor" twoOctavesOf7NoteScale)
                    ("0 1 3 5 7 8 11 12 13 15 17 19 20 23"::Pattern Int)
            it "can transform notes correctly over 2 octaves - enigmatic" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "enigmatic" twoOctavesOf7NoteScale)
                    ("0 1 4 6 8 10 11 12 13 16 18 20 22 23"::Pattern Int)
            it "can transform notes correctly over 2 octaves - spanish" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "spanish" twoOctavesOf7NoteScale)
                    ("0 1 4 5 7 8 10 12 13 16 17 19 20 22"::Pattern Int)
            it "can transform notes correctly over 2 octaves - leadingWhole" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "leadingWhole" twoOctavesOf7NoteScale)
                    ("0 2 4 6 8 10 11 12 14 16 18 20 22 23"::Pattern Int)
            it "can transform notes correctly over 2 octaves - lydianMinor" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "lydianMinor" twoOctavesOf7NoteScale)
                    ("0 2 4 6 7 8 10 12 14 16 18 19 20 22"::Pattern Int)
            it "can transform notes correctly over 2 octaves - neapolitanMajor" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "neapolitanMajor" twoOctavesOf7NoteScale)
                    ("0 1 3 5 7 9 11 12 13 15 17 19 21 23"::Pattern Int)
            it "can transform notes correctly over 2 octaves - locrianMajor" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "locrianMajor" twoOctavesOf7NoteScale)
                    ("0 2 4 5 6 8 10 12 14 16 17 18 20 22"::Pattern Int)
        describe "8 note scales" $ do
            let twoOctavesOf8NoteScale = "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15"
            it "can transform notes correctly over 2 octaves - diminished" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "diminished" twoOctavesOf8NoteScale)
                    ("0 1 3 4 6 7 9 10 12 13 15 16 18 19 21 22"::Pattern Int)
            it "can transform notes correctly over 2 octaves - octatonic" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "octatonic" twoOctavesOf8NoteScale)
                    (Sound.Tidal.Scales.scale "diminished" twoOctavesOf8NoteScale::Pattern Int)
            it "can transform notes correctly over 2 octaves - diminished2" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "diminished2" twoOctavesOf8NoteScale)
                    ("0 2 3 5 6 8 9 11 12 14 15 17 18 20 21 23"::Pattern Int)
            it "can transform notes correctly over 2 octaves - octatonic2" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "octatonic2" twoOctavesOf8NoteScale)
                    (Sound.Tidal.Scales.scale "diminished2" twoOctavesOf8NoteScale::Pattern Int)
        describe "modes of limited transposition" $ do
            let twoOctavesOf6NoteScale = "0 1 2 3 4 5 6 7 8 9 10 11"
            let twoOctavesOf8NoteScale = "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15"
            let twoOctavesOf9NoteScale = "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17"
            let twoOctavesOf10NoteScale = "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19"
            it "can transform notes correctly over 2 octaves - messiaen1" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "messiaen1" twoOctavesOf6NoteScale)
                    (Sound.Tidal.Scales.scale "wholetone" twoOctavesOf6NoteScale::Pattern Int)
            it "can transform notes correctly over 2 octaves - messiaen2" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "messiaen2" twoOctavesOf8NoteScale)
                    (Sound.Tidal.Scales.scale "diminished" twoOctavesOf8NoteScale::Pattern Int)
            it "can transform notes correctly over 2 octaves - messiaen3" $ do
                -- tone, semitone, semitone, tone, semitone, semitone, tone, semitone, semitone
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "messiaen3" twoOctavesOf9NoteScale)
                    ("0 2 3 4 6 7 8 10 11 12 14 15 16 18 19 20 22 23"::Pattern Int)
            it "can transform notes correctly over 2 octaves - messiaen4" $ do
                -- semitone, semitone, minor third, semitone, semitone, semitone, minor third, semitone
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "messiaen4" twoOctavesOf8NoteScale)
                    ("0 1 2 5 6 7 8 11 12 13 14 17 18 19 20 23"::Pattern Int)
            it "can transform notes correctly over 2 octaves - messiaen5" $ do
                -- semitone, major third, semitone, semitone, major third, semitone
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "messiaen5" twoOctavesOf6NoteScale)
                    ("0 1 5 6 7 11 12 13 17 18 19 23"::Pattern Int)
            it "can transform notes correctly over 2 octaves - messiaen6" $ do
                -- tone, tone, semitone, semitone, tone, tone, semitone, semitone
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "messiaen6" twoOctavesOf8NoteScale)
                    ("0 2 4 5 6 8 10 11 12 14 16 17 18 20 22 23"::Pattern Int)
            it "can transform notes correctly over 2 octaves - messiaen7" $ do
                -- semitone, semitone, semitone, tone, semitone, semitone, semitone, semitone, tone, semitone
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "messiaen7" twoOctavesOf10NoteScale)
                    ("0 1 2 3 5 6 7 8 9 11 12 13 14 15 17 18 19 20 21 23"::Pattern Int)
        describe "12 note scales" $ do
            let twoOctavesOf12NoteScale = "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23"
            it "can transform notes correctly over 2 octaves - chromatic" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "chromatic" twoOctavesOf12NoteScale)
                    (twoOctavesOf12NoteScale::Pattern Int)
        describe "edge cases" $ do
            it "responds to unknown scales by mapping to octaves" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "ergaerv" "0 1 2 3 4")
                    ("0 12 24 36 48"::Pattern Int)
            it "correctly maps negative numbers" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "major" "0 -1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11 -12 -13")
                    ("0 -1 -3 -5 -7 -8 -10 -12 -13 -15 -17 -19 -20 -22 ":: Pattern Int)
        describe "chord names" $ do
            let twoOctavesOfTwoNoteChord = "0 1 2 3"
            let twoOctavesOfThreeNoteChord = "0 1 2 3 4 5"
            let twoOctavesOfFourNoteChord = "0 1 2 3 4 5 6 7"
            let twoOctavesOfFiveNoteChord = "0 1 2 3 4 5 6 7 8 9"
            let twoOctavesOfSixNoteChord = "0 1 2 3 4 5 6 7 8 9 10 11"
            let twoOctavesOfSevenNoteChord = "0 1 2 3 4 5 6 7 8 9 10 11 12 13"
            it "can use maj" $ do 
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "maj" twoOctavesOfThreeNoteChord)
                    ("0 4 7 12 16 19"::Pattern Int)
            it "can use min" $ do 
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "min" twoOctavesOfThreeNoteChord)
                    ("0 3 7 12 15 19"::Pattern Int)
            it "can use maj7" $ do 
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "maj7" twoOctavesOfFourNoteChord)
                    ("0 4 7 11 12 16 19 23"::Pattern Int)
            it "can use major7" $ do 
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "major7" twoOctavesOfFourNoteChord)
                    (Sound.Tidal.Scales.scale "maj7" twoOctavesOfFourNoteChord::Pattern Int)
            it "can use min7" $ do 
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "min7" twoOctavesOfFourNoteChord)
                    ("0 3 7 10 12 15 19 22"::Pattern Int)
            it "can use minor7" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "minor7" twoOctavesOfFourNoteChord)
                    (Sound.Tidal.Scales.scale "min7" twoOctavesOfFourNoteChord::Pattern Int)
            it "can use dom7" $ do 
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "dom7" twoOctavesOfFourNoteChord)
                    ("0 4 7 10 12 16 19 22"::Pattern Int)
            it "can use aug" $ do 
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "aug" twoOctavesOfThreeNoteChord)
                    ("0 4 8 12 16 20"::Pattern Int)
            it "can use dim" $ do 
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "dim" twoOctavesOfThreeNoteChord)
                    ("0 3 6 12 15 18"::Pattern Int)
            it "can use dim7" $ do 
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "dim7" twoOctavesOfFourNoteChord)
                    ("0 3 6 9 12 15 18 21"::Pattern Int)
            it "can use one" $ do 
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "one" "0 1 2")
                    ("0 12 24"::Pattern Int)
            it "can use 1" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "1" "0 1 2")
                    (Sound.Tidal.Scales.scale "one" "0 1 2"::Pattern Int)
            it "can use five" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "five" twoOctavesOfTwoNoteChord)
                    ("0 7 12 19"::Pattern Int)
            it "can use plus" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "plus" twoOctavesOfThreeNoteChord)
                    (Sound.Tidal.Scales.scale "aug" twoOctavesOfThreeNoteChord::Pattern Int)
            it "can use sharp5" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "sharp5" twoOctavesOfThreeNoteChord)
                    (Sound.Tidal.Scales.scale "aug" twoOctavesOfThreeNoteChord::Pattern Int)
            it "can use msharp5" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "msharp5" twoOctavesOfThreeNoteChord)
                    ("0 3 8 12 15 20"::Pattern Int)
            it "can use sus2" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "sus2" twoOctavesOfThreeNoteChord)
                    ("0 2 7 12 14 19"::Pattern Int)
            it "can use sus4" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "sus4" twoOctavesOfThreeNoteChord)
                    ("0 5 7 12 17 19"::Pattern Int)
            it "can use six" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "six" twoOctavesOfFourNoteChord)
                    ("0 4 7 9 12 16 19 21"::Pattern Int)
            it "can use m6" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "m6" twoOctavesOfFourNoteChord)
                    ("0 3 7 9 12 15 19 21"::Pattern Int)
            it "can use sevenSus2" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "sevenSus2" twoOctavesOfFourNoteChord)
                    ("0 2 7 10 12 14 19 22"::Pattern Int)
            it "can use sevenSus4" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "sevenSus4" twoOctavesOfFourNoteChord)
                    ("0 5 7 10 12 17 19 22"::Pattern Int)
            it "can use sevenSus4" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "sevenSus4" twoOctavesOfFourNoteChord)
                    ("0 5 7 10 12 17 19 22"::Pattern Int)
            it "can use sevenFlat5" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "sevenFlat5" twoOctavesOfFourNoteChord)
                    ("0 4 6 10 12 16 18 22"::Pattern Int)
            it "can use m7flat5" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "m7flat5" twoOctavesOfFourNoteChord)
                    ("0 3 6 10 12 15 18 22"::Pattern Int)
            it "can use sevenSharp5" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "sevenSharp5" twoOctavesOfFourNoteChord)
                    ("0 4 8 10 12 16 20 22"::Pattern Int)
            it "can use m7sharp5" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "m7sharp5" twoOctavesOfFourNoteChord)
                    ("0 3 8 10 12 15 20 22"::Pattern Int)
            it "can use nine" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "nine" twoOctavesOfFiveNoteChord)
                    ("0 2 4 7 10 12 14 16 19 22"::Pattern Int)
            it "can use m9" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "m9" twoOctavesOfFiveNoteChord)
                    ("0 2 3 7 10 12 14 15 19 22"::Pattern Int)
            it "can use m7sharp9" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "m7sharp9" twoOctavesOfFiveNoteChord)
                    (Sound.Tidal.Scales.scale "m9" twoOctavesOfFiveNoteChord::Pattern Int)
            it "can use maj9" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "maj9" twoOctavesOfFiveNoteChord)
                    ("0 2 4 7 11 12 14 16 19 23"::Pattern Int)
            it "can use nineSus4" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "nineSus4" twoOctavesOfFiveNoteChord)
                    ("0 2 5 7 10 12 14 17 19 22"::Pattern Int)
            it "can use sixby9" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "sixby9" twoOctavesOfFiveNoteChord)
                    ("0 2 4 7 9 12 14 16 19 21"::Pattern Int)
            it "can use m6by9" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "m6by9" twoOctavesOfFiveNoteChord)
                    ("0 2 3 7 9 12 14 15 19 21"::Pattern Int)
            it "can use sevenFlat9" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "sevenFlat9" twoOctavesOfFiveNoteChord)
                    ("0 1 4 7 10 12 13 16 19 22"::Pattern Int)
            it "can use m7flat9" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "m7flat9" twoOctavesOfFiveNoteChord)
                    ("0 1 3 7 10 12 13 15 19 22"::Pattern Int)
            it "can use sevenFlat10" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "sevenFlat10" twoOctavesOfFiveNoteChord)
                    ("0 3 4 7 10 12 15 16 19 22"::Pattern Int)
            it "can use nineSharp5" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "nineSharp5" twoOctavesOfTwoNoteChord)
                    ("0 1 12 13"::Pattern Int)
            it "can use m9sharp5" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "m9sharp5" twoOctavesOfThreeNoteChord)
                    ("0 1 2 12 13 14"::Pattern Int)
            it "can use sevenSharp5flat9" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "sevenSharp5flat9" twoOctavesOfFiveNoteChord)
                    ("0 1 4 8 10 12 13 16 20 22"::Pattern Int)
            it "can use m7sharp5flat9" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "m7sharp5flat9" twoOctavesOfFiveNoteChord)
                    ("0 1 3 8 10 12 13 15 20 22"::Pattern Int)
            it "can use eleven" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "eleven" twoOctavesOfSixNoteChord)
                    ("0 2 4 5 7 10 12 14 16 17 19 22"::Pattern Int)
            it "can use 11" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "eleven" twoOctavesOfSixNoteChord)
                    (Sound.Tidal.Scales.scale "11" twoOctavesOfSixNoteChord::Pattern Int)
            it "can use m11" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "m11" twoOctavesOfSixNoteChord)
                    ("0 2 3 5 7 10 12 14 15 17 19 22"::Pattern Int)
            it "can use maj11" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "maj11" twoOctavesOfSixNoteChord)
                    ("0 2 4 5 7 11 12 14 16 17 19 23"::Pattern Int)
            it "can use elevenSharp" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "elevenSharp" twoOctavesOfSixNoteChord)
                    ("0 2 4 6 7 10 12 14 16 18 19 22"::Pattern Int)
            it "can use sharp11" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "sharp11" twoOctavesOfSixNoteChord)
                    (Sound.Tidal.Scales.scale "elevenSharp" twoOctavesOfSixNoteChord::Pattern Int)
            it "can use m11sharp" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "m11sharp" twoOctavesOfSixNoteChord)
                    ("0 2 3 6 7 10 12 14 15 18 19 22"::Pattern Int)
            it "can use thirteen" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "thirteen" twoOctavesOfSevenNoteChord)
                    ("0 2 4 5 7 9 10 12 14 16 17 19 21 22"::Pattern Int)
            it "can use m13" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "m13" twoOctavesOfSevenNoteChord)
                    ("0 2 3 5 7 9 10 12 14 15 17 19 21 22"::Pattern Int)    
            




           