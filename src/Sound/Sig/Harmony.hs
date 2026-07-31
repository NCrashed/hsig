-- | Таблицы ладов и аккордов.
--
-- Только числа в полутонах и никакой зависимости от 'Sound.Sig.Score.Note':
-- партитура импортирует этот модуль, а не наоборот. Имена и наборы взяты из
-- Tidal, чтобы чужие рецепты переносились дословно.
module Sound.Sig.Harmony
  ( scaleTable
  , chordTable
  , scaleSemitones
  , chordSemitones
  , degreeSemitones
  ) where

-- | Лады: имя и ступени в полутонах от основы.
scaleTable :: [(String, [Int])]
scaleTable =
  [ ("major", [0, 2, 4, 5, 7, 9, 11])
  , ("ionian", [0, 2, 4, 5, 7, 9, 11])
  , ("minor", [0, 2, 3, 5, 7, 8, 10])
  , ("aeolian", [0, 2, 3, 5, 7, 8, 10])
  , ("harmonicMinor", [0, 2, 3, 5, 7, 8, 11])
  , ("melodicMinor", [0, 2, 3, 5, 7, 9, 11])
  , ("dorian", [0, 2, 3, 5, 7, 9, 10])
  , ("phrygian", [0, 1, 3, 5, 7, 8, 10])
  , ("lydian", [0, 2, 4, 6, 7, 9, 11])
  , ("mixolydian", [0, 2, 4, 5, 7, 9, 10])
  , ("locrian", [0, 1, 3, 5, 6, 8, 10])
  , ("majPent", [0, 2, 4, 7, 9])
  , ("minPent", [0, 3, 5, 7, 10])
  , ("blues", [0, 3, 5, 6, 7, 10])
  , ("wholetone", [0, 2, 4, 6, 8, 10])
  , ("chromatic", [0 .. 11])
  ]

-- | Аккорды: имя и интервалы в полутонах от основы.
chordTable :: [(String, [Int])]
chordTable =
  [ ("maj", [0, 4, 7])
  , ("major", [0, 4, 7])
  , ("min", [0, 3, 7])
  , ("minor", [0, 3, 7])
  , ("aug", [0, 4, 8])
  , ("dim", [0, 3, 6])
  , ("dim7", [0, 3, 6, 9])
  , ("maj7", [0, 4, 7, 11])
  , ("dom7", [0, 4, 7, 10])
  , ("min7", [0, 3, 7, 10])
  , ("m7b5", [0, 3, 6, 10])
  , ("six", [0, 4, 7, 9])
  , ("m6", [0, 3, 7, 9])
  , ("sus2", [0, 2, 7])
  , ("sus4", [0, 5, 7])
  , ("add9", [0, 4, 7, 14])
  , ("m9", [0, 3, 7, 10, 14])
  , ("maj9", [0, 4, 7, 11, 14])
  , ("nine", [0, 4, 7, 10, 14])
  , ("five", [0, 7])
  ]

scaleSemitones :: String -> Maybe [Int]
scaleSemitones name = lookup name scaleTable

chordSemitones :: String -> Maybe [Int]
chordSemitones name = lookup name chordTable

-- | Ступень лада в полутоны. Ступени за пределами октавы переносятся:
-- седьмая ступень семиступенного лада это октава, минус первая - предыдущая
-- седьмая. Так же считает Tidal, и именно поэтому мелодию можно двигать по
-- ступеням, не думая о границе октавы.
degreeSemitones :: [Int] -> Int -> Int
degreeSemitones steps d
  | null steps = d
  | otherwise = 12 * octave + steps !! i
  where
    n = length steps
    octave = d `div` n
    i = d `mod` n
