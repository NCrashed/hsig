-- | Слушатель: сходимость сюрприза к энтропийной скорости.
--
-- Приёмочный тест этапа M4 (docs/PRED.md, разд. 5 и 8): на golden mean
-- слушатель первого порядка выходит ровно на h_mu, на even застревает выше
-- на вычислимую величину, и избыток падает с ростом порядка.
module ListenerSpec (tests) where

import Sound.Pred.Dist
import Sound.Pred.Kernel
import Sound.Pred.Listener
import Sound.Pred.Machine
import Sound.Pred.Model
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Listener"
    [ basicTests
    , convergenceTests
    , shortTermTests
    ]

-- | Длина потока для оценок. Стандартная ошибка среднего сюрприза при
-- такой длине около 0.01 бита, отсюда допуски ниже.
streamLen :: Int
streamLen = 40000

-- | Истинная энтропийная скорость обеих машин при p = 1/2.
hTrue :: Double
hTrue = 2 / 3

-- | Оценка слушателя первого порядка на even: (1/3)*H(1/2) + (2/3)*H(3/4).
hEvenOrder1 :: Double
hEvenOrder1 = (1 / 3) * 1 + (2 / 3) * hBits (3 / 4)
  where
    hBits q = negate (q * logBase 2 q + (1 - q) * logBase 2 (1 - q))

streamOf :: Machine TwoState Int -> Int -> [Int]
streamOf m seed = take streamLen (generateSeeded seed (toPred m))

-- | Средний сюрприз по второй половине потока: без переходного участка.
rateOf :: Int -> [Int] -> Double
rateOf k xs = tailMean 0.5 (onlineSurprisals (newListener k [0, 1]) xs)

basicTests :: TestTree
basicTests =
  testGroup
    "основы"
    [ testCase "пустой слушатель предсказывает равномерно" $ do
        distPairs (predictNext (newListener 2 [0 :: Int, 1])) @?= [(0, 0.5), (1, 0.5)]
    , testCase "неслыханный символ имеет конечный сюрприз" $ do
        let l = trainOn (newListener 2 [0 :: Int, 1]) (replicate 100 0)
        assertBool "бесконечный сюрприз" (not (isInfinite (surprisalOf (predictNext l) 1)))
    , testCase "повторение снижает сюрприз" $ do
        case onlineSurprisals (newListener 2 [0 :: Int, 1]) (replicate 50 0) of
          [] -> assertFailure "пустой список сюрпризов"
          (s0 : rest) ->
            assertBool ("первый " <> show s0 <> ", последний " <> show (last rest)) (last rest < s0)
    , testCase "история ограничена порядком" $ do
        length (listenerHist (trainOn (newListener 3 [0 :: Int, 1]) (replicate 20 0))) @?= 3
    , testCase "предсказание нормировано" $ do
        let l = trainOn (newListener 4 [0 :: Int, 1]) (take 500 (generateSeeded 2 (toPred (goldenMean 0.5))))
        assertBool "не единица" (abs (sum (map snd (distPairs (predictNext l))) - 1) < 1e-9)
    ]

convergenceTests :: TestTree
convergenceTests =
  testGroup
    "сходимость"
    [ -- Golden mean это цепь порядка один, значит слушатель порядка один
      -- обязан выйти на истинную энтропийную скорость, а не приблизиться.
      testCase "golden mean: порядок 1 выходит на h_mu = 2/3" $ do
        let r = rateOf 1 (streamOf (goldenMean 0.5) 101)
        assertBool ("оценка " <> show r <> ", истина " <> show hTrue) (abs (r - hTrue) < 0.02)
    , testCase "golden mean: больший порядок не портит" $ do
        let r = rateOf 6 (streamOf (goldenMean 0.5) 102)
        assertBool ("оценка " <> show r) (abs (r - hTrue) < 0.03)
    , -- У even марковского порядка нет, и слушатель порядка один даёт
      -- вычислимую наперёд оценку, отличную от истины.
      testCase "even: порядок 1 даёт 0.874 вместо 2/3" $ do
        let r = rateOf 1 (streamOf (evenProcess 0.5) 103)
        assertBool ("оценка " <> show r <> ", предсказано " <> show hEvenOrder1) (abs (r - hEvenOrder1) < 0.03)
    , testCase "even: избыток над h_mu строго положителен при порядке 1" $ do
        let r = rateOf 1 (streamOf (evenProcess 0.5) 104)
        assertBool ("избыток " <> show (r - hTrue)) (r - hTrue > 0.15)
    , -- Пробеги единиц длиннее порядка редки, поэтому избыток падает, а не
      -- держится. Проверяем именно падение, а не мифическую несходимость.
      testCase "even: избыток падает с ростом порядка" $ do
        let xs = streamOf (evenProcess 0.5) 105
            r1 = rateOf 1 xs
            r4 = rateOf 4 xs
            r8 = rateOf 8 xs
        assertBool ("r1=" <> show r1 <> " r4=" <> show r4) (r4 < r1 - 0.1)
        assertBool ("r4=" <> show r4 <> " r8=" <> show r8) (r8 <= r4 + 0.02)
        assertBool ("r8=" <> show r8 <> ", h_mu=" <> show hTrue) (abs (r8 - hTrue) < 0.05)
    , -- Разделяющий тест: при одинаковых h_mu и C_mu процессы для
      -- слушателя конечного порядка совершенно разные.
      testCase "процессы с одними инвариантами различаются на слух" $ do
        let g = rateOf 1 (streamOf (goldenMean 0.5) 106)
            e = rateOf 1 (streamOf (evenProcess 0.5) 107)
        assertBool ("golden " <> show g <> ", even " <> show e) (e - g > 0.15)
    ]

-- | Латентный режим блоками: то одно распределение, то другое.
--
-- Локальная структура тут есть, но контекста хватает, чтобы её опознать:
-- по последним четырём символам режим виден. Это случай, где краткосрочная
-- не нужна.
blocked :: Int -> [Char]
blocked seed = generateSeeded seed (nest 16 upper leaf)
  where
    upper = constPred (uniform [0 :: Int, 1])
    leaf i = constPred (dist [('a', if i == 0 then 0.9 else 0.1), ('b', if i == 0 then 0.1 else 0.9)])

-- | Свежий мотив в каждой фразе, повторённый четыре раза.
--
-- Вот случай, ради которого краткосрочная существует. Мотивы разные и
-- по всей пьесе не повторяются, поэтому долговременная статистика их
-- размазывает: одному и тому же контексту в разных фразах отвечают разные
-- продолжения. Краткосрочная же слышит мотив второй раз в той же фразе и
-- дальше знает его наизусть.
motifPhrases :: Int -> Int -> [String]
motifPhrases seed n = [concat (replicate 4 (motif i)) | i <- [0 .. n - 1]]
  where
    motif i = [alpha !! floor (4 * u) | u <- take 4 (drop (4 * i) (uniformsFrom seed))]
    alpha = "abcd"

shortTermTests :: TestTree
shortTermTests =
  testGroup
    "краткосрочная память"
    [ testCase "по умолчанию её нет" $ do
        assertBool "появилась без спроса" (not (hasShortTerm (newListener 3 "ab")))
    , testCase "включается явно" $ do
        assertBool "не включилась" (hasShortTerm (newListenerWith 3 2 "ab"))
    , -- Замкнутые числа приёмочных проверок относятся к чистой цепи, и
      -- добавление второй компоненты не должно их трогать.
      testCase "без краткосрочной предсказание не меняется" $ do
        let xs = take 400 (blocked 41)
            l = trainOn (newListener 3 "ab") xs
        distPairs (predictNext l) @?= distPairs (predictNext (trainOn (newListener 3 "ab") xs))
    , testCase "граница без краткосрочной это тождество" $ do
        let l = trainOn (newListener 3 "ab") (take 200 (blocked 42))
        distPairs (predictNext (boundary l)) @?= distPairs (predictNext l)
    , -- После границы краткосрочная видит только новое, и предсказание
      -- уезжает туда сильнее, чем у одной долговременной на тех же данных.
      -- Это и есть локальная адаптация в чистом виде.
      testCase "после границы краткосрочная ведёт к свежему материалу" $ do
        let base = trainOn (newListenerWith 3 2 "ab") (replicate 40 'a')
            fresh = trainOn (boundary base) (replicate 6 'b')
            longOnly = trainOn (newListener 3 "ab") (replicate 40 'a' <> replicate 6 'b')
        assertBool
          ( "с краткой p(b) = "
              <> show (probOf 'b' (predictNext fresh))
              <> ", только долгая "
              <> show (probOf 'b' (predictNext longOnly))
          )
          (probOf 'b' (predictNext fresh) > probOf 'b' (predictNext longOnly))
    , -- Носитель обязан оставаться полным: символ с нулевой вероятностью
      -- стоит бесконечного сюрприза и отравляет все средние.
      testCase "сведение сохраняет полный носитель" $ do
        let l = trainOn (newListenerWith 4 3 "ab") (replicate 200 'a')
        support (predictNext l) @?= "ab"
        assertBool "нулевая вероятность" (probOf 'b' (predictNext l) > 0)
        assertBool "бесконечный сюрприз" (not (isInfinite (surprisalOf (predictNext l) 'b')))
    , -- Ради этого числа всё и делалось: свежий мотив, повторённый внутри
      -- фразы, долговременной памятью не берётся, а краткосрочной берётся
      -- со второго проведения.
      --
      -- Порядок величины стоит понимать. Первые четыре события фразы это
      -- новый мотив, они неустранимо стоят по два бита; остальные двенадцать
      -- краткосрочная берёт почти даром. Идеальный предел около 0.5 бита,
      -- одна долговременная даёт 1.09, вместе выходит 0.87. То есть снимается
      -- пятая часть сюрприза, а не половина: остаток съедает то, что
      -- уверенность краткосрочной внутри фразы растёт не мгновенно.
      testCase "на свежих мотивах краткосрочная снимает пятую часть сюрприза" $ do
        let segs = motifPhrases 91 400
            long = tailMean 0.5 (onlineSurprisals (newListener 4 "abcd") (concat segs))
            both = tailMean 0.5 (onlineSurprisalsSeg (newListenerWith 4 3 "abcd") segs)
        assertBool
          ("только долгая " <> show long <> ", с краткой " <> show both)
          (both < long * 0.85)
    , -- Граница обязана стоять там, где мотив меняется. Та же пьеса с
      -- границами не по фразам теряет весь выигрыш: это прямо показывает,
      -- что рычаг композитора - именно разбиение.
      testCase "смещённые границы съедают выигрыш" $ do
        let segs = motifPhrases 91 400
            aligned = tailMean 0.5 (onlineSurprisalsSeg (newListenerWith 4 3 "abcd") segs)
            shifted = tailMean 0.5 (onlineSurprisalsSeg (newListenerWith 4 3 "abcd") (chunk 7 (concat segs)))
        assertBool
          ("по фразам " <> show aligned <> ", вразрез " <> show shifted)
          (shifted > aligned + 0.2)
    , -- Обратная сторона, и её надо знать. Если контекста хватает, чтобы
      -- опознать локальный режим, долговременная справляется сама, а
      -- полупустая краткосрочная только тянет к равномерному.
      testCase "там, где хватает контекста, краткосрочная мешает" $ do
        let xs = take 8000 (blocked 43)
            long = tailMean 0.5 (onlineSurprisals (newListener 4 "ab") xs)
            both = tailMean 0.5 (onlineSurprisalsSeg (newListenerWith 4 3 "ab") (chunk 16 xs))
        assertBool
          ("только долгая " <> show long <> ", с краткой " <> show both)
          (both > long)
    , -- Контроль: на процессе без всякой локальной структуры выигрыша быть
      -- не должно, иначе выигрыш выше объясняется не тем, чем заявлено.
      testCase "на стационарном процессе выигрыша нет" $ do
        let xs = take 8000 (generateSeeded 44 (constPred (dist [('a', 0.7), ('b', 0.3)])))
            long = tailMean 0.5 (onlineSurprisals (newListener 4 "ab") xs)
            both = tailMean 0.5 (onlineSurprisalsSeg (newListenerWith 4 3 "ab") (chunk 16 xs))
        assertBool
          ("только долгая " <> show long <> ", с краткой " <> show both)
          (both > long - 0.02)
    , testCase "сегментированный прогон даёт сюрприз на каждый символ" $ do
        let xs = take 96 (blocked 45)
        length (onlineSurprisalsSeg (newListenerWith 3 2 "ab") (chunk 16 xs)) @?= length xs
    , testCase "обучение по фразам эквивалентно ручным границам" $ do
        let segs = chunk 16 (take 96 (blocked 46))
            byFold = trainSegmented (newListenerWith 3 2 "ab") segs
            byHand = foldl (\l s -> trainOn (boundary l) s) (newListenerWith 3 2 "ab") segs
        distPairs (predictNext byFold) @?= distPairs (predictNext byHand)
    ]
  where
    chunk k xs
      | null xs = []
      | otherwise = take k xs : chunk k (drop k xs)
