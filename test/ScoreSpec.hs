-- | Алгебра паттернов: семантика Tidal.
module ScoreSpec (tests) where

import Data.List (nub, sort, sortOn)
import Data.Ratio ((%))
import Sound.Sig.Score
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Score"
    [ pureTests
    , timeTests
    , combineTests
    , everyTests
    , degradeTests
    , lawTests
    ]

-- | Событие: целый отрезок, видимая часть, значение.
ev :: (Time, Time) -> (Time, Time) -> a -> Event a
ev (ws, we) (ps, pe) = Event (Just (Arc ws we)) (Arc ps pe)

-- | Запрос по отрезку.
q :: Pattern a -> (Time, Time) -> [Event a]
q p (s, e) = queryArc p (Arc s e)

pureTests :: TestTree
pureTests =
  testGroup
    "pure"
    [ -- Одно событие на цикл, целый отрезок это сам цикл.
      testCase "цикл целиком" $ do
        q (pure 'a') (0, 1) @?= [ev (0, 1) (0, 1) 'a']
    , testCase "два цикла дают два события" $ do
        q (pure 'a') (0, 2) @?= [ev (0, 1) (0, 1) 'a', ev (1, 2) (1, 2) 'a']
    , -- Часть обрезается запросом, целое остаётся циклом: по нему видно,
      -- где нота началась и сколько длится.
      testCase "кусок цикла обрезает часть, но не целое" $ do
        q (pure 'a') (1 % 4, 3 % 4) @?= [ev (0, 1) (1 % 4, 3 % 4) 'a']
    , testCase "запрос со сдвигом попадает в свой цикл" $ do
        q (pure 'a') (3 % 2, 7 % 4) @?= [ev (1, 2) (3 % 2, 7 % 4) 'a']
    , testCase "пустой запрос ничего не даёт" $ do
        q (pure 'a') (1, 1) @?= []
    , testCase "silence всегда пуст" $ do
        q (silence :: Pattern Char) (0, 4) @?= []
    ]

timeTests :: TestTree
timeTests =
  testGroup
    "время"
    [ testCase "fast 2 укладывает два события в цикл" $ do
        q (fast 2 (pure 'a')) (0, 1)
          @?= [ev (0, 1 % 2) (0, 1 % 2) 'a', ev (1 % 2, 1) (1 % 2, 1) 'a']
    , testCase "slow 2 растягивает событие на два цикла" $ do
        q (slow 2 (pure 'a')) (0, 1) @?= [ev (0, 2) (0, 1) 'a']
    , testCase "fast 1 ничего не меняет" $ do
        q (fast 1 (pure 'a')) (0, 2) @?= q (pure 'a') (0, 2)
    , testCase "rotL сдвигает влево" $ do
        q (rotL (1 % 2) (pure 'a')) (0, 1 % 2) @?= [ev (negate (1 % 2), 1 % 2) (0, 1 % 2) 'a']
    , testCase "rotR обратен rotL" $ do
        q (rotR (1 % 4) (rotL (1 % 4) (pure 'a'))) (0, 1) @?= q (pure 'a') (0, 1)
    ]

combineTests :: TestTree
combineTests =
  testGroup
    "склейка"
    [ testCase "stack накладывает" $ do
        q (stack [pure 'a', pure 'b']) (0, 1)
          @?= [ev (0, 1) (0, 1) 'a', ev (0, 1) (0, 1) 'b']
    , -- cat отдаёт по паттерну на цикл, fastcat умещает всё в один цикл.
      testCase "cat берёт по паттерну на цикл" $ do
        q (cat [pure 'a', pure 'b']) (0, 2)
          @?= [ev (0, 1) (0, 1) 'a', ev (1, 2) (1, 2) 'b']
    , testCase "fastcat умещает всё в цикл" $ do
        q (fastcat [pure 'a', pure 'b']) (0, 1)
          @?= [ev (0, 1 % 2) (0, 1 % 2) 'a', ev (1 % 2, 1) (1 % 2, 1) 'b']
    , -- Имена как в Tidal: fromList это по элементу на цикл, listToPat это
      -- весь список в цикле. Перепутать их значит тихо поменять ритм.
      testCase "fromList берёт по элементу на цикл" $ do
        inTime (q (fromList "ab") (0, 2)) @?= "ab"
        inTime (q (fromList "ab") (0, 1)) @?= "a"
    , testCase "listToPat кладёт весь список в цикл" $ do
        q (listToPat "abcd") (0, 1) @?= q (fastcat (map pure "abcd")) (0, 1)
        inTime (q (listToPat "ab") (0, 1)) @?= "ab"
    , -- Порядок в списке событий Tidal не трогает, переворачивается время,
      -- поэтому сравниваем по времени.
      testCase "rev переворачивает цикл" $ do
        inTime (q (rev (listToPat "abcd")) (0, 1)) @?= "dcba"
    , testCase "rev дважды это тождество" $ do
        q (rev (rev (listToPat "abcd"))) (0, 1) @?= q (listToPat "abcd") (0, 1)
    , testCase "rev переворачивает каждый цикл отдельно" $ do
        inTime (q (rev (listToPat "ab")) (0, 2)) @?= "baba"
    ]

-- | Значения событий в порядке времени.
inTime :: [Event a] -> [a]
inTime = map eventValue . sortOn (arcStart . eventPart)

everyTests :: TestTree
everyTests =
  testGroup
    "every"
    [ testCase "срабатывает на нулевом цикле" $ do
        map eventValue (q (every 2 (fast 2) (pure 'a')) (0, 1)) @?= "aa"
    , testCase "пропускает следующий" $ do
        map eventValue (q (every 2 (fast 2) (pure 'a')) (1, 2)) @?= "a"
    , testCase "возвращается через период" $ do
        map eventValue (q (every 2 (fast 2) (pure 'a')) (2, 3)) @?= "aa"
    , testCase "every 1 применяет всегда" $ do
        map eventValue (q (every 1 (fast 2) (pure 'a')) (1, 2)) @?= "aa"
    ]

degradeTests :: TestTree
degradeTests =
  testGroup
    "degradeBy"
    [ testCase "ноль ничего не выкидывает" $ do
        length (q (degradeBy 0 (fast 16 (pure 'a'))) (0, 1)) @?= 16
    , testCase "единица выкидывает всё" $ do
        q (degradeBy 1 (fast 16 (pure 'a'))) (0, 1) @?= []
    , testCase "половина выкидывает примерно половину" $ do
        let n = length (q (degradeBy 0.5 (fast 512 (pure 'a'))) (0, 1))
        assertBool (show n) (n > 200 && n < 312)
    , -- Детерминизм важнее: два рендера трека обязаны совпасть побитово.
      testCase "детерминирован" $ do
        let once = map eventPart (q (degradeBy 0.5 (fast 64 (pure 'a'))) (0, 4))
            twice = map eventPart (q (degradeBy 0.5 (fast 64 (pure 'a'))) (0, 4))
        once @?= twice
    , testCase "разные циклы прорежены по-разному" $ do
        let cycleOf c = map eventPart (q (degradeBy 0.5 (fast 64 (pure 'a'))) (c, c + 1))
        assertBool "циклы совпали" (length (cycleOf 0) /= length (cycleOf 1))
    , -- Решение висит на самой ноте, а не на видимой части: планировщик
      -- спрашивает паттерн поблочно, и от разбиения ничего зависеть не
      -- должно. Ноты тут длиной в четверть цикла, границы нарочно не по
      -- ним.
      testCase "не зависит от разбиения запроса" $ do
        let p = degradeBy 0.5 (slow 4 (fast 16 (pure 'a')))
            wholes = sort . nub . map eventWhole
            atOnce = wholes (q p (0, 1))
            byParts = wholes (q p (0, 1 % 3) <> q p (1 % 3, 2 % 3) <> q p (2 % 3, 1))
        atOnce @?= byParts
    ]

-- Законы -------------------------------------------------------------------

lawTests :: TestTree
lawTests =
  testGroup
    "законы"
    [ testCase "fmap id" $ do
        q (fmap id (listToPat "abc")) (0, 2) @?= q (listToPat "abc") (0, 2)
    , testCase "fmap композиция" $ do
        let f = succ
            g = succ
        q (fmap (f . g) (listToPat "abc")) (0, 1) @?= q (fmap f (fmap g (listToPat "abc"))) (0, 1)
    , -- Аппликатив пересекает структуру обоих аргументов.
      testCase "аппликатив пересекает отрезки" $ do
        let p = (,) <$> listToPat "ab" <*> pure 'x'
        map eventValue (q p (0, 1)) @?= [('a', 'x'), ('b', 'x')]
    , testCase "аппликатив с pure слева это fmap" $ do
        q (pure succ <*> listToPat "abc") (0, 1) @?= q (fmap succ (listToPat "abc")) (0, 1)
    , -- Соединение по Tidal не сжимает вложенный паттерн в отрезок
      -- события, а пересекает их: внутренний спрашивается в абсолютном
      -- времени. Поэтому fast 4 внутри первой половины даёт два события.
      testCase "монада пересекает вложенный паттерн" $ do
        let p = listToPat "ab" >>= \c -> if c == 'a' then fast 4 (pure c) else pure c
        inTime (q p (0, 1)) @?= "aab"
    , testCase "левая единица монады" $ do
        let f c = listToPat [c, succ c]
        q (pure 'a' >>= f) (0, 1) @?= q (f 'a') (0, 1)
    , testCase "правая единица монады" $ do
        q (listToPat "abc" >>= pure) (0, 1) @?= q (listToPat "abc") (0, 1)
    ]
