-- | Диаграмма ядра выводится из машины.
--
-- Смысл тестов не в разметке, а в том, что картинка не расходится с
-- машиной: сколько рёбер выше порога, столько и нарисовано, и ведут они
-- туда же, куда ведёт 'machineStep'.
module DiagramSpec (tests) where

import Data.List (isInfixOf, nub)
import Sound.Pred.Diagram
import Sound.Pred.Dist
import Sound.Pred.Kernel
import Sound.Pred.Machine
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Diagram"
    [ mermaidTests
    , dotTests
    , svgTests
    , geometryTests
    ]

showState :: TwoState -> String
showState = show

showSym :: Int -> String
showSym = show

-- | Три состояния с разной остротой выходов: одно ребро заведомо уходит
-- под любой разумный порог.
tri :: Machine Int Int
tri =
  Machine
    { machineStart = 0
    , machineStates = [0, 1, 2]
    , machineOut = \s -> dist [(0, 0.9), (1, 0.09), (2, 0.01 + fromIntegral s * 0)]
    , machineStep = \s x -> (s + x) `mod` 3
    }

edgeLines :: String -> [String]
edgeLines = filter ("-->" `isInfixOf`) . lines

mermaidTests :: TestTree
mermaidTests =
  testGroup
    "mermaid"
    [ testCase "заголовок на месте" $ do
        take 1 (lines (mermaidOf tri show showSym 0)) @?= ["stateDiagram-v2"]
    , testCase "все состояния подписаны" $ do
        let out = mermaidOf tri show showSym 0
        sequence_
          [ assertBool ("нет состояния " <> show s) (("s" <> show s <> ":") `isInfixOf` out)
          | s <- machineStates tri
          ]
    , testCase "старт отмечен" $ do
        assertBool "нет стартовой стрелки" ("[*] --> s0" `isInfixOf` mermaidOf tri show showSym 0)
    , -- Ребро на каждый символ каждого состояния плюс стартовая стрелка.
      testCase "рёбер столько же, сколько переходов выше порога" $ do
        length (edgeLines (mermaidOf tri show showSym 0)) @?= 3 * 3 + 1
    , testCase "порог отсекает хвост" $ do
        length (edgeLines (mermaidOf tri show showSym 0.05)) @?= 3 * 2 + 1
    , testCase "порог выше всего оставляет только старт" $ do
        length (edgeLines (mermaidOf tri show showSym 1.5)) @?= 1
    , -- Главное: стрелка ведёт туда, куда ведёт машина.
      testCase "цель ребра совпадает с machineStep" $ do
        let out = mermaidOf tri show showSym 0
        sequence_
          [ assertBool
            ("нет ребра " <> show s <> " -> " <> show (machineStep tri s x))
            (("s" <> show s <> " --> s" <> show (machineStep tri s x) <> ":") `isInfixOf` out)
          | s <- machineStates tri
          , x <- [0, 1, 2]
          ]
    , testCase "работает на машине с нечисловыми состояниями" $ do
        let out = mermaidOf (evenProcess 0.5) showState showSym 0
        assertBool "нет SA" ("SA" `isInfixOf` out)
        assertBool "нет SB" ("SB" `isInfixOf` out)
    , testCase "вывод детерминирован" $ do
        mermaidOf tri show showSym 0.02 @?= mermaidOf tri show showSym 0.02
    ]

-- | Числа из атрибутов вида @name="12.5"@.
attrs :: String -> String -> [Double]
attrs name src = go src
  where
    key = name <> "=\""
    go s = case breakOn key s of
      Nothing -> []
      Just rest -> case reads (takeWhile (/= '"') rest) of
        [(v, _)] -> v : go rest
        _ -> go rest

breakOn :: String -> String -> Maybe String
breakOn pat s
  | length s < length pat = Nothing
  | pat == take (length pat) s = Just (drop (length pat) s)
  | otherwise = breakOn pat (drop 1 s)

svgTests :: TestTree
svgTests =
  testGroup
    "svg"
    [ testCase "документ обрамлён" $ do
        let out = ringSvg defaultTheme tri (const "") 0.05
        assertBool "нет корня" ("<svg" `isInfixOf` out)
        assertBool "нет закрытия" ("</svg>" `isInfixOf` out)
        assertBool "нет тёмной темы" ("prefers-color-scheme:dark" `isInfixOf` out)
    , testCase "узел на каждое состояние" $ do
        let out = ringSvg defaultTheme tri (const "") 0.05
        sequence_
          [ assertBool ("нет узла s" <> show i) ((">s" <> show i <> "<") `isInfixOf` out)
          | i <- [0 .. 2 :: Int]
          ]
    , -- Наконечник в пользовательских единицах, иначе он множится на
      -- толщину линии и на сильном ребре закрывает узел.
      testCase "наконечник не масштабируется толщиной" $ do
        assertBool
          "markerUnits не задан"
          ("markerUnits=\"userSpaceOnUse\"" `isInfixOf` ringSvg defaultTheme tri (const "") 0.05)
    , testCase "подписи состояний экранируются" $ do
        let out = ringSvg defaultTheme tri (const "a<b&c") 0.05
        assertBool "нет экранирования" ("a&lt;b&amp;c" `isInfixOf` out)
        assertBool "сырой угол просочился" (not (">a<b&" `isInfixOf` out))
    , testCase "порог отсекает рёбра и в svg" $ do
        let dense = length (attrs "stroke-width" (ringSvg defaultTheme tri (const "") 0))
            sparse = length (attrs "stroke-width" (ringSvg defaultTheme tri (const "") 0.5))
        assertBool ("плотно " <> show dense <> ", редко " <> show sparse) (sparse < dense)
    , -- Ширину объявляют ещё корневой элемент и подложка, отсюда +2.
      testCase "след даёт клетку на событие" $ do
        let evs = concat (replicate 4 [0, 1, 2 :: Int])
            out = traceSvg defaultTheme tri 4 3 evs
        length (attrs "width" out) @?= length evs + 2
    , testCase "след пуст без событий" $ do
        length (attrs "width" (traceSvg defaultTheme tri 4 3 [])) @?= 2
    , -- Холст обязан обтягивать содержимое: след короче строки не должен
      -- получать пустоту справа.
      testCase "холст следа обтягивает содержимое" $ do
        let side n = case attrs "width" (traceSvg defaultTheme tri 4 20 (replicate n (0 :: Int))) of
              (v : _) -> v
              [] -> 0
        assertBool "восемь событий шире двенадцати" (side 8 < side 12)
        assertBool "потолок строки не раздувает холст" (side 8 < side 80)
        side 80 @?= side 160 -- две строки той же ширины
    , testCase "вывод детерминирован" $ do
        ringSvg defaultTheme tri (const "x") 0.05 @?= ringSvg defaultTheme tri (const "x") 0.05
    ]

-- | Геометрия проверяется числами, а не глазом: подписи обязаны лежать
-- внутри холста и не садиться на узлы, а встречные дуги - расходиться.
--
-- Пара одинаково сильных встречных переходов это тот самый случай, на
-- котором картинка ломалась: знак изгиба брался от силы ребра, и обе дуги
-- ложились одна на другую.
geometryTests :: TestTree
geometryTests =
  testGroup
    "геометрия"
    [ testCase "всё внутри холста" $ do
        let out = ringSvg defaultTheme mirror (const "0 4 7") 0.05
            xs = attrs "x" out <> attrs "cx" out
            ys = attrs "y" out <> attrs "cy" out
        -- Первый width это подложка, то есть сторона холста.
        case attrs "width" out of
          [] -> assertFailure "в svg нет ширины"
          (side : _) -> do
            assertBool
              ("x вне холста: " <> show (filter (\v -> v < 0 || v > side) xs))
              (all (\v -> v >= 0 && v <= side) xs)
            assertBool
              ("y вне холста: " <> show (filter (\v -> v < 0 || v > side) ys))
              (all (\v -> v >= 0 && v <= side) ys)
    , testCase "встречные дуги не совпадают" $ do
        -- У mirror переходы 0->1 и 1->0 имеют одинаковый вес: если знак
        -- изгиба зависит от веса, обе кривые дают один и тот же путь.
        let paths = pathsOf (ringSvg defaultTheme mirror (const "") 0.05)
        length paths @?= length (nub paths)
    , -- Четыре состояния, по два перехода из каждого, петель нет.
      testCase "число дуг равно числу переходов между разными состояниями" $ do
        length (pathsOf (ringSvg defaultTheme mirror (const "") 0.05)) @?= 8
    ]
  where
    -- Только квадратичные кривые: у наконечников стрелок тоже есть path d,
    -- и они одинаковы по построению.
    pathsOf src = [p | p <- splitOn "<path d=\"" src, 'Q' `elem` p]
    splitOn pat s = case breakOn pat s of
      Nothing -> []
      Just rest -> takeWhile (/= '"') rest : splitOn pat rest

-- | Четыре состояния, где встречные переходы имеют равный вес.
mirror :: Machine Int Int
mirror =
  Machine
    { machineStart = 0
    , machineStates = [0 .. 3]
    , machineOut = \_ -> dist [(0, 0.5), (1, 0.5)]
    , machineStep = \s x -> if x == 0 then (s + 1) `mod` 4 else (s + 3) `mod` 4
    }

dotTests :: TestTree
dotTests =
  testGroup
    "dot"
    [ testCase "обрамление на месте" $ do
        let ls = lines (dotOf tri show showSym 0)
        take 1 ls @?= ["digraph kernel {"]
        drop (length ls - 1) ls @?= ["}"]
    , testCase "рёбер столько же, сколько в mermaid без старта" $ do
        let dotEdges = length (filter ("->" `isInfixOf`) (lines (dotOf tri show showSym 0.05)))
        dotEdges @?= length (edgeLines (mermaidOf tri show showSym 0.05)) - 1
    ]
