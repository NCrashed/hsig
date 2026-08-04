-- | Ядро на картинке.
--
-- Диаграмма выводится из самой машины, а не рисуется отдельно. Это не
-- удобство: нарисованная вручную схема расходится с кодом на первой же
-- правке весов, и тогда картинка начинает врать про то, что звучит.
--
-- Форматы текстовые и без зависимостей, как и запись WAV. Mermaid и DOT
-- отдают разметку чужим рисовалкам; 'ringSvg' и 'traceSvg' рисуют сами и
-- кладут готовый файл рядом со звуком.
module Sound.Pred.Diagram
  ( -- * Разметка для чужих рисовалок
    mermaidOf
  , dotOf

    -- * Готовые картинки
  , Theme (..)
  , defaultTheme
  , ringSvg
  , traceSvg
  ) where

import Data.List (elemIndex, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Numeric (showFFloat)
import Sound.Pred.Dist
import Sound.Pred.Machine

-- | Рёбра машины: из состояния, по символу, с вероятностью, в состояние.
--
-- Рёбра ниже порога отбрасываются: у машины с четырьмя символами их
-- вчетверо больше состояний, и хвост в сотые доли делает картинку
-- нечитаемой, ничего не добавляя.
edgesOf :: Machine s a -> Double -> [(s, a, Double, s)]
edgesOf m floorP =
  [ (s, x, p, machineStep m s x)
  | s <- machineStates m
  , (x, p) <- sortOn (negate . snd) (distPairs (machineOut m s))
  , p >= floorP
  ]

-- | Диаграмма состояний в синтаксисе mermaid.
--
-- Параметры: подпись состояния, подпись символа и порог вероятности.
-- Подписи снаружи, потому что смысл символа знает пьеса, а не машина.
mermaidOf :: (Ord s) => Machine s a -> (s -> String) -> (a -> String) -> Double -> String
mermaidOf m stateLabel symLabel floorP =
  unlines $
    ["stateDiagram-v2", "    direction LR"]
      <> ["    " <> node s <> ": " <> stateLabel s | s <- machineStates m]
      <> ["    [*] --> " <> node (machineStart m)]
      <> [ "    " <> node s <> " --> " <> node t <> ": " <> symLabel x <> " " <> pct p
         | (s, x, p, t) <- edgesOf m floorP
         ]
  where
    node s = "s" <> show (index s)
    index s = M.findWithDefault 0 s ids
    ids = M.fromList (zip (machineStates m) [0 :: Int ..])

-- | То же в синтаксисе graphviz.
dotOf :: (Ord s) => Machine s a -> (s -> String) -> (a -> String) -> Double -> String
dotOf m stateLabel symLabel floorP =
  unlines $
    ["digraph kernel {", "  rankdir=LR;", "  node [shape=circle];"]
      <> ["  s" <> show (index s) <> " [label=\"" <> stateLabel s <> "\"];" | s <- machineStates m]
      <> [ "  s"
          <> show (index s)
          <> " -> s"
          <> show (index t)
          <> " [label=\""
          <> symLabel x
          <> " "
          <> pct p
          <> "\"];"
         | (s, x, p, t) <- edgesOf m floorP
         ]
      <> ["}"]
  where
    index s = M.findWithDefault 0 s ids
    ids = M.fromList (zip (machineStates m) [0 :: Int ..])

-- | Вероятность в проценты без дробной части: на схеме сотые не читаются.
pct :: Double -> String
pct p = show (round (100 * p) :: Int) <> "%"

-- SVG -------------------------------------------------------------------------

-- | Цвета картинки. Палитра циклическая: состояний может быть больше, чем
-- цветов, и это не повод падать.
data Theme = Theme
  { themeBgLight :: String
  , themeInkLight :: String
  , themeMutedLight :: String
  , themeBgDark :: String
  , themeInkDark :: String
  , themeMutedDark :: String
  , themePalette :: [String]
  }

-- | Умолчание: бумага и тушь, палитра средней светлоты.
--
-- Цвета состояний одни на обе темы намеренно. Средняя светлота читается и
-- на бумаге, и на тёмном, а две палитры пришлось бы держать в согласии
-- вручную и они бы разъехались.
defaultTheme :: Theme
defaultTheme =
  Theme
    { themeBgLight = "#f6f4ef"
    , themeInkLight = "#141c1e"
    , themeMutedLight = "#5d6e6d"
    , themeBgDark = "#0d1315"
    , themeInkDark = "#e6edeb"
    , themeMutedDark = "#94a6a5"
    , themePalette =
        [ "#c8862a"
        , "#a8524a"
        , "#2f8f80"
        , "#4a7fa5"
        , "#7d8c3a"
        , "#8a6aa0"
        , "#c2712f"
        , "#46806b"
        ]
    }

colorOf :: Theme -> Int -> String
colorOf th i = ps !! (i `mod` length ps)
  where
    ps = themePalette th

-- | Обёртка файла: размеры, переменные тем и общие классы.
--
-- Переменные меняются по @prefers-color-scheme@, поэтому один файл годится
-- и для светлой страницы, и для тёмной. Просмотрщик, не понимающий
-- медиазапросов, увидит светлый вариант из объявления по умолчанию.
svgDoc :: Theme -> Double -> Double -> [String] -> String
svgDoc th w h body =
  unlines $
    [ "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" <> num w <> "\" height=\"" <> num h <> "\" viewBox=\"0 0 " <> num w <> " " <> num h <> "\">"
    , "<style>"
    , "svg{--bg:" <> themeBgLight th <> ";--ink:" <> themeInkLight th <> ";--muted:" <> themeMutedLight th <> "}"
    , "@media(prefers-color-scheme:dark){svg{--bg:" <> themeBgDark th <> ";--ink:" <> themeInkDark th <> ";--muted:" <> themeMutedDark th <> "}}"
    , "text{font-family:ui-monospace,'SF Mono',Menlo,Consolas,monospace}"
    , ".halo{paint-order:stroke;stroke:var(--bg);stroke-width:4;stroke-linejoin:round}"
    , "</style>"
    , "<rect width=\"" <> num w <> "\" height=\"" <> num h <> "\" fill=\"var(--bg)\"/>"
    ]
      <> body
      <> ["</svg>"]

num :: Double -> String
num x = showFFloat (Just 1) x ""

esc :: String -> String
esc = concatMap one
  where
    one '&' = "&amp;"
    one '<' = "&lt;"
    one '>' = "&gt;"
    one c = [c]

-- | Подпись с ореолом цвета подложки: читается, даже когда под ней линия.
svgText :: Double -> Double -> String -> String -> Double -> String
svgText x y fill body size =
  "<text class=\"halo\" x=\""
    <> num x
    <> "\" y=\""
    <> num y
    <> "\" text-anchor=\"middle\" dominant-baseline=\"middle\" fill=\""
    <> fill
    <> "\" font-size=\""
    <> num size
    <> "\">"
    <> esc body
    <> "</text>"

-- | Кольцевая диаграмма машины.
--
-- Состояния раскладываются по окружности, толщина ребра равна вероятности,
-- подпись под номером задаётся вызывающим (обычно созвучие состояния).
--
-- Две тонкости геометрии, обе выяснены на живой картинке. Знак изгиба дуги
-- берётся от канонической пары (меньший индекс к большему) и лишь потом
-- умножается на направление: если считать нормаль от самого ребра, у
-- встречной пары развернётся и вектор, и знак, они погасят друг друга, и
-- два одинаково сильных встречных перехода лягут на одну кривую. Наконечник
-- стрелки задан в пользовательских единицах, иначе он умножается на толщину
-- линии и на сильном ребре вырастает во весь узел.
ringSvg :: (Eq s) => Theme -> Machine s a -> (s -> String) -> Double -> String
ringSvg th m sub floorP = svgDoc th w w (defs <> loops <> arcs <> nodes <> labels)
  where
    sts = machineStates m
    n = length sts
    idx s = fromMaybe 0 (elemIndex s sts)
    r = max 110 (fromIntegral n * 25)
    nr = 34
    -- Поле считается по самой дальней подписи, а не подбирается на глаз.
    reach = r + nr + 54 + 30
    w = 2 * reach
    cx = reach
    cy = reach

    angleOf i = (-90 + fromIntegral i * 360 / fromIntegral n) * pi / 180
    posOf i = (cx + r * cos (angleOf i), cy + r * sin (angleOf i))

    -- Тонкие рёбра рисуются первыми, чтобы толстые не тонули под ними.
    edges = sortOn (\(_, _, p, _) -> p) (edgesOf m floorP)

    defs =
      "<defs>"
        : [ "<marker id=\"ah"
            <> show i
            <> "\" viewBox=\"0 0 10 10\" refX=\"9\" refY=\"5\" markerUnits=\"userSpaceOnUse\" markerWidth=\"11\" markerHeight=\"11\" orient=\"auto\">"
            <> "<path d=\"M 0 0 L 10 5 L 0 10 z\" fill=\""
            <> colorOf th i
            <> "\"/></marker>"
          | i <- [0 .. n - 1]
          ]
          <> ["</defs>"]

    width p = 1 + 6 * p

    loops =
      [ "<circle cx=\""
        <> num lx
        <> "\" cy=\""
        <> num ly
        <> "\" r=\"18\" fill=\"none\" stroke=\""
        <> colorOf th (idx s)
        <> "\" stroke-width=\""
        <> num (width p)
        <> "\" opacity=\"0.9\"/>"
      | (s, _, p, t) <- edges
      , s == t
      , let a = angleOf (idx s)
      , let (x, y) = posOf (idx s)
      , let lx = x + (nr + 20) * cos a
      , let ly = y + (nr + 20) * sin a
      ]

    arcs =
      [ "<path d=\"M "
        <> num sx
        <> " "
        <> num sy
        <> " Q "
        <> num qx
        <> " "
        <> num qy
        <> " "
        <> num ex
        <> " "
        <> num ey
        <> "\" fill=\"none\" stroke=\""
        <> colorOf th (idx s)
        <> "\" stroke-width=\""
        <> num (width p)
        <> "\" opacity=\""
        <> (if p > 0.5 then "0.95" else "0.45")
        <> "\" marker-end=\"url(#ah"
        <> show (idx s)
        <> ")\"/>"
      | (s, _, p, t) <- edges
      , s /= t
      , let (qx, qy, sx, sy, ex, ey, _, _) = geom s t
      ]

    -- Подписи идут последними: ореол должен перекрывать всё, что под ними
    -- прошло, а не только нарисованное раньше.
    labels =
      [ svgText tx ty (colorOf th (idx s)) (pct p) (if s == t || p > 0.5 then 13 else 11)
      | (s, _, p, t) <- edges
      , let (tx, ty) = labelAt s t
      ]

    -- Подпись петли отходит от её обода (петля радиусом 18 стоит на nr+20),
    -- подпись дуги уезжает за её вершину, а не садится на линию.
    labelAt s t
      | s == t = (x + (nr + 54) * cos a, y + (nr + 54) * sin a)
      | otherwise = let (_, _, _, _, _, _, lx, ly) = geom s t in (lx, ly)
      where
        a = angleOf (idx s)
        (x, y) = posOf (idx s)

    geom s t = (qx, qy, sx, sy, ex, ey, lx, ly)
      where
        (x1, y1) = posOf (idx s)
        (x2, y2) = posOf (idx t)
        mx = (x1 + x2) / 2
        my = (y1 + y2) / 2
        lo = min (idx s) (idx t)
        hi = max (idx s) (idx t)
        (ax, ay) = posOf lo
        (bx, by) = posOf hi
        cd = max 1 (sqrt ((bx - ax) ** 2 + (by - ay) ** 2))
        sign = if idx s < idx t then 1 else -1
        nx = negate (by - ay) / cd * sign
        ny = (bx - ax) / cd * sign
        bow = 40
        qx = mx + nx * bow
        qy = my + ny * bow
        (sx, sy) = toward x1 y1 qx qy
        (ex, ey) = toward x2 y2 qx qy
        lx = mx + nx * (bow / 2 + 17)
        ly = my + ny * (bow / 2 + 17)

    -- Конец дуги отодвигается от центра узла, чтобы стрелка не заезжала
    -- на кружок.
    toward px py tx ty = (px + ux / d * (nr + 7), py + uy / d * (nr + 7))
      where
        ux = tx - px
        uy = ty - py
        d = max 1 (sqrt (ux * ux + uy * uy))

    nodes =
      concat
        [ [ "<circle cx=\""
            <> num x
            <> "\" cy=\""
            <> num y
            <> "\" r=\""
            <> num nr
            <> "\" fill=\"var(--bg)\" stroke=\""
            <> colorOf th i
            <> "\" stroke-width=\"2\"/>"
          , svgText x (y - 6) (colorOf th i) ("s" <> show i) 17
          , svgText x (y + 12) "var(--muted)" (sub s) 10
          ]
        | (i, s) <- zip [0 ..] sts
        , let (x, y) = posOf i
        ]

-- | Полоса следа: клетка на событие, группа на такт.
--
-- Показывает форму пьесы до того, как её услышали: длинные одноцветные
-- пробеги это выдержанные аккорды, чередование - качание между соседними
-- состояниями.
traceSvg :: (Eq s) => Theme -> Machine s a -> Int -> Int -> [s] -> String
traceSvg th m barLen perRow evs
  | barLen < 1 || perRow < 1 = error "hsig-pred: непозитивный размер решётки следа"
  | otherwise = svgDoc th w h cells
  where
    sts = machineStates m
    idx s = fromMaybe 0 (elemIndex s sts)
    cw = 8
    ch = 26
    gap = 1
    barGap = 7
    pad = 16
    rowGap = 10
    barW = fromIntegral barLen * (cw + gap) - gap
    bars = chunk barLen evs
    rows = chunk perRow bars
    -- Ширина по фактическому числу тактов в строке, а не по потолку:
    -- короткий след иначе получает холст с пустотой на треть.
    cols = max 1 (min perRow (length bars))
    w = 2 * pad + fromIntegral cols * (barW + barGap) - barGap
    h = 2 * pad + fromIntegral (max 1 (length rows)) * (ch + rowGap) - rowGap

    cells =
      [ "<rect x=\""
        <> num x
        <> "\" y=\""
        <> num y
        <> "\" width=\""
        <> num cw
        <> "\" height=\""
        <> num ch
        <> "\" fill=\""
        <> colorOf th (idx s)
        <> "\"/>"
      | (ri, row) <- zip [0 :: Int ..] rows
      , (bi, bar) <- zip [0 :: Int ..] row
      , (ci, s) <- zip [0 :: Int ..] bar
      , let x = pad + fromIntegral bi * (barW + barGap) + fromIntegral ci * (cw + gap)
      , let y = pad + fromIntegral ri * (ch + rowGap)
      ]

    chunk k xs
      | null xs = []
      | otherwise = take k xs : chunk k (drop k xs)
