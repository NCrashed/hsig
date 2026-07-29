-- | Планировщик, стемы и сведение.
module RenderSpec (tests) where

import Control.Exception (ErrorCall, IOException, evaluate, try)
import Data.ByteString qualified as BS
import Data.List (isInfixOf)
import Data.Vector.Unboxed qualified as U
import Sound.Sig.Core
import Sound.Sig.IO
import Sound.Sig.Osc (sine)
import Sound.Sig.Render
import Sound.Sig.Score
import System.Directory (doesFileExist)
import System.FilePath (takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Render"
    [ playTests
    , stemTests
    ]

rate :: Double
rate = envRate defaultEnv

-- | Инструмент-метка: постоянный уровень в долях частоты, длиной в ноту.
-- По нему видно и когда нота началась, и какая именно это нота.
blip :: Instrument
blip n = takeSec (noteDur n) (constant (noteFreq n / 1000))

-- | Сэмпл по времени в секундах.
at :: U.Vector Double -> Double -> Double
at xs t = xs U.! round (t * rate)

playTests :: TestTree
playTests =
  testGroup
    "play"
    [ -- Четыре ноты в цикле встают на свои четверти. Цикл это секунда.
      testCase "ноты встают на свои доли" $ do
        let p = play blip (listToPat (map noteOf [100, 200, 300, 400]))
            xs = render defaultEnv (takeSec 1 p)
        at xs 0.1 @?= 0.1
        at xs 0.3 @?= 0.2
        at xs 0.6 @?= 0.3
        at xs 0.9 @?= 0.4
    , testCase "длительность ноты берётся из события" $ do
        let p = play blip (listToPat (map noteOf [100, 200]))
            xs = render defaultEnv (takeSec 1 p)
        at xs 0.49 @?= 0.1
        at xs 0.51 @?= 0.2
    , testCase "паузы остаются паузами" $ do
        let p = play blip (fastcat [pure (noteOf 100), silence])
            xs = render defaultEnv (takeSec 1 p)
        at xs 0.25 @?= 0.1
        at xs 0.75 @?= 0
    , -- Наложенные ноты складываются, а не затирают друг друга.
      testCase "наложенные ноты складываются" $ do
        let p = play blip (stack [pure (noteOf 100), pure (noteOf 200)])
            xs = render defaultEnv (takeSec 1 p)
        assertBool (show (at xs 0.5)) (abs (at xs 0.5 - 0.3) < 1e-12)
    , -- Нота длиннее блока обязана переезжать через границы блоков.
      testCase "нота длиннее блока не рвётся" $ do
        let p = play blip (slow 4 (pure (noteOf 1000)))
            xs = render defaultEnv (takeSec 3 p)
        assertBool "оборвалась" (U.all (\v -> abs (v - 1) < 1e-12) xs)
    , testCase "не зависит от размера блока" $ do
        let p = play blip (listToPat (map noteOf [100, 200, 300, 400]))
            big = render defaultEnv (takeSec 2 p)
            small = render defaultEnv {envBlock = 97} (takeSec 2 p)
        big @?= small
    , testCase "паттерн повторяется по циклам" $ do
        let p = play blip (listToPat (map noteOf [100, 200]))
            xs = render defaultEnv (takeSec 2 p)
        at xs 0.25 @?= 0.1
        at xs 1.25 @?= 0.1
    , -- Контракт разд. 7: сигнал ноты конечен. Бесконечный вешал бы рендер
      -- молча, поэтому он обязан падать с объяснением.
      testCase "бесконечная нота падает, а не вешает рендер" $ do
        let p = play (const (constant 1)) (pure (noteOf 100))
        r <- try (evaluate (U.length (render defaultEnv (takeSec 0.1 p))))
        case r :: Either ErrorCall Int of
          Left err -> assertBool (show err) ("конечным" `isInfixOf` show err)
          Right _ -> assertFailure "ожидали ошибку"
    , -- Граница предела: нота ровно в предел законна и падать не должна.
      testCase "нота ровно в предел проходит" $ do
        let p = play (const (takeSec 60 (constant 0.5))) (pure (noteOf 100))
        r <- try (evaluate (U.sum (render defaultEnv (takeSec 0.01 p))))
        case r :: Either ErrorCall Double of
          Left err -> assertFailure (show err)
          Right v -> assertBool (show v) (v > 0)
    , -- Каждое событие срабатывает ровно один раз: иначе нота, попавшая в
      -- два блока, зазвучала бы дважды.
      testCase "событие не срабатывает дважды" $ do
        let p = play blip (slow 2 (pure (noteOf 1000)))
            xs = render defaultEnv (takeSec 2 p)
        assertBool "удвоилось" (U.maximum xs < 1.5)
        assertBool "не прозвучало" (U.maximum xs > 0.5)
    , -- У аналогового события нет целого отрезка, значит нет ни атаки, ни
      -- длительности: планировщик обязан его пропускать, как eventHasOnset в
      -- Tidal. Иначе длина ноты бралась бы из части, то есть из размера
      -- блока, и результат зависел бы от envBlock.
      testCase "аналоговое событие не запускается" $ do
        let analog = Pattern (\a -> [Event Nothing a (noteOf 100)])
            big = render defaultEnv (takeSec 1 (play blip analog))
            small = render defaultEnv {envBlock = 97} (takeSec 1 (play blip analog))
        assertBool (show (U.maximum big)) (U.all (== 0) big)
        big @?= small
    , -- Регрессия: cat и всё, что на splitQueries, режет запрос по границам
      -- циклов и отдаёт одну ноту несколькими фрагментами с общим целым
      -- отрезком. Нота, пересекающая границу цикла, запускалась столько раз,
      -- сколько её фрагментов попало в блок. Здесь блок это две секунды, а
      -- нота тянется три и пересекает границы на 1 и 2.
      testCase "нота через границу цикла не удваивается" $ do
        let env = defaultEnv {envRate = 4, envBlock = 8}
            p = play blip (cat [slow 3 (pure (noteOf 100))])
            xs = render env (takeSec 3 p)
        assertBool (show (U.maximum xs)) (abs (U.maximum xs - 0.1) < 1e-12)
    ]

stemTests :: TestTree
stemTests =
  testGroup
    "стемы"
    [ testCase "renderStem кладёт файл с хэшем в имени" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          path <- renderStem defaultEnv 0.1 dir (stemOf "bass" "v1" (takeSec 0.1 (sine 200 * 0.5)))
          doesFileExist path >>= assertBool "файла нет"
          let name = takeFileName path
          assertBool name ("bass-" `isInfixOf` name)
          assertBool name (length name == length "bass-" + 8 + length ".wav")
    , -- Ключ кэша считается от спецификации: её меняют, чтобы заставить
      -- перерендерить.
      testCase "разная спецификация даёт разные файлы" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          a <- renderStem defaultEnv 0.02 dir (stemOf "s" "v1" (constant 0.1))
          b <- renderStem defaultEnv 0.02 dir (stemOf "s" "v2" (constant 0.1))
          assertBool "имена совпали" (a /= b)
    , -- Ключ обязан различать всё, от чего зависит содержимое файла. Сумма
      -- компонент этого не давала: правки, компенсирующие друг друга,
      -- совпадали бы в ключе.
      testCase "ключ различает seed, частоту и длину" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let stem = stemOf "s" "v1" (constant 0.1)
              path env secs = renderStem env secs dir stem
          base <- path defaultEnv 0.02
          seeded <- path defaultEnv {envSeed = 1} 0.02
          longer <- path defaultEnv 0.03
          -- Длина в ключе считается в сэмплах, а не в миллисекундах: иначе
          -- разные длины склеивались бы в один файл.
          nudged <- path defaultEnv 0.0204
          -- Столько же сэмплов, но на другой частоте: без частоты в ключе
          -- это был бы один и тот же файл.
          half <- path defaultEnv {envRate = 24000} 0.04
          assertBool "seed не в ключе" (base /= seeded)
          assertBool "длина не в ключе" (base /= longer)
          assertBool "длина округляется" (base /= nudged)
          assertBool "частота не в ключе" (base /= half)
    , -- Компенсирующая пара правок не должна давать то же имя файла.
      testCase "компенсирующие правки дают разные файлы" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let stem = stemOf "s" "v1" (constant 0.1)
          a <- renderStem defaultEnv {envSeed = 1} (1 / rate) dir stem
          b <- renderStem defaultEnv {envSeed = 0} (2 / rate) dir stem
          assertBool "имена совпали" (a /= b)
    , -- Путь зависит от имени и спецификации, но не от сигнала, поэтому два
      -- одноимённых стема писали бы в один файл одновременно.
      testCase "повтор имени стема это ошибка" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let track =
                [ stemOf "one" "v1" (constant 0.1)
                , stemOf "one" "v1" (constant 0.2)
                ]
          r <- try (renderTrack defaultEnv 0.02 (dir </> "mix.wav") track)
          case r :: Either IOException FilePath of
            Left err -> assertBool (show err) ("one" `isInfixOf` show err)
            Right _ -> assertFailure "ожидали ошибку"
    , -- А одно имя с разными спецификациями это разные файлы и законный
      -- случай: два слоя одной партии.
      testCase "одно имя с разными спецификациями законно" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let track =
                [ stemOf "pad" "v1" (constant 0.1)
                , stemOf "pad" "v2" (constant 0.2)
                ]
          out <- renderTrack defaultEnv 0.02 (dir </> "mix.wav") track
          (_, _, xs) <- readWav out
          assertBool "не сложилось" (U.all (\v -> abs (abs v - 0.3 / sqrt 2) < 1e-3) xs)
    , -- Битый стем обязан падать громко: молча он подмешал бы тишину, и
      -- сам бы не починился, потому что хэш не зависит от содержимого.
      testCase "усечённый стем в кэше это ошибка" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          path <- renderStem defaultEnv 0.05 dir (stemOf "s" "v1" (takeSec 0.05 (sine 200 * 0.5)))
          bytes <- BS.readFile path
          BS.writeFile path (BS.take (BS.length bytes - 200) bytes)
          r <- try (mixStems defaultEnv [path] >>= evaluate . U.sum . render defaultEnv)
          case r :: Either IOException Double of
            -- Именно от проверки длины чанка, а не от любого сбоя ввода.
            Left err -> assertBool (show err) ("data" `isInfixOf` show err)
            Right v -> assertFailure ("прочиталось как " <> show v)
    , -- Кэш: файл на месте, значит рендер пропускается. Проверяем подменой
      -- содержимого.
      testCase "готовый файл не перерендеривается" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let stem = stemOf "s" "v1" (constant 0.5)
          path <- renderStem defaultEnv 0.02 dir stem
          writeFile path "подмена"
          path' <- renderStem defaultEnv 0.02 dir stem
          path' @?= path
          readFile path >>= (@?= "подмена")
    , testCase "стем читается обратно тем же сигналом" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let sig = takeSec 0.05 (sine 200 * 0.5)
          path <- renderStem defaultEnv 0.05 dir (stemOf "s" "v1" sig)
          (r, channels, xs) <- readWav path
          r @?= rate
          channels @?= 1
          let want = render defaultEnv sig
          U.length xs @?= U.length want
          -- float32 при чтении обратно даёт ошибку не больше своей точности.
          assertBool "разошлось" (U.maximum (U.map abs (U.zipWith (-) xs want)) < 1e-7)
    , testCase "mixStems складывает файлы" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          a <- renderStem defaultEnv 0.05 dir (stemOf "a" "v1" (constant 0.25))
          b <- renderStem defaultEnv 0.05 dir (stemOf "b" "v1" (constant 0.5))
          mixed <- mixStems defaultEnv [a, b]
          let xs = render defaultEnv mixed
          U.length xs @?= round (0.05 * rate)
          assertBool "не сложилось" (U.all (\v -> abs (v - 0.75) < 1e-6) xs)
    , -- Мастер стерео: панорама стемов раскладывает их по каналам.
      testCase "renderTrack пишет стерео-мастер" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let track =
                [ stemOf "one" "v1" (play blip (listToPat (map noteOf [100, 200])))
                , stemOf "two" "v1" (play blip (listToPat [noteOf 300]))
                ]
          path <- renderTrack defaultEnv 0.5 (dir </> "mix.wav") track
          (_, channels, xs) <- readWav path
          channels @?= 2
          U.length xs @?= 2 * round (0.5 * rate)
          -- По центру каналы равны с точностью до дизера: он у каждого
          -- сэмпла чередованного потока свой, то есть у каналов независимый.
          let frame i = (xs U.! (2 * i), xs U.! (2 * i + 1))
              (l, r) = frame (round (0.25 * rate))
          assertBool (show (l, r)) (abs (l - r) <= 2 / 32768)
          assertBool (show l) (abs (l - 0.4 / sqrt 2) < 1e-3)
    , testCase "панорама разводит стемы по каналам" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let track =
                [ (stemOf "l" "v1" (constant 0.5)) {stemPan = -1}
                , (stemOf "r" "v1" (constant 0.25)) {stemPan = 1}
                ]
          path <- renderTrack defaultEnv 0.02 (dir </> "mix.wav") track
          (_, _, xs) <- readWav path
          let i = 400
              (l, r) = (xs U.! (2 * i), xs U.! (2 * i + 1))
          assertBool (show l) (abs (l - 0.5) < 1e-3)
          assertBool (show r) (abs (r - 0.25) < 1e-3)
    , -- Требование разд. 12: два рендера дают побитово одинаковый файл.
      -- Стемов несколько, потому что рендерятся они параллельно.
      testCase "рендер детерминирован" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let track =
                [ stemOf "one" "v1" (play blip (listToPat (map noteOf [100, 200])))
                , (stemOf "two" "v1" (play blip (fast 3 (pure (noteOf 300))))) {stemPan = -0.5}
                , (stemOf "three" "v1" (play blip (slow 2 (pure (noteOf 400))))) {stemPan = 0.7}
                , stemOf "four" "v1" (takeSec 0.3 (sine 220 * 0.2))
                ]
          a <- renderTrack defaultEnv 0.3 (dir </> "a" </> "mix.wav") track
          b <- renderTrack defaultEnv 0.3 (dir </> "b" </> "mix.wav") track
          (_, _, xs) <- readWav a
          (_, _, ys) <- readWav b
          -- Длина и энергия: без них тест прошёл бы и на двух пустых файлах.
          U.length xs @?= 2 * round (0.3 * rate)
          assertBool "тишина" (U.sum (U.map abs xs) > 0)
          xs @?= ys
    ]
