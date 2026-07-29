-- | Планировщик, стемы и сведение.
module RenderSpec (tests) where

import Control.Exception (ErrorCall, evaluate, try)
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
    ]

stemTests :: TestTree
stemTests =
  testGroup
    "стемы"
    [ testCase "renderStem кладёт файл с хэшем в имени" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let stem = Stem "bass" (takeSec 0.1 (sine 200 * 0.5))
          path <- renderStem defaultEnv 0.1 dir stem
          doesFileExist path >>= assertBool "файла нет"
          let name = takeFileName path
          assertBool name ("bass-" `isInfixOf` name)
          assertBool name (".wav" `isInfixOf` name)
          assertBool name (length name == length "bass-" + 8 + length ".wav")
    , testCase "стем читается обратно тем же сигналом" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let sig = takeSec 0.05 (sine 200 * 0.5)
              stem = Stem "s" sig
          path <- renderStem defaultEnv 0.05 dir stem
          (r, xs) <- readWav path
          r @?= rate
          let want = render defaultEnv sig
          U.length xs @?= U.length want
          -- float32 при чтении обратно даёт ошибку не больше своей точности.
          assertBool "разошлось" (U.maximum (U.map abs (U.zipWith (-) xs want)) < 1e-7)
    , testCase "mixStems складывает файлы" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          a <- renderStem defaultEnv 0.05 dir (Stem "a" (constant 0.25))
          b <- renderStem defaultEnv 0.05 dir (Stem "b" (constant 0.5))
          mixed <- mixStems defaultEnv [a, b]
          let xs = render defaultEnv mixed
          U.length xs @?= round (0.05 * rate)
          assertBool "не сложилось" (U.all (\v -> abs (v - 0.75) < 1e-6) xs)
    , testCase "renderTrack пишет мастер и стемы" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let track =
                [ Stem "one" (play blip (listToPat (map noteOf [100, 200])))
                , Stem "two" (play blip (listToPat [noteOf 300]))
                ]
          path <- renderTrack defaultEnv 0.5 (dir </> "mix.wav") track
          doesFileExist path >>= assertBool "мастера нет"
          (_, xs) <- readWav path
          U.length xs @?= round (0.5 * rate)
          -- 0.1 от первого стема плюс 0.3 от второго.
          assertBool (show (at xs 0.25)) (abs (at xs 0.25 - 0.4) < 1e-3)
    , -- Требование разд. 12: два рендера дают побитово одинаковый файл.
      testCase "рендер детерминирован" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let track = [Stem "s" (play blip (listToPat (map noteOf [100, 200])))]
          a <- renderTrack defaultEnv 0.3 (dir </> "a.wav") track
          b <- renderTrack defaultEnv 0.3 (dir </> "b.wav") track
          (_, xs) <- readWav a
          (_, ys) <- readWav b
          xs @?= ys
    ]
