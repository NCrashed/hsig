-- | Планировщик, стемы и сведение.
module RenderSpec (tests) where

import Control.Exception (ErrorCall, IOException, evaluate, try)
import Control.Monad (filterM, zipWithM_)
import Data.ByteString qualified as BS
import Data.List (isInfixOf)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Data.Vector.Unboxed qualified as U
import Sound.Sig.Core
import Sound.Sig.IO
import Sound.Sig.Osc (sine)
import Sound.Sig.Render
import Sound.Sig.Score
import Sound.Sig.Stereo (Stereo (..), bothChannels)
import System.Directory (doesFileExist, listDirectory, setModificationTime)
import System.FilePath (takeExtension, takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit
import TestIO (captureStderr)

tests :: TestTree
tests =
  testGroup
    "Render"
    [ playTests
    , stemTests
    ]

rate :: Double
rate = envRate defaultEnv

-- | Кэшируемый стем заданной длины: renderStem длину аргументом не берёт,
-- стем обязан кончаться сам.
stemOf :: String -> String -> Double -> Sig -> Stem
stemOf name spec secs sig = cached spec (stem name (takeSec secs sig))

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
    , -- Инвариант блоков: все блоки ровно по envBlock. Держит вторую половину
      -- условия в overlapAdd (длина хвоста не меньше блока): без неё в паузе
      -- после короткой ноты выдался бы обрезанный блок, и всё дальше уехало
      -- бы по времени, потому что смещение ноты считается от номера блока.
      testCase "блоки ровно по envBlock" $ do
        let p = play blip (fastcat [pure (noteOf 100), silence])
            blocks = take 30 (runSig p defaultEnv)
        assertBool (show (map U.length blocks)) (all ((== envBlock defaultEnv) . U.length) blocks)
        -- И то же по существу: результат не зависит от нарезки.
        render defaultEnv (takeSec 2 p) @?= render defaultEnv {envBlock = 97} (takeSec 2 p)
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
    [ -- По умолчанию кэша нет: имя файла это имя стема, и он перезаписывается
      -- каждый прогон. Так правка патча слышна без возни с версиями.
      testCase "стем без кэша пишется по своему имени" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let s = stem "bass" (takeSec 0.05 (sine 200 * 0.5))
          path <- renderStem defaultEnv dir s
          takeFileName path @?= "bass.wav"
          writeFile path "подмена"
          path' <- renderStem defaultEnv dir s
          path' @?= path
          -- Перерендерился, а не остался подменой.
          bytes <- BS.readFile path
          assertBool "не перерендерился" (BS.take 4 bytes == BS.pack (map (fromIntegral . fromEnum) "RIFF"))
    , testCase "cached кладёт файл с хэшем в имени" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          path <- renderStem defaultEnv dir (stemOf "bass" "v1" 0.05 (sine 200 * 0.5))
          doesFileExist path >>= assertBool "файла нет"
          -- Имя это <имя>-<хэш>.wav.
          let name = takeFileName path
          assertBool name ("bass-" `isInfixOf` name)
          assertBool name (length name == length "bass-" + 8 + length ".wav")
    , -- Ключ кэша считается от спецификации: её меняют, чтобы заставить
      -- перерендерить.
      testCase "разная спецификация даёт разные файлы" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          a <- renderStem defaultEnv dir (stemOf "s" "v1" 0.02 (constant 0.1))
          b <- renderStem defaultEnv dir (stemOf "s" "v2" 0.02 (constant 0.1))
          assertBool "имена совпали" (a /= b)
    , -- Ключ обязан различать всё, от чего зависит содержимое файла, помимо
      -- спецификации. Сумма компонент этого не давала: правки, компенсирующие
      -- друг друга, совпадали бы в ключе.
      testCase "ключ различает seed и частоту" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let path env = renderStem env dir (stemOf "s" "v1" 0.02 (constant 0.1))
          base <- path defaultEnv
          seeded <- path defaultEnv {envSeed = 1}
          other <- path defaultEnv {envRate = 24000}
          assertBool "seed не в ключе" (base /= seeded)
          assertBool "частота не в ключе" (base /= other)
    , -- А длина в ключ не входит: её теперь задаёт материал, и узнать её
      -- заранее нельзя, не отрендерив стем. Это цена кэша: спецификация
      -- обязана описывать и длину, иначе укороченный трек подмешает старый
      -- стем целиком. Закрепляем как поведение, а не как случайность.
      testCase "длина в ключ не входит" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          short <- renderStem defaultEnv dir (stemOf "s" "v1" 0.02 (constant 0.1))
          long <- renderStem defaultEnv dir (stemOf "s" "v1" 0.05 (constant 0.1))
          long @?= short
          (_, _, xs) <- readWav long
          U.length xs @?= round (0.02 * rate)
    , -- Путь без кэша зависит только от имени, поэтому два одноимённых стема
      -- писали бы в один файл одновременно.
      testCase "повтор имени стема это ошибка" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let track =
                [ stem "one" (takeSec 0.02 (constant 0.1))
                , stem "one" (takeSec 0.02 (constant 0.2))
                ]
          r <- try (renderTrack defaultEnv (dir </> "mix.wav") track)
          case r :: Either IOException FilePath of
            Left err -> assertBool (show err) ("one" `isInfixOf` show err)
            Right _ -> assertFailure "ожидали ошибку"
    , -- А одно имя с разными спецификациями это разные файлы и законный
      -- случай: два слоя одной партии.
      testCase "одно имя с разными спецификациями законно" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let track =
                [ stemOf "pad" "v1" 0.02 (constant 0.1)
                , stemOf "pad" "v2" 0.02 (constant 0.2)
                ]
          out <- renderTrack defaultEnv (dir </> "mix.wav") track
          (_, _, xs) <- readWav out
          assertBool "не сложилось" (U.all (\v -> abs (abs v - 0.3 / sqrt 2) < 1e-3) xs)
    , -- Битый стем обязан падать громко: молча он подмешал бы тишину, и
      -- сам бы не починился, потому что хэш не зависит от содержимого.
      testCase "усечённый стем в кэше это ошибка" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          path <- renderStem defaultEnv dir (stemOf "s" "v1" 0.05 (sine 200 * 0.5))
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
          let s = stemOf "s" "v1" 0.02 (constant 0.5)
          path <- renderStem defaultEnv dir s
          writeFile path "подмена"
          path' <- renderStem defaultEnv dir s
          path' @?= path
          readFile path >>= (@?= "подмена")
    , -- Бесконечный стем это ошибка автора: длина берётся из материала,
      -- значит окно или огибающая обязаны его закончить. Предел проверяем на
      -- игрушечной частоте, иначе тест пришлось бы гнать 600 секунд.
      testCase "бесконечный стем падает с объяснением" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let env = defaultEnv {envRate = 100, envBlock = 64}
          r <- try (renderStem env dir (stem "s" (constant 0.1)))
          case r :: Either IOException FilePath of
            Left err -> assertBool (show err) ("gate" `isInfixOf` show err)
            Right _ -> assertFailure "ожидали ошибку"
          -- И недописанный файл за собой не оставляет.
          doesFileExist (dir </> "s.wav") >>= assertBool "остался обрубок" . not
    , testCase "стем читается обратно тем же сигналом" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let sig = takeSec 0.05 (sine 200 * 0.5)
          path <- renderStem defaultEnv dir (stem "s" sig)
          (r, channels, xs) <- readWav path
          r @?= rate
          channels @?= 1
          let want = render defaultEnv sig
          U.length xs @?= U.length want
          -- float32 при чтении обратно даёт ошибку не больше своей точности.
          assertBool "разошлось" (U.maximum (U.map abs (U.zipWith (-) xs want)) < 1e-7)
    , testCase "mixStems складывает файлы" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          a <- renderStem defaultEnv dir (stem "a" (takeSec 0.05 (constant 0.25)))
          b <- renderStem defaultEnv dir (stem "b" (takeSec 0.05 (constant 0.5)))
          mixed <- mixStems defaultEnv [a, b]
          let xs = render defaultEnv mixed
          U.length xs @?= round (0.05 * rate)
          assertBool "не сложилось" (U.all (\v -> abs (v - 0.75) < 1e-6) xs)
    , -- Длина мастера берётся из самого длинного стема, короткие дотягиваются
      -- нулями: аргумента длины у renderTrack больше нет.
      testCase "длина мастера это длина самого длинного стема" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let track =
                [ stem "short" (takeSec 0.02 (constant 0.5))
                , stem "long" (takeSec 0.05 (constant 0.25))
                ]
          path <- renderTrack defaultEnv (dir </> "mix.wav") track
          (_, channels, xs) <- readWav path
          channels @?= 2
          U.length xs @?= 2 * round (0.05 * rate)
          let frame i = xs U.! (2 * i)
          assertBool (show (frame 100)) (abs (frame 100 - 0.75 / sqrt 2) < 1e-3)
          assertBool (show (frame 2000)) (abs (frame 2000 - 0.25 / sqrt 2) < 1e-3)
    , -- Мастер стерео: панорама стемов раскладывает их по каналам.
      testCase "renderTrack пишет стерео-мастер" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let track =
                [ stem "one" (takeSec 0.5 (play blip (listToPat (map noteOf [100, 200]))))
                , stem "two" (takeSec 0.5 (play blip (listToPat [noteOf 300])))
                ]
          path <- renderTrack defaultEnv (dir </> "mix.wav") track
          (_, channels, xs) <- readWav path
          channels @?= 2
          U.length xs @?= 2 * round (0.5 * rate)
          -- По центру каналы равны с точностью до дизера: он у каждого
          -- сэмпла чередованного потока свой, то есть у каналов независимый.
          let frame i = (xs U.! (2 * i), xs U.! (2 * i + 1))
              (l, r) = frame (round (0.25 * rate))
          assertBool (show (l, r)) (abs (l - r) <= 2 / 32768)
          assertBool (show l) (abs (l - 0.4 / sqrt 2) < 1e-3)
    , testCase "panned разводит стемы по каналам" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let track =
                [ panned (-1) (stem "l" (takeSec 0.02 (constant 0.5)))
                , panned 1 (stem "r" (takeSec 0.02 (constant 0.25)))
                ]
          path <- renderTrack defaultEnv (dir </> "mix.wav") track
          (_, _, xs) <- readWav path
          let i = 400
              (l, r) = (xs U.! (2 * i), xs U.! (2 * i + 1))
          assertBool (show l) (abs (l - 0.5) < 1e-3)
          assertBool (show r) (abs (r - 0.25) < 1e-3)
    , -- stereoStems это два стема по краям: готовое стерео не надо руками
      -- расписывать на каналы.
      testCase "stereoStems раскладывает стерео по краям" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let sig = Stereo (takeSec 0.02 (constant 0.5)) (takeSec 0.02 (constant 0.25))
          path <- renderTrack defaultEnv (dir </> "mix.wav") (stereoStems "pad" sig)
          (_, _, xs) <- readWav path
          let (l, r) = (xs U.! 800, xs U.! 801)
          assertBool (show l) (abs (l - 0.5) < 1e-3)
          assertBool (show r) (abs (r - 0.25) < 1e-3)
    , -- Обработка мастера идёт после сведения и до записи, поэтому её правка
      -- не трогает стемы и не сбивает кэш.
      testCase "renderTrackWith прогоняет мастер" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let track = [stem "one" (takeSec 0.02 (constant 0.5))]
          plain <- renderTrack defaultEnv (dir </> "a" </> "mix.wav") track
          quiet <-
            renderTrackWith
              defaultEnv
              (dir </> "b" </> "mix.wav")
              (bothChannels (* constant 0.5))
              track
          (_, _, xs) <- readWav plain
          (_, _, ys) <- readWav quiet
          U.length ys @?= U.length xs
          assertBool "мастер не обработан" $
            U.and (U.zipWith (\a b -> abs (b - a / 2) < 2 / 32768) xs ys)
    , -- Уборка кэша: правка спецификации меняет хэш, и без уборки каталог
      -- зарастает версиями. Удаление разрушительно, поэтому границы
      -- закреплены: чужие имена, чужие расширения, не-хэши и стемы без кэша
      -- (это one.wav) остаются на месте. Сколько версий переживает уборку -
      -- отдельным тестом ниже.
      testCase "уборка не трогает чужие файлы" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let mix = dir </> "mix.wav"
              track spec = [stemOf "one" spec 0.02 (constant 0.1)]
              -- renderTrack отдаёт путь мастера, поэтому путь стема берём
              -- отдельно; второй вызов на уже отрендеренном просто его вернёт.
              stemOne spec = renderStem defaultEnv dir (stemOf "one" spec 0.02 (constant 0.1))
          old <- stemOne "v1"
          -- Путь свежего стема берём до рендера, иначе renderStem создаст его
          -- сам и проверка "новый жив" станет тавтологией.
          new <- stemOne "v2"
          writeFile (dir </> "one-notahash.wav") "чужое"
          writeFile (dir </> "one-1a2b3c4d.txt") "чужое"
          writeFile (dir </> "other-1a2b3c4d.wav") "чужое"
          writeFile (dir </> "one.wav") "чужое"
          _ <- renderTrack defaultEnv mix (track "v2")
          assertBool "хэш не сменился" (old /= new)
          doesFileExist new >>= assertBool "уборка снесла свежий стем"
          mapM_
            (\f -> doesFileExist (dir </> f) >>= assertBool ("снесли " <> f))
            ["one-notahash.wav", "one-1a2b3c4d.txt", "other-1a2b3c4d.wav", "one.wav"]
    , -- Пустой список стемов это отладочный случай (закомментировали всё):
      -- уборка не должна на нём падать, даже если каталога ещё нет.
      testCase "пустой трек не падает на уборке" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          out <- renderTrack defaultEnv (dir </> "empty" </> "mix.wav") []
          doesFileExist out >>= assertBool "мастер не записан"
    , -- Требование разд. 12: два рендера дают побитово одинаковый файл.
      -- Стемов несколько, потому что рендерятся они параллельно.
      testCase "рендер детерминирован" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let track =
                [ stem "one" (takeSec 0.3 (play blip (listToPat (map noteOf [100, 200]))))
                , panned (-0.5) (stem "two" (takeSec 0.3 (play blip (fast 3 (pure (noteOf 300))))))
                , panned 0.7 (stem "three" (takeSec 0.3 (play blip (slow 2 (pure (noteOf 400))))))
                , stem "four" (takeSec 0.3 (sine 220 * 0.2))
                ]
          a <- renderTrack defaultEnv (dir </> "a" </> "mix.wav") track
          b <- renderTrack defaultEnv (dir </> "b" </> "mix.wav") track
          (_, _, xs) <- readWav a
          (_, _, ys) <- readWav b
          -- Длина и энергия: без них тест прошёл бы и на двух пустых файлах.
          U.length xs @?= 2 * round (0.3 * rate)
          assertBool "тишина" (U.sum (U.map abs xs) > 0)
          xs @?= ys
    , -- Компенсирующая пара правок Env не должна давать одно имя файла: при
      -- сложении компонент seed на единицу вверх и частота на герц вниз
      -- совпали бы, и в микс молча ушёл бы стем из чужого прогона.
      testCase "ключ различает компенсирующие правки Env" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let path env = renderStem env dir (stemOf "s" "v1" 0.002 (constant 0.1))
          a <- path defaultEnv
          b <- path defaultEnv {envRate = 47999, envSeed = 1}
          assertBool "имена совпали" (a /= b)
    , -- Хэш это ровно восемь строчных hex-цифр. Потеря дополнения нулями или
      -- верхний регистр ломают не рендер, а уборку: свои файлы она узнаёт
      -- по этому шаблону, и каталог начал бы зарастать навсегда.
      testCase "хэш это восемь строчных hex" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          names <-
            mapM
              (\i -> takeFileName <$> renderStem defaultEnv dir (stemOf "s" ("v" <> show i) 0.001 (constant 0.1)))
              [1 .. 24 :: Int]
          let hashes = map (take 8 . drop 2) names
          mapM_ (\h -> assertBool h (length h == 8 && all (`elem` "0123456789abcdef") h)) hashes
          -- Иначе тест теряет зубы: без ведущего нуля потеря дополнения
          -- незаметна.
          assertBool "нет хэша с ведущим нулём" (any (("0" ==) . take 1) hashes)
    , -- Граница потолка: стем ровно в предел законен, на сэмпл длиннее нет.
      -- Частота игрушечная, иначе тест считал бы десять минут.
      testCase "стем ровно в предел проходит, длиннее нет" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let env = defaultEnv {envRate = 100, envBlock = 64, envMaxSec = 2}
          path <- renderStem env dir (stem "s" (takeSec 2 (constant 0.1)))
          (_, _, xs) <- readWav path
          U.length xs @?= 200
          r <- try (renderStem env dir (stem "long" (takeSec 2.02 (constant 0.1))))
          case r :: Either IOException FilePath of
            Left err -> assertBool (show err) ("long" `isInfixOf` show err)
            Right _ -> assertFailure "ожидали ошибку"
    , -- Потолок задаётся в Env: у честного трека длиннее умолчания должна
      -- быть возможность его поднять, иначе длина трека упирается в
      -- страховку от забытого окна.
      testCase "envMaxSec поднимает потолок" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let env = defaultEnv {envRate = 100, envBlock = 64, envMaxSec = 1}
          r <- try (renderStem env dir (stem "s" (takeSec 2 (constant 0.1))))
          case r :: Either IOException FilePath of
            Left _ -> pure ()
            Right _ -> assertFailure "ожидали ошибку по потолку"
          path <- renderStem env {envMaxSec = 3} dir (stem "s" (takeSec 2 (constant 0.1)))
          (_, _, xs) <- readWav path
          U.length xs @?= 200
    , -- Отвергнутый стем не должен оставаться на конечном пути: кэш считает
      -- существующий файл готовым и на следующем прогоне подмешал бы обрубок.
      testCase "провалившийся стем не остаётся готовым" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let env = defaultEnv {envRate = 100, envBlock = 64, envMaxSec = 1}
              s = cached "v1" (stem "s" (constant 0.1))
              failing = try (renderStem env dir s) :: IO (Either IOException FilePath)
          first <- failing
          second <- failing
          case (first, second) of
            (Left _, Left _) -> pure ()
            _ -> assertFailure "второй прогон не упал: обрубок остался в кэше"
          files <- listDirectory dir
          assertBool (show files) (not (any ((== ".wav") . takeExtension) files))
    , -- Стем упал - трек обязан упасть с ним: молча сведённый микс без партии
      -- не отличить от верного.
      testCase "провал стема валит весь трек" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let env = defaultEnv {envRate = 100, envBlock = 64, envMaxSec = 1}
              track =
                [ stem "good" (takeSec 0.5 (constant 0.1))
                , stem "endless" (constant 0.2)
                ]
          r <- try (renderTrack env (dir </> "mix.wav") track)
          case r :: Either IOException FilePath of
            Left err -> assertBool (show err) ("endless" `isInfixOf` show err)
            Right _ -> assertFailure "ожидали ошибку"
    , -- Тот же потолок нужен мастеру: обработка идёт после сведения и может
      -- сделать его бесконечным (+ constant), а это запись до предела WAV.
      testCase "бесконечный мастер это ошибка" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let env = defaultEnv {envRate = 100, envBlock = 64, envMaxSec = 1}
              mix = dir </> "mix.wav"
          r <- try (renderTrackWith env mix (bothChannels (+ constant 0.01)) [stem "one" (takeSec 0.5 (constant 0.1))])
          case r :: Either IOException FilePath of
            Left err -> assertBool (show err) ("мастер" `isInfixOf` show err)
            Right _ -> assertFailure "ожидали ошибку"
          doesFileExist mix >>= assertBool "остался обрубок мастера" . not
    , -- Мастер применяется к сумме, а не к каждому стему. С линейным
      -- усилением разницы нет, поэтому мастер нелинейный, а стемов два.
      testCase "мастер применяется к сумме, а не к стемам" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let track =
                [ stem "a" (takeSec 0.02 (constant 0.5))
                , stem "b" (takeSec 0.02 (constant 0.5))
                ]
          path <- renderTrackWith defaultEnv (dir </> "mix.wav") (bothChannels (\s -> s * s)) track
          (_, _, xs) <- readWav path
          -- Сумма по центру это 2 * 0.5 / sqrt 2, её квадрат 0.5. Поштучно
          -- вышло бы 2 * (0.5 / sqrt 2)^2 = 0.25.
          let l = xs U.! 800
          assertBool (show l) (abs (l - 0.5) < 1e-3)
    , -- Цена дизайна: длина не входит в ключ, поэтому кэш-файл прошлой длины
      -- задаёт длину мастера. Закрепляем, чтобы правка "брать длину из
      -- материала" не прошла молча.
      testCase "кэш-стем чужой длины задаёт длину мастера" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          _ <- renderStem defaultEnv dir (stemOf "one" "v1" 0.05 (constant 0.1))
          path <- renderTrack defaultEnv (dir </> "mix.wav") [stemOf "one" "v1" 0.02 (constant 0.1)]
          (_, _, xs) <- readWav path
          U.length xs @?= 2 * round (0.05 * rate)
    , -- Одно имя и одна спецификация у двух стемов это тот же конфликт, что
      -- и без кэша: путь от сигнала не зависит.
      testCase "повтор имени со спецификацией это ошибка" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let track =
                [ stemOf "one" "v1" 0.02 (constant 0.1)
                , stemOf "one" "v1" 0.02 (constant 0.2)
                ]
          r <- try (renderTrack defaultEnv (dir </> "mix.wav") track)
          case r :: Either IOException FilePath of
            Left err -> assertBool (show err) ("one" `isInfixOf` show err)
            Right _ -> assertFailure "ожидали ошибку"
    , -- Стем, пишущий по пути мастера, затирался бы каждым прогоном.
      testCase "стем по пути мастера это ошибка" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          r <- try (renderTrack defaultEnv (dir </> "mix.wav") [stem "mix" (takeSec 0.02 (constant 0.1))])
          case r :: Either IOException FilePath of
            Left err -> assertBool (show err) ("mix" `isInfixOf` show err)
            Right _ -> assertFailure "ожидали ошибку"
    , -- Уборка не трогает ни стемы без кэша, ни файлы с непохожим хэшем.
      -- Похожих файлов у стема без кэша больше, чем оставляет уборка: иначе
      -- они пережили бы её просто по свежести, и фильтр по кэшу остался бы
      -- непроверенным.
      testCase "уборка не трогает стемы без кэша" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let track =
                [ stemOf "one" "v2" 0.02 (constant 0.1)
                , stem "two" (takeSec 0.02 (constant 0.1))
                ]
              alien = ["two-1a2b3c4d.wav", "two-2a2b3c4d.wav", "two-3a2b3c4d.wav", "two-4a2b3c4d.wav", "two-5a2b3c4d.wav", "one-1a2b3c4d5.wav"]
          mapM_ (\f -> writeFile (dir </> f) "чужое") alien
          zipWithM_
            (\f i -> setModificationTime (dir </> f) (UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime i)))
            alien
            [1 ..]
          _ <- renderTrack defaultEnv (dir </> "mix.wav") track
          mapM_
            (\f -> doesFileExist (dir </> f) >>= assertBool ("снесли " <> f))
            alien
    , -- Несколько последних версий переживают уборку: рабочий цикл это
      -- метание между вариантами, и снос всего, кроме текущего, отменял бы
      -- кэш ровно там, где он нужен.
      testCase "уборка оставляет несколько последних версий" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let versions = ["v1", "v2", "v3", "v4", "v5"]
          paths <- mapM (\v -> renderStem defaultEnv dir (stemOf "one" v 0.002 (constant 0.1))) versions
          -- Время правим руками: файлы созданы в одну секунду, а уборка
          -- решает по свежести.
          zipWithM_
            (\p i -> setModificationTime p (UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime i)))
            paths
            [1 ..]
          _ <- renderTrack defaultEnv (dir </> "mix.wav") [stemOf "one" "v5" 0.002 (constant 0.1)]
          alive <- filterM doesFileExist paths
          -- Свежие четыре (v2..v5) на месте, самый старый убран.
          map takeFileName alive @?= map takeFileName (drop 1 paths)
    , -- Уборка разрушительна, поэтому обязана докладывать: молчаливое
      -- удаление кэша неотличимо от его отсутствия.
      testCase "уборка не молчит" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          old <- mapM (\v -> renderStem defaultEnv dir (stemOf "one" v 0.002 (constant 0.1))) ["v1", "v2", "v3", "v4"]
          zipWithM_
            (\p i -> setModificationTime p (UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime i)))
            old
            [1 ..]
          (_, msg) <-
            captureStderr (renderTrack defaultEnv (dir </> "mix.wav") [stemOf "one" "v5" 0.002 (constant 0.1)])
          assertBool msg ("убрано устаревших стемов" `isInfixOf` msg)
          assertBool msg (any (\o -> takeFileName o `isInfixOf` msg) (take 1 old))
    , -- Стем на другой частоте это чужой файл: подмешать его значило бы
      -- играть партию на другой скорости.
      testCase "чужая частота в стеме это ошибка" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          path <- renderStem defaultEnv {envRate = 24000} dir (stem "s" (takeSec 0.02 (constant 0.1)))
          r <- try (mixStems defaultEnv [path] >>= evaluate . U.sum . render defaultEnv)
          case r :: Either IOException Double of
            Left err -> assertBool (show err) ("частота" `isInfixOf` show err)
            Right _ -> assertFailure "ожидали ошибку"
    , -- Стерео-файл в стемах это чередованный поток: молча он дал бы кашу.
      testCase "стерео-файл в стемах это ошибка" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let path = dir </> "s.wav"
          _ <- writeWavStereo defaultEnv Float32 path (takeSec 0.02 (constant 0.1)) (takeSec 0.02 (constant 0.2))
          r <- try (mixStems defaultEnv [path] >>= evaluate . U.sum . render defaultEnv)
          case r :: Either IOException Double of
            Left err -> assertBool (show err) ("моно" `isInfixOf` show err)
            Right _ -> assertFailure "ожидали ошибку"
    , -- Пустой микс обязан быть конечным: литеральный ноль бесконечен и
      -- увёл бы запись в предел WAV.
      testCase "пустой микс конечен" $ do
        sig <- mixStems defaultEnv []
        U.length (render defaultEnv sig) @?= 0
    , -- Панорама вне диапазона это опечатка (panned 35 вместо 0.35), а
      -- молчаливый зажим её прятал бы.
      testCase "панорама вне диапазона это ошибка" $ do
        r <- try (evaluate (panned 35 (stem "x" (constant 0))))
        case r :: Either ErrorCall Stem of
          Left err -> assertBool (show err) ("панорама" `isInfixOf` show err)
          Right _ -> assertFailure "ожидали ошибку"
    ]
