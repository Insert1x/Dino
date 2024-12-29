import System.IO (hFlush, stdout, hSetBuffering, BufferMode(NoBuffering), stdin, hReady)
import Control.Concurrent (threadDelay)
import Control.Monad (when)

-- Размеры игрового поля
fieldWidth :: Int
fieldWidth = 30

fieldHeight :: Int
fieldHeight = 9

-- Динозаврик
data Dino = Dino { position :: Int, isJumping :: Bool, jumpHeight :: Int }

-- Препятствие
data Obstacle = Obstacle { obstaclePosition :: Int }

-- Инициализация игры
initGame :: IO (Dino, Obstacle)
initGame = return (Dino { position = 5, isJumping = False, jumpHeight = 0 }, Obstacle fieldWidth)

-- Вывод игрового поля
draw :: Dino -> Obstacle -> IO ()
draw dino obstacle = do
    -- Очистка экрана
    putStr "\ESC[2J\ESC[H"
    -- Рисуем верхнюю рамку
    putStrLn $ "#" ++ replicate fieldWidth '#' ++ "#"
    -- Рисуем игровое поле с боковыми рамками
    mapM_ (putStrLn . generateLine dino obstacle) [0 .. fieldHeight - 1]
    -- Рисуем нижнюю рамку
    putStrLn $ "#" ++ replicate fieldWidth '#' ++ "#"
    -- Делаем паузу перед следующим кадром
    threadDelay 40000  -- 0. секунды

generateLine :: Dino -> Obstacle -> Int -> String
generateLine (Dino dinoPos _ jumpHeight) (Obstacle obsPos) row
    -- Земля с препятствием
    | row == fieldHeight - 2 =
        let line = replicate fieldWidth ' '
            lineWithObstacle = replaceAt (obsPos - 1) 'X' line
        in if row == (fieldHeight - 2 - jumpHeight)
              then "#" ++ replaceAt (dinoPos - 1) 'D' lineWithObstacle ++ "#"
              else "#" ++ lineWithObstacle ++ "#"
    -- Прыжок динозаврика
    | row == (fieldHeight - 2 - jumpHeight) =
        "#" ++ replaceAt (dinoPos - 1) 'D' (replicate fieldWidth ' ') ++ "#"
    -- Земля
    | row == fieldHeight - 1 = "#" ++ replicate fieldWidth '-' ++ "#"
    -- Пустое пространство
    | otherwise = "#" ++ replicate fieldWidth ' ' ++ "#"


-- Вспомогательная функция для замены символа в строке
replaceAt :: Int -> Char -> String -> String
replaceAt idx char str = take idx str ++ [char] ++ drop (idx + 1) str

-- Обработчик ввода (прыжок динозаврика)
handleInput :: Maybe Char -> Dino -> Dino
handleInput (Just ' ') dino = dino { isJumping = True }  -- Начать прыжок при нажатии пробела
handleInput _ dino = dino  -- Игнорируем любой другой ввод

-- Обновление состояния игры с проверкой столкновения
updateGame :: Dino -> Obstacle -> Int -> IO (Dino, Obstacle,Int, Bool)
updateGame (Dino pos jumping jumpHeight) (Obstacle obsPos) score = do
    -- Обновление высоты прыжка
    let (newJumping, newJumpHeight) = if jumping
            then if jumpHeight < 3 then (True, jumpHeight + 1) else (False, jumpHeight - 1)
            else if jumpHeight > 0 then (False, jumpHeight - 1) else (False, 0)

    -- Двигаем препятствие
    let newObsPos = if obsPos > 0 then obsPos - 1 else fieldWidth
    let newObstacle = Obstacle newObsPos

     -- Увеличиваем счёт при прохождении препятствия
    let newScore = if obsPos == 4 then score + 1 else score

    -- Проверка на столкновение (только если динозаврик на земле)
    let collision = pos == newObsPos && newJumpHeight == 0
    return (Dino pos newJumping newJumpHeight, newObstacle, newScore, collision)

-- Основной игровой цикл
gameLoop :: Dino -> Obstacle -> Int -> Int -> IO ()
gameLoop dino obstacle score speed = do
    -- Получаем ввод
    input <- getInput
    let updatedDino = handleInput input dino

    -- Отображаем игровое поле
    draw updatedDino obstacle
    putStrLn $ "Score: " ++ show score

    -- Обновляем состояние игры
    (nextDino, updatedObstacle, newScore, collision) <- updateGame updatedDino obstacle score

    -- Проверяем на столкновение
    if collision
        then putStrLn "Game Over! Dino hit an obstacle!"
         else do
            -- Увеличиваем скорость каждые 2 пройденных препятствий
            let newSpeed = if newScore `mod` 2 == 0 && newScore > score
                           then max 1000 (speed - 10000)
                           else speed

            -- Отладочная информация для отслеживания скорости
            putStrLn $ "New Speed: " ++ show newSpeed

            threadDelay newSpeed
            gameLoop nextDino updatedObstacle newScore newSpeed

-- Получение ввода с клавиатуры 
getInput :: IO (Maybe Char)
getInput = do
    available <- hReady stdin
    if available
        then Just <$> getChar
        else return Nothing

-- Главная функция
main :: IO ()
main = do
    hSetBuffering stdin NoBuffering  -- Отключение буферизации для корректного ввода
    -- Инициализация игры
    (dino, obstacle) <- initGame
    -- Запуск игрового цикла
    let initialSpeed = 50000  -- Стартовая задержка (0.05 секунды)
    gameLoop dino obstacle 0 initialSpeed
