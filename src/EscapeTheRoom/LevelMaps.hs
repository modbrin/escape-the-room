-- | Level maps for Escape the Room.
module EscapeTheRoom.LevelMaps where

-- | Tiles used in Escape the Room game.
data Tile
  = Wall                          -- ^ An unpassable wall.
  | Floor                         -- ^ Floor to walk on.
  | Door TileColor                -- ^ Door of a given color.
  | Exit                          -- ^ An exit.
  | Button TileColor deriving Eq  -- ^ A button that opens/toggles

-- | Available door and button colors.
data TileColor
  -- standard colors
  = Red | Green | Blue
  -- extra colors, used in some levels
  | Orange | Pink | Yellow | Purple | Purple2 | Black | Grey | Brown deriving Eq

-- | Coordinates on a level map.
data Coords = Coords Integer Integer

-- | A level map with initial coordinates.
data LevelMap = LevelMap Coords (Coords -> Tile)

-- | A list of all level maps.
levelMaps :: [LevelMap]
levelMaps =
  [ levelMap1     -- Author: Nikita Alekseev
  , levelMap2     -- Author: Pavel Bashkirov
  , levelMap3     -- Author: Denis Chernikov
  , levelMap5     -- Author: Iurii Gavrilin
  , levelMap7     -- Author: Vladislav Kuleykin
  , levelMap9     -- Author: Boris Makaev
  , levelMap10    -- Author: Bulat Maksudov
  , levelMap11    -- Author: Ilgiz Mustafin
  , levelMap12    -- Author: Ivan Panchenko
  , levelMap13    -- Author: Gheorghe Pinzaru
  , levelMap14    -- Author: Robert Sayakhov
  , levelMap15    -- Author: Maxim Surkov
  -- , levelMap16    -- Author: Oleg Taizov
  , levelMap17    -- Author: Ivan Tkachenko
  , levelMap18    -- Author: Damir Tuktarov
  ]


-- | Author: Nikita Alekseev
levelMap1 :: LevelMap
levelMap1 = LevelMap (Coords 0 0) maze
  where
    maze (Coords i j)
      | abs i > 9 || abs j > 9 = Wall
      | i == 5 && j == 8 = Door Red
      | i == -8 && j == -8 = Button Red
      | i == 8 && j == 1 = Button Blue
      | i == -6 && j == -7 = Door Blue
      | i == 5 && j < 3 && j > -7 = Wall
      | i == 5 && j > 4 = Wall
      | i > -7 && j == 4 = Wall
      | i == -7 && j > -4 && j < 5 = Wall
      | i == -6 && j < -6 = Wall
      | i < 5 && j == -6 = Wall
      | (i, j) == (7, 7) = Exit
      | otherwise = Floor

-- | Author: Pavel Bashkirov
levelMap2 :: LevelMap
levelMap2 = LevelMap (Coords 0 0) maze
  where
    maze (Coords i j)
      -- edge walls
      | abs i > 10 || abs j > 10 = Wall
      -- vertical walls
      | i == -1 && j > 8 = Wall
      | i == -1 && j <= 7 && j >= 0 = Wall
      | i == 1 && j >= -1 = Wall
      | i == 9 && j <= 9 && j >= -9 = Wall
      | i == -9 && j >= -8 && j <= -3 = Wall
      | i == -7 && j >= -7 && j <= -3 = Wall
      | i == -1 && j >= -9 && j <= -8 = Wall
      | i == 1 && j >= -8 && j <= -7 = Wall
      | i == 7 && j >= -7 && j <= -3 = Wall
      -- horizontal walls
      | i < 0 && j == -1 = Wall
      | i > 2 && i <= 9 && j == 9 = Wall
      | i >= -9 && i <= 9 && j == -9 = Wall
      | i >= -9 && i <= -1 && j == -1 = Wall
      | i >= 1 && i <= 9 && j == -1 = Wall
      | i >= -8 && i <= 7 && j == -3 = Wall
      -- exits, buttons and doors
      | (i, j) == (3, 3) = Exit
      | (i, j) == (2, 9) = Door Red
      | (i, j) == (8, -3) = Door Green
      | (i, j) == (-8, -4) = Button Red
      | (i, j) == (-9, 0) = Button Green
      | otherwise = Floor

-- | Author: Denis Chernikov
levelMap3 :: LevelMap
levelMap3 = LevelMap (Coords (-8) (-8)) maze
  where
    maze (Coords i j)
      | abs i > 8 || abs j > 8 = Wall
      | (i, j) == (-6, -6) = Button Orange
      | (i, j) == (0, -6) = Button Pink
      | (i, j) == (6, -6) = Button Blue
      | (i, j) == (-2, -2) = Button Yellow
      | (i, j) == (-6, 0) = Button Blue
      | (i, j) == (0, 0) = Button Red
      | (i, j) == (6, 0) = Button Purple
      | (i, j) == (-6, 6) = Button Purple2
      | (i, j) == (0, 6) = Button Green
      | (i, j) == (-3, -6) = Door Blue
      | (i, j) == (3, -6) = Door Red
      | (i, j) == (-6, -3) = Door Orange
      | (i, j) == (0, -3) = Door Purple2
      | (i, j) == (6, -3) = Door Green
      | (i, j) == (-3, 0) = Door Pink
      | (i, j) == (3, 0) = Door Green
      | (i, j) == (-6, 3) = Door Red
      | (i, j) == (0, 3) = Door Purple
      | (i, j) == (6, 3) = Door Red
      | (i, j) == (-3, 6) = Door Yellow
      | (i, j) == (3, 6) = Door Yellow
      | (i, j) == (6, 6) = Exit
      | abs i == 3 || abs j == 3 = Wall
      | otherwise = Floor

-- | Author: Iurii Gavrilin
levelMap5 :: LevelMap
levelMap5 = LevelMap (Coords (-5) (-5)) maze
  where
    maze (Coords i j)
      | abs i > 10 || abs j > 10 = Wall
      | (i, j) == (0, 0) = Exit
      | (i, j) == (-5, 5) = Button Blue
      | (i, j) == ( 5, 5) = Button Green
      | (i, j) == ( 5,-5) = Button Red
      | i == 0 && abs j == 8 = Door Blue
      | abs i == 3 && j == 0 = Door Green
      | abs i == 2 && j == 0 = Door Red
      | abs i < 2 && abs j < 2 = Door Blue
      | (i, j) == (5, 6) = Door Red
      | (i, j) == (-5,-5) = Floor
      | (i, j) == (6, -5) = Floor
      | (i, j) == (-6, 5) = Floor
      | (i, j) == (-8, 0) = Floor
      | (i, j) == (-5,-6) = Floor
      | abs i < 3 && abs j < 3 = Wall
      | abs i < 7 && abs j < 7 && abs i > 3 && abs j > 3 = Wall
      | j == 0 = Wall
      | i == 0 = Wall
      | otherwise = Floor

-- | Author: Vladislav Kuleykin
levelMap7 :: LevelMap
levelMap7 = LevelMap (Coords 0 2) maze
  where
    maze (Coords i j)
      | abs i > 9 || abs j > 9                            = Wall
      | i == -7 && isIn j [-8,-7,-6,-5,-4]                = Wall
      | (i,j) == (-6,-6)                                  = Wall
      | i == -5 && isIn j [-8,-7,-5,-4,5,6,7,8,9]         = Wall
      | i == -3 && isIn j [-8,-7,-6,-5,-4,5,6,7,8,9]      = Wall
      | i == -2 && isIn j [-8,-6,-4,0,1]                  = Wall
      | i == -1 && isIn j [-8,-4,-1,2,5,6,7]              = Wall
      | i ==  0 && isIn j [-2,6,8]                        = Wall
      | i ==  1 && isIn j [-8,-7,-6,-5,-4,-1,2,5,6,7,8,9] = Wall
      | i ==  2 && isIn j [-8,0,1]                        = Wall
      | i ==  3 && isIn j [-8,5,7,8,9]                    = Wall
      | i ==  4 && isIn j [5,7,9]                         = Wall
      | i ==  5 && isIn j [-8,-7,-6,-5,-4,5,6,7,9]        = Wall
      | j ==  0 && not (isIn i [-3,-1,0,1,3])             = Door Black
      | j ==  0 && isIn i [-3,3]                          = Door Orange
      | j ==  5 && not (isIn i [-7,-4,-2,0,6])            = Door Black
      | j == -8 && not (isIn i [-9,-6,5,6,7])             = Door Black
      | j == -8 && isIn i [6,7]                           = Wall
      | j == 5 && isIn i [-7,6]                           = Door Blue
      | (i,j) == (-2,5)                                   = Door Yellow
      | (i,j) == (4,8)                                    = Button Orange
      | (i,j) == (0,5)                                    = Button Blue
      | (i,j) == (9,-9)                                   = Button Black
      | (i,j) == (-9,9)                                   = Button Yellow
      | (i,j) == (-4,7)                                   = Wall
      | (i,j) == (0, 9)                                   = Button Red
      | (i,j) == (0, 1)                                   = Door Red
      | (i,j) == (0,0)                                    = Exit
      | otherwise                                         = Floor

    isIn = elem

-- | Author: Boris Makaev
levelMap9 :: LevelMap
levelMap9 = LevelMap (Coords (-10) 10) maze
  where
    maze (Coords i j)
      | abs i > 10 || abs j > 10 = Wall
      | (i, j) == (8, -8) = Exit
      | i == -7 && j /= 5 = Wall
      | i < -7 && j == -7 && i /= -9 = Wall
      | i > 6 && j == -6 = Wall
      | i == 6 && j <= -6 && j /= -9 = Wall
      | i == -5 && j <= 7 && j >= -4 = Wall
      | j == 7 && i >= -5 = Wall
      | i == -2 && j <= 9 && j >= 8 = Wall
      | i == 0 && j >8 = Wall
      | i == 2 && j <= 9 && j >= 8 = Wall
      | i == 4 && j >8 = Wall
      | i == -3 && j < 6 = Wall
      | (i, j) == (-9, -7) = Door Red
      | (i, j) == (6, -9) = Door Blue
      | (i, j) == (10, 10) = Button Red
      | (i, j) == (-9, -9) = Button Blue
      | otherwise = Floor

-- | Author: Bulat Maksudov
levelMap10 :: LevelMap
levelMap10 = LevelMap (Coords 0 0) maze
  where
    maze (Coords i j)
      | abs i > 9 || abs j > 9 = Wall
      | (i, j) == (1, 2) = Door Green
      | (i, j) == (3, 3) = Exit
      | (i, j) == (-1,2) = Button Red
      | (i,j) == (-3,-2) = Door Red
      | (i,j) == (-5,-5) = Button Blue
      | (i,j) == (1,-3) = Door Blue
      | (i,j) == (3,-5) = Button Green
      | i == 1 = Wall
      | j == -2 = Wall
      | otherwise = Floor

-- | Author: Ilgiz Mustafin
levelMap11 :: LevelMap
levelMap11 = LevelMap (Coords 0 0) maze
  where
    maze (Coords i j)
      | (i, j) == (0, 5) = Exit
      | (i, j) == (0, 2) = (Button Red)
      | (i, j) == (0, 1) = Floor
      | (i, j) == (2, 0) = (Door Red)
      | (i, j) == (-2, 0) = (Button Green)
      | (i, j) == (-2, 2) = (Door Green)
      | (i, j) == (-3, 2) = (Door Green)
      | (i, j) == (-3, 0) = (Button Blue)
      | (i, j) == (-2, 1) = (Door Blue)
      | (i, j) == (-3, 1) = (Door Blue)
      | 1 <= i && i <= 3 && j == 5 = (Door Red)
      | j == 0 && -2 <= i && i <= 2 = Floor
      | i == 3 && 0 <= j && j <= 5 = (Door Red)
      | j == 5 && -2 <= i && i <= 2 = Floor
      | i == -2 && 0 <= j && j <= 5 = Floor
      | i == -3 && 0 <= j && j <= 5 = Floor
      | (i, j) == (-1, 2) = Floor
      | j < ((i `quot` 2)^(2 :: Integer) - 6) = Exit
      | otherwise = Wall

-- | Author: Ivan Panchenko
levelMap12 :: LevelMap
levelMap12 = LevelMap (Coords 0 0) maze
  where
    maze (Coords i j)
      | abs i > 9 || abs j > 9 = Wall
      | i == (-9) && j == (-2) = Wall
      | i == (-9) && j == (8) = Wall
      | i == (-9) && j == (9) = Button Grey
      | i == (-8) && j == (-8) = Wall
      | i == (-8) && j == (-7) = Wall
      | i == (-8) && j == (-6) = Wall
      | i == (-8) && j == (-5) = Wall
      | i == (-8) && j == (-4) = Wall
      | i == (-8) && j == (0) = Wall
      | i == (-8) && j == (1) = Wall
      | i == (-8) && j == (2) = Wall
      | i == (-8) && j == (3) = Wall
      | i == (-8) && j == (4) = Wall
      | i == (-8) && j == (5) = Wall
      | i == (-8) && j == (6) = Wall
      | i == (-8) && j == (8) = Wall
      | i == (-7) && j == (-4) = Wall
      | i == (-7) && j == (-3) = Wall
      | i == (-7) && j == (-2) = Wall
      | i == (-7) && j == (-1) = Wall
      | i == (-7) && j == (0) = Wall
      | i == (-7) && j == (6) = Wall
      | i == (-7) && j == (8) = Wall
      | i == (-6) && j == (-9) = Wall
      | i == (-6) && j == (-8) = Wall
      | i == (-6) && j == (-7) = Wall
      | i == (-6) && j == (-6) = Wall
      | i == (-6) && j == (-5) = Door Blue
      | i == (-6) && j == (-4) = Wall
      | i == (-6) && j == (4) = Wall
      | i == (-6) && j == (6) = Wall
      | i == (-6) && j == (8) = Wall
      | i == (-6) && j == (9) = Door Pink
      | i == (-5) && j == (-8) = Button Green
      | i == (-5) && j == (-7) = Wall
      | i == (-5) && j == (-2) = Wall
      | i == (-5) && j == (-1) = Wall
      | i == (-5) && j == (0) = Wall
      | i == (-5) && j == (1) = Wall
      | i == (-5) && j == (2) = Wall
      | i == (-5) && j == (4) = Wall
      | i == (-5) && j == (6) = Wall
      | i == (-4) && j == (-8) = Wall
      | i == (-4) && j == (-7) = Wall
      | i == (-4) && j == (-5) = Wall
      | i == (-4) && j == (-4) = Wall
      | i == (-4) && j == (-3) = Wall
      | i == (-4) && j == (-2) = Wall
      | i == (-4) && j == (2) = Wall
      | i == (-4) && j == (4) = Wall
      | i == (-4) && j == (6) = Wall
      | i == (-4) && j == (7) = Wall
      | i == (-4) && j == (8) = Wall
      | i == (-3) && j == (-7) = Wall
      | i == (-3) && j == (-3) = Wall
      | i == (-3) && j == (0) = Door Purple
      | i == (-3) && j == (4) = Wall
      | i == (-3) && j == (7) = Button Blue
      | i == (-3) && j == (8) = Wall
      | i == (-2) && j == (-9) = Wall
      | i == (-2) && j == (-7) = Wall
      | i == (-2) && j == (-5) = Wall
      | i == (-2) && j == (-3) = Wall
      | i == (-2) && j == (-1) = Wall
      | i == (-2) && j == (0) = Door Brown
      | i == (-2) && j == (1) = Wall
      | i == (-2) && j == (2) = Wall
      | i == (-2) && j == (4) = Wall
      | i == (-2) && j == (6) = Wall
      | i == (-2) && j == (7) = Door Red
      | i == (-2) && j == (8) = Wall
      | i == (-1) && j == (-7) = Wall
      | i == (-1) && j == (-5) = Wall
      | i == (-1) && j == (-1) = Wall
      | i == (-1) && j == (0) = Exit
      | i == (-1) && j == (1) = Wall
      | i == (-1) && j == (4) = Wall
      | i == (-1) && j == (6) = Wall
      | i == (-1) && j == (8) = Wall
      | i == (0) && j == (-9) = Door Orange
      | i == (0) && j == (-8) = Wall
      | i == (0) && j == (-7) = Wall
      | i == (0) && j == (-6) = Wall
      | i == (0) && j == (-5) = Wall
      | i == (0) && j == (-4) = Wall
      | i == (0) && j == (-2) = Wall
      | i == (0) && j == (-1) = Wall
      | i == (0) && j == (0) = Wall
      | i == (0) && j == (1) = Wall
      | i == (0) && j == (3) = Wall
      | i == (0) && j == (4) = Wall
      | i == (0) && j == (6) = Wall
      | i == (0) && j == (7) = Button Pink
      | i == (0) && j == (8) = Wall
      | i == (0) && j == (9) = Button Orange
      | i == (1) && j == (-1) = Wall
      | i == (1) && j == (3) = Wall
      | i == (1) && j == (4) = Wall
      | i == (1) && j == (6) = Wall
      | i == (1) && j == (7) = Wall
      | i == (1) && j == (8) = Wall
      | i == (1) && j == (9) = Wall
      | i == (2) && j == (-8) = Wall
      | i == (2) && j == (-7) = Wall
      | i == (2) && j == (-6) = Wall
      | i == (2) && j == (-4) = Wall
      | i == (2) && j == (0) = Wall
      | i == (2) && j == (1) = Wall
      | i == (2) && j == (6) = Door Grey
      | i == (3) && j == (-7) = Wall
      | i == (3) && j == (-4) = Wall
      | i == (3) && j == (-3) = Wall
      | i == (3) && j == (-2) = Wall
      | i == (3) && j == (1) = Wall
      | i == (3) && j == (2) = Wall
      | i == (3) && j == (3) = Wall
      | i == (3) && j == (4) = Wall
      | i == (3) && j == (5) = Wall
      | i == (3) && j == (6) = Wall
      | i == (3) && j == (7) = Wall
      | i == (3) && j == (8) = Wall
      | i == (4) && j == (-9) = Wall
      | i == (4) && j == (-7) = Wall
      | i == (4) && j == (-5) = Wall
      | i == (4) && j == (-4) = Wall
      | i == (4) && j == (-3) = Wall
      | i == (4) && j == (0) = Door Green
      | i == (4) && j == (4) = Wall
      | i == (4) && j == (8) = Wall
      | i == (5) && j == (-5) = Wall
      | i == (5) && j == (-4) = Wall
      | i == (5) && j == (-1) = Wall
      | i == (5) && j == (2) = Wall
      | i == (5) && j == (4) = Wall
      | i == (5) && j == (6) = Wall
      | i == (5) && j == (8) = Wall
      | i == (6) && j == (-9) = Wall
      | i == (6) && j == (-8) = Wall
      | i == (6) && j == (-7) = Wall
      | i == (6) && j == (-6) = Wall
      | i == (6) && j == (-5) = Wall
      | i == (6) && j == (-2) = Wall
      | i == (6) && j == (1) = Wall
      | i == (6) && j == (4) = Wall
      | i == (6) && j == (6) = Wall
      | i == (6) && j == (8) = Wall
      | i == (7) && j == (-9) = Button Brown
      | i == (7) && j == (-3) = Wall
      | i == (7) && j == (0) = Wall
      | i == (7) && j == (3) = Wall
      | i == (7) && j == (4) = Wall
      | i == (7) && j == (6) = Wall
      | i == (7) && j == (7) = Button Purple
      | i == (7) && j == (8) = Wall
      | i == (8) && j == (-9) = Wall
      | i == (8) && j == (-8) = Wall
      | i == (8) && j == (-7) = Wall
      | i == (8) && j == (-6) = Wall
      | i == (8) && j == (-5) = Wall
      | i == (8) && j == (-4) = Wall
      | i == (8) && j == (-1) = Wall
      | i == (8) && j == (2) = Wall
      | i == (8) && j == (3) = Wall
      | i == (8) && j == (4) = Wall
      | i == (8) && j == (6) = Wall
      | i == (8) && j == (7) = Wall
      | i == (8) && j == (8) = Wall
      | i == (9) && j == (-9) = Button Red
      | i == (9) && j == (-2) = Wall
      | i == (9) && j == (1) = Wall
      | i == (9) && j == (2) = Wall
      | otherwise = Floor

-- | Author: Gheorghe Pinzaru
levelMap13 :: LevelMap
levelMap13 = LevelMap (Coords 0 0) maze
  where
    maze (Coords i j)
      | i == 5 && j == 6 = Door Blue
      | i == 1 && j == 9 = Button Blue
      | i == 5 && j == (-8) = Door Red
      | i == 3 && j == 9 = Button Red
      | i == (-5) && j == 2 = Door Green
      | i == 2 && j == 9 = Button Green
      | i == (-8) && j == (-9) = Exit
      | i > (-5) && i < 1 && j == (-9) = Wall
      | abs i > 10 || abs j > 10 = Wall
      | i > 8  && j == 3 = Wall
      | i == 6 && j == 3 = Wall
      | i == 5 = Wall
      | i < 5 && i > (-1) && j == (-5) = Wall
      | i == (-1) && j >= (-5) = Wall
      | i == (-5) = Wall
      | otherwise = Floor

-- | Author: Robert Sayakhov
levelMap14 :: LevelMap
levelMap14 = LevelMap (Coords 0 0) maze
  where
    maze (Coords x y)
      | x == 3  && y == 5                 = Door Red
      | x == -6 && y == -4                = Door Blue
      | x == -4 && y == -8                = Button Red
      | x == -4 && y == 8                 = Button Blue
      | x == -8 && y == 8                 = Exit
      | abs x > 9 || abs y > 9            = Wall
      | x == -6                           = Wall
      | x > -6  && y == 5                 = Wall
      | x == -2 && y < 3                  = Wall
      | (x > - 2 && x < 2) && y == 2      = Wall
      | x == 1 && (y > -9  && y < 2)      = Wall
      | (x > 1 && x < 9) && y == -8       = Wall
      | otherwise                         = Floor

-- | Author: Maxim Surkov
--
-- NOTE: more interesting with buttons working as toggles (open/close).
levelMap15 :: LevelMap
levelMap15 = LevelMap (Coords (-8) 8) maze
  where
    maze (Coords i jm)
      | i == (-8) && jm == (-8) = Exit
      | (i == -5 && j==8)||(i == 0 && j==8)||(i == 5 && j==6) = Button Red
      | (i == -5 && j==7)||(i == 0 && j==6)||(i == 5 && j==8) = Button Green
      | (i == -5 && j==6)||(i == 0 && j==7)||(i == 5 && j==7) = Button Blue
      | i < 10 && i > 6 && j == 8 = Door Red
      | i < 10 && i > 6 && j == 7 = Door Green
      | i < 10 && i > 6 && j == 6 = Door Blue
      | i == 5 && j<5 && j>0 = Door Red
      | i == 1 && j<5 && j>0 = Door Green
      | i == (-3) && j<5 && j>0 = Door Blue
      | i == (-7) && j<5 && j>0 = Door Red
      | (i==8||i==6||i==(-1))&&j==2 = Button Red
      | (i==4||i==2||i==7||i==(-5))&&j==2 = Button Green
      | (i==0||i==(-2)||i==3)&&j==2 = Button Blue
      | (i==(-4)||i==(-6))&&j==2 = Button Red
      | i==(-9) && (j==1||j==(0)) = Floor
      | i ==9 && j>2 && j<6 = Floor
      | abs i > 9 || abs j > 9 = Wall
      | (i == -6 || i==(-4) || i==(-1) || i==1 || i==4||i==6)&& j>5 &&j<9 = Wall
      | (j < 6 && j>2) || (j<2 && j>(-1)) = Wall
      | i < -5 && j == 6 = Wall
      | otherwise = Floor
        where
          j = abs jm

-- | Author: Oleg Taizov
levelMap16 :: LevelMap
levelMap16 = LevelMap (Coords 8 8) maze
  where
    maze (Coords i j)
      | (i, j) == (6, 6) = Door Red
      | (i, j) == (6, 2) = Door Green
      | (i, j) == (9, 7) = Floor
      | (i, j) == (19, 16) = Door Blue
      | i == 0 || i == 21 || i == 6 || j == 0 || j == 21 || (i < 6 && j == 3)
        || (i == 17 && j >= 16) || (i >=17 && j == 16) || (i >= 13 && j == 7)
        || (i,j) == (12,7) || (i,j) == (11, 6) || (i, j) == (10, 5) = Wall
      | (i, j) == (1, 1) = Exit
      | otherwise = Floor

-- | Author: Ivan Tkachenko
levelMap17 :: LevelMap
levelMap17 = LevelMap (Coords 0 0) maze
  where
    maze (Coords x y)
      | (x, y) == (3, 3) = Exit
      | (x, y) == (1, 2) = Door Blue
      | (x, y) == (-2, -2) = Door Red
      | (x, y) == (-7, 6) = Button Red
      | (x, y) == (8, -2) = Button Blue
      | x == (-10) || y == (-10) || x == 10 || y == 10 = Wall
      | y ==  0 && x  >  0 = Wall
      | y == -2 && x <=  1 = Wall
      | x ==  1 && y /= -7 = Wall
      | x == -4 && y >= -2 && y /= 3 = Wall
      | otherwise = Floor

-- | Author: Damir Tuktarov
levelMap18 :: LevelMap
levelMap18 = LevelMap (Coords 0 0) maze
  where
    maze (Coords i j)
      | abs i > 9 || abs j > 9 = Wall
      | (i, j) == (1, 2) = Door Blue
      | (i, j) == (7, -2) = Button Blue
      | (i, j) == (-1, -1) = Door Red
      | (i, j) == (-7, 4) = Button Red
      | i == 1 && j /= -6 = Wall
      | i == -4 && j > -2 && j /= 3 = Wall
      | j == 1 && i > 1 = Wall
      | j == -1 && i < 1 = Wall
      | i == 4 && j == 4 = Exit
      | otherwise = Floor
