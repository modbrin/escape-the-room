{-# LANGUAGE OverloadedStrings #-}
module ETR where
import EscapeTheRoom.LevelMaps
import CodeWorld
run :: IO ()

-- | World state - position of character and list of opened colored doors
data State = State Coords Int [TileColor] [LevelMap]

-- | Draw a given level map of size 21x21.
drawLevelMap :: (Coords -> Tile) -> Picture
drawLevelMap = drawMap (-10) (-10) 10 10

-- | Draw row on explicit coordinates from 'x0' to 'x' on fixed y = 'y0'.
-- * levelMap - mapping of tiles on given level
drawRow::Integer->Integer->Integer->(Coords->Tile)->Picture
drawRow x0 y0 x levelMap
  | x0 <= x = (drawTileAt (x0,y0) levelMap) <> (drawRow (x0 + 1) y0 x levelMap)
  | otherwise = blank

-- | Draw column of rows, i.e. matrix on given coordinates from left bottom.
-- | corner with coordinates ('x0','y0') and top right corner with coordinates ('x','y').
drawMap::Integer->Integer->Integer->Integer->(Coords->Tile)->Picture
drawMap x0 y0 x y levelMap
  | y0 <= y = (drawRow x0 y0 x levelMap) <> drawMap x0 (y0 + 1) x y levelMap
  |otherwise = blank

-- | Draw single tile at given coordinates based on mapping.
-- * (x,y) - coordinates of tile
-- * levelMap - mapping for tile type
drawTileAt :: (Integer, Integer)-> (Coords->Tile) -> Picture
drawTileAt (x, y) levelMap
  = translated xi yi (drawTile (levelMap (Coords x y)))
  where
    xi = fromIntegral x
    yi = fromIntegral y

-- | Definition for tile color.
tileColor :: TileColor -> Color
tileColor Red   = red
tileColor Green = green
tileColor Blue  = blue
tileColor Orange = orange
tileColor Pink = pink
tileColor Yellow = yellow
tileColor Purple = purple
tileColor Purple2 = purple
tileColor Black = black
tileColor Grey = gray 0.5
tileColor Brown = brown

-- | Definition for Tile enums as drawings.
drawTile :: Tile -> Picture
drawTile Wall        = wallTile
drawTile Exit        = exitTile
drawTile Floor       = floorTile
drawTile (Door dc)   = doorTile (tileColor dc)
drawTile (Button bc) = buttonTile (tileColor bc)

-- | Floor tile as a yellow square.
floorTile :: Picture
floorTile = colored yellow (solidRectangle 1 1)

-- | Wall tile as a black square.
wallTile :: Picture
wallTile = solidRectangle 1 1

-- | Door tile as a black square with a colored dot in center.
doorTile :: Color -> Picture
doorTile c
  = colored c (solidCircle 0.3)
 <> wallTile

-- | Button tile as a yellow square with a colored dot in center.
buttonTile :: Color -> Picture
buttonTile c
  = colored c (solidCircle 0.3)
 <> floorTile

-- | Exit tile as a 'door emoji'.
exitTile :: Picture
exitTile = colored pink (solidRectangle 1 1) <> floorTile

-- | Draw a character as 'human emoji' in a given coordinates.
drawPlayerAt :: Coords -> Picture
drawPlayerAt (Coords i j) = translated x y (text "웃") -- regular human emoji
  where                                              -- wouldn't work in browser
    x = fromIntegral i
    y = fromIntegral j

-- | Get 'Pos' variable from LevelMap
getPos::LevelMap->Coords
getPos (LevelMap pos _) = pos

-- | Get 'level' variable from LevelMap
getLevel::LevelMap->(Coords->Tile)
getLevel (LevelMap _ level) = level

-- | World state before game starts
initState :: State
initState = State (getPos (levelMaps!!0)) (-1) [] levelMaps

-- | Direction in cartesian axis
-- * U - up, i.e. X+1
-- * D - down, i.e. X-1
-- * L - left, i.e. Y+1
-- * R - right, i.e. Y-1
data Dir = U | D | L | R

-- | Translate a given coordinate by 1 in a given direction.
move :: Dir -> Coords -> Coords
move U (Coords x y) = Coords x (y + 1) -- translate up
move D (Coords x y) = Coords x (y - 1) -- translate down
move L (Coords x y) = Coords (x - 1) y -- translate left
move R (Coords x y) = Coords (x + 1) y -- translate right

-- | Event handler for keyboard arrows.
handleWorld :: Event -> State -> State
handleWorld (KeyPress "Up")    = tryMove U
handleWorld (KeyPress "Down")  = tryMove D
handleWorld (KeyPress "Left")  = tryMove L
handleWorld (KeyPress "Right") = tryMove R
handleWorld (KeyPress " ") = tryStart
handleWorld _event = id

-- | Check if level is complete, i.e. if coordinates map to 'Exit' tile.
isLevelComplete ::Coords->(Coords->Tile)->Bool
isLevelComplete pos level = level pos == Exit

-- | Increment level index inside state
nextLevel::State->State
nextLevel (State pos index list levels)
  = resetLevel (State pos (index + 1) list levels)

-- | Given the input of 'index' and limit 'bound' return value that fits in
-- | [0..bound-1], i.e. return index if value is inside bound and closest bound
-- | if not.
boundedLevelIndex::Int->Int->Int
boundedLevelIndex index bound
  | index > (bound - 1) = (bound - 1)
  | index < 0 = 0
  | otherwise = index


  -- | Given the input of 'index' and limit 'bound' return True if index is in
  -- | [0..bound) and false otherwise.
isInBound::Int->Int->Bool
isInBound index bound = (index < bound) && (index >= 0)

-- | Reset doors and player position on current level
resetLevel::State->State
resetLevel (State _ index _ levels)
  = (State initPos index [] levels)
   where initPos =  getPos (levelMaps!!(boundedLevelIndex index bound))
         bound = fromIntegral (length levels)

-- | Handle special events such as game start and level reset.
tryStart :: State -> State
tryStart state@(State _ index _ _)
  | index == -1 = nextLevel state
  | otherwise = resetLevel state

-- | Attempt to move in given direction 'dir' from coordinates 'coords',
-- | in case of failure same coordinates 'coords' entity is returned.
-- | Also handle stepping on the button during move.
tryMove :: Dir -> State -> State
tryMove dir state@(State pos index list levels)
  | isLevelComplete nextPos level = nextLevel state
  | canMove tile = State nextPos index (checkButton tile list) levels
  -- It would be good to add uniqueness addon, but since that is not a very huge problem
  | otherwise = state
    where
      level = getLevel (levels!!(boundedLevelIndex (index) bound))
      nextPos = move dir pos
      tile = openDoors list level nextPos
      bound = fromIntegral (length levels)

-- | delete all occurance of element from list
removeItem::(Eq a)=>a->[a]->[a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

 -- | Check if the tile is button and return its color, otherwise return empty array.
checkButton::Tile->[TileColor]->[TileColor]
checkButton (Button bc) list
  | bc `elem` list = removeItem bc list
  | otherwise = bc:list
checkButton _ list = list

-- | Check if player can move to given tile, walls and doors are prohibited.
canMove::Tile->Bool
canMove Wall     = False
canMove (Door _) = False
canMove _        = True

-- | Graphics update function, makes actual drawing of world.
drawWorld :: State -> Picture
drawWorld (State pos index list levels)
  | index == -1 = text "ESCAPE THE ROOM"
    <> translated 0 (-1) (dilated 0.7 (text "press <space> to reset level"))
    <> translated 0 (-2) (dilated 0.7 (text "use arrows for movement"))
    <> translated 0 (-3) (dilated 0.7 (text "now press <space> to start"))
  | isInBound index bound = dilated 0.7
    (drawPlayerAt pos <> drawLevelMap (openDoors list level))
  | otherwise = text "VICTORY"
    where level = getLevel (levels!!(boundedLevelIndex index bound))
          bound = fromIntegral (length levels)


-- | Given a list of colors 'colors', 'mapping' from coordinates to tile and actual
openDoors::[TileColor]->(Coords->Tile)->Coords->Tile
openDoors colors mapping coords
  | tile `elem` [Door dc|dc<-colors] = Floor
  | otherwise = tile
    where tile = mapping coords

-- | playLevel includes several levels with complete logic.
-- | Reset level and win by reaching end are implemented as well.
playLevel :: IO()
playLevel = interactionOf initState (\_->id) handleWorld drawWorld

run = playLevel
