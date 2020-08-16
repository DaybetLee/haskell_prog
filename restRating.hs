--
-- MATHFUN - DISCRETE MATHEMATICS AND FUNCTIONAL PROGRAMMING
-- Functional Programming Assignment, 2020/7
-- Possible Solution (for functionality only)
--
import System.IO
import Text.Printf
import Data.Char

--
-- Types
--
type Rest_ID = String
type Rest_Name = String
type Rest_Area = String
type Rest_Star = Int
type Chef = String
type Score = Int
type Rest_Rating = (Chef, Score)

data Rest = Rest Rest_ID Rest_Name Rest_Area Rest_Star [Rest_Rating]
            deriving (Eq, Show, Read)

--
-- Test database
--
testDatabase :: [Rest] 
testDatabase =
    [
        Rest "01" "Alma by Juan Amador" "Orchard" 3
            [("Bryan",7), ("Petrina",9), ("Justin",4), ("Janice",7), ("Jason",4)],
        Rest "02" "Basque Kitchen by Aitor" "Tanjong Pagar" 1 
            [("Colin",9), ("Nixon",5), ("Julien",7), ("Cheryl",8), ("Sam",8)],
        Rest "03" "Beni" "Orchard" 1 
            [("Julien",9), ("Cheryl",6), ("Justin",4), ("Janice",5), ("Jason",8)],
        Rest "04" "Les Amis" "Orchard" 3
            [("Colin",6), ("Nixon",7),("Bryan",6), ("Petrina",9),  ("Sam",5)],
        Rest "05" "Burnt Ends" "Chinatown" 1
            [("Bryan",4), ("Petrina",8),("Colin",4), ("Nixon",7), ("Sam",5)],
        Rest"06" "Buona Terra" "Newton" 1
            [("Julien",5), ("Cheryl",8), ("Bryan",7), ("Petrina",9), ("Justin",4), ("Janice",7), ("Jason",4)],
        Rest "07" "Candlenut Kitchen" "Tanglin" 1
            [("Colin",8), ("Nixon",9), ("Bryan",7), ("Petrina",9), ("Sam",9)],
        Rest "08" "Cheek Bistro" "Tanjong Pagar" 1
            [("Julien",5), ("Cheryl",5),  ("Justin",4), ("Janice",8), ("Jason",4)],
        Rest "09" "Chef Kang's" "Rochor" 1
            [("Colin",4),  ("Sam",7), ("Petrina",6)],
        Rest "10" "Corner House" "Tanglin" 1
            [("Bryan",7),("Colin",4),("Nixon",5), ("Nixon",7), ("Sam",5)],
        Rest "11" "CUT by Wolfgang Puck" "Marina Bay" 1
            [("Julien",5), ("Cheryl",2), ("Petrina",9), ("Janice",7), ("Jason",4)],
        Rest "12" "Garibaldi Italian Restaurant and Bar" "City Hall" 1
            [("Colin",4), ("Nixon",7), ("Sam",5), ("Justin",4), ("Bryan",6)],
        Rest "13" "Hill Street Tai Hwa Pork Noodle" "Kallang" 1
            [("Julien",5), ("Cheryl",8), ("Bryan",7), ("Petrina",9), ("Justin",4)],
        Rest "14" "Iggy's" "Orchard" 1
            [("Colin",4), ("Nixon",4), ("Janice",6), ("Jason",4), ("Sam",5)],
        Rest "15" "Imperial Treasure Fine Teochew Cuisine" "Orchard" 1
            [("Bryan",7), ("Petrina",9),("Colin",4), ("Nixon",7), ("Sam",5)],
        Rest "16" "Jaan" "City Hall" 1
            [("Julien",5), ("Cheryl",8),  ("Justin",4), ("Janice",6), ("Jason",4)],
        Rest "17" "Restaurant JAG" "Outram" 1
            [("Colin",4), ("Bryan",8), ("Petrina",8), ("Nixon",7), ("Sam",5)],
        Rest "18" "Jiang-Nan Chun" "Orchard" 1
            [("Julien",5), ("Cheryl",8), ("Bryan",7), ("Petrina",9), ("Justin",4), ("Janice",7), ("Jason",4)],
        Rest "19" "Restaurant Labyrinth" "City Hall" 1
            [("Colin",4), ("Nixon",7), ("Sam",5)],
        Rest "20" "Lei Garden" "City Hall" 1
            [("Bryan",7), ("Petrina",9), ("Janice",7), ("Colin",4), ("Nixon",7)],
        Rest "21" "Lerouy" "Tanjong Pagar" 1
            [("Julien",5), ("Petrina",9), ("Justin",4), ("Jason",4), ("Sam",8)],
        Rest "22" "Liao Fan Hong Kong Soya Sauce Chicken Rice" "Chinatown" 1
            [("Colin",6), ("Nixon",5), ("Sam",5), ("Cheryl",7), ("Bryan",7)],
        Rest "23" "Ma Cuisine" "Tanjong Pagar" 1
            [("Julien",5), ("Cheryl",8), ("Bryan",7), ("Janice",7), ("Jason",4)],
        Rest "24" "Meta" "Outram" 1
            [("Colin",4), ("Nixon",6), ("Sam",5), ("Petrina",7), ("Justin",5)],
        Rest "25" "Restaurant Zen" "Outram" 2
            [("Bryan",7), ("Petrina",9),("Colin",4), ("Nixon",7), ("Sam",5)]
    ]


--
-- Helper functions
--

-- Restuarant ID
getRestID :: Rest -> Rest_ID
getRestID (Rest id _ _ _ _) = id

-- Restuarant Name
getRestName :: Rest -> Rest_Name
getRestName (Rest _ name _ _ _) = name

-- Restuarant Area
getRestArea :: Rest -> Rest_Area
getRestArea (Rest _ _ area _ _) = area

-- Restuarant Star
getRestStar :: Rest -> Rest_Star
getRestStar (Rest _ _ _ star _) = star

-- Restuarant Ratings
getRestRtg :: Rest -> [Rest_Rating]
getRestRtg (Rest _ _ _ _ rating) = rating

-- Restuarant Number of Ratings
getNumRtg :: Rest -> Int
getNumRtg = length . getRestRtg

-- Restuarant Total Rating Score
getTotScore :: Rest -> Score
getTotScore (Rest _ _ _ _ rating) = foldr (+) 0 scores
    where
        scores = [(score) | (chef, score)<-rating]

-- Restuarant Average Score per chef
getAvgScore :: Rest -> Float
getAvgScore r 
    | totScore == 0 && numRtg == 0 = 0
    | otherwise = totScore / numRtg
    where
        totScore = fromIntegral(getTotScore r)
        numRtg = fromIntegral(getNumRtg r)

-- List of chef rated
getChf :: Rest -> [Chef]
getChf r = [ toLowerString(chef) | (chef, score) <- getRestRtg r ]

-- Check if chef rate restaurant
chfRtd :: Chef -> Rest -> Bool
chfRtd chef r = toLowerString(chef) `elem` chefA
    where
        chefA = getChf r

-- Filter chef rated restaurant
filChfRtdRest :: Chef -> [Rest] -> [Rest]
filChfRtdRest chef [] = []
filChfRtdRest chef (r:rs)
    | (chfRtd chef r) = r: filChfRtdRest chef rs
    | otherwise = filChfRtdRest chef rs

-- Filter chef that have not rate restaurant
filChfNotRtdRest :: Chef -> [Rest] -> [Rest]
filChfNotRtdRest chef [] = []
filChfNotRtdRest chef (r:rs)
    | not (chfRtd chef r) = r: filChfNotRtdRest chef rs
    | otherwise = filChfNotRtdRest chef rs    
    
-- Print Restuarant Details
printRest :: Rest -> String
printRest  r
    = 
    "\nID: " ++ (getRestID r) ++ "\n" ++
    "Name: " ++ (getRestName r) ++ "\n" ++
    "Area: " ++ (getRestArea r) ++ "\n" ++
    "Star: " ++ show(getRestStar r) ++ "\n" ++
    "Performance: " ++ printf "%.2g" (getAvgScore r) ++ "\n" ++
    "------------------------------------------------------"

-- Convert String Int Tuple to String
strIntTup2Str :: (String, Int) -> String
strIntTup2Str (a, b)
    = 
    "\nName: " ++ show(fst (a, b)) ++ "\n" ++
    "Score: " ++ show(snd (a, b)) ++ "\n" ++
    "------------------------------------------------------"

-- Print Rating Tuple Details
strIntTup :: [(String, Int)] -> String
strIntTup = foldr (++) "" . map strIntTup2Str

-- Convert String Float Tuple to String
strFltTup2Str :: (String, Float) -> String
strFltTup2Str (a, b)
    = 
    "\nName: " ++ show(fst (a, b)) ++ "\n" ++
    "Performance: " ++ printf "%.2g"(snd (a, b)) ++ "\n" ++
    "------------------------------------------------------"

-- Covert String to lowercase
toLowerString :: [Char] -> [Char]
toLowerString str = [ toLower x | x <- str]

-- Print Rating Tuple Details 2
strFltTup :: [(String, Float)] -> String
strFltTup = foldr (++) "" . map strFltTup2Str

-- Accummulate all chef rating
extChfRatAll :: Chef -> [[(String, Int)]] -> [[Int]]
extChfRatAll chef [] = []
extChfRatAll chef (r:rs) = [ (s) | (c,s) <- r, (toLowerString(c)) == (toLowerString(chef)) ] : extChfRatAll chef rs

-- Concatanate chef and rate into list
concatScore :: Chef -> [[(String, Int)]] -> [Int]
concatScore chef r  = concat (extChfRatAll chef r)

-- Check String is a NUm
checkNum :: String -> Bool
checkNum = all isDigit

-- total score of a Restuarant
totScore :: Rest_Area -> [Rest] -> Float
totScore area r = foldr (+) 0 [ getAvgScore r | r <- (filAreaRest area r) ]

-- total score of a Restuarant
numRestInArea :: Rest_Area -> [Rest] -> Float
numRestInArea area r = fromIntegral(length (filAreaRest area r))

-- Check if restuarant is in [rest] by id
findRestByID :: Rest_ID -> [Rest] -> Rest
findRestByID id (r:rs)
    | id == getRestID r = r
    | otherwise        = findRestByID id rs

-- Find restaurant and return its information by name
findRestByName :: Rest_Name -> [Rest] -> Rest
findRestByName name (r:rs)
    | toLowerString(name) == toLowerString(getRestName r) = r
    | otherwise        = findRestByName name rs

-- Check if rest_id is being used in the [Rest]
uniqRestID :: Rest_ID -> [Rest] -> Bool
uniqRestID id [] = True
uniqRestID id (r:rs)
    | id == getRestID r = False
    | otherwise        = uniqRestID id rs

-- Check if restuarant is in [rest]
findRestOnly :: Rest_Name -> [Rest] -> Bool
findRestOnly name [] = False
findRestOnly name (r:rs) 
    | toLowerString(name) == toLowerString((getRestName r)) = True
    | otherwise                              = findRestOnly name rs

-- return emplty input string error
errEmptyStr :: String -> String 
errEmptyStr word = "You have not entered any " ++ word ++ ". Please enter " ++ word ++ " to continue."

-- return invalid input error
errInvalidInput :: String 
errInvalidInput = "You have entered an incorrect input. Please enter valid input to continue."

--
-- fuctional code
--

-- 
-- i. add a new restaurant to the database
--
addRest :: Rest_ID -> Rest_Name -> Rest_Area -> Rest_Star -> [Rest] -> [Rest]
addRest id name area star rest = rest ++ [(Rest id name area star [])]

--
-- ii. show all restaurants in the database
--
showAll :: [Rest] -> String
showAll = foldr (++) "" . map printRest

--
-- iii. give all restaurants operating in a certain area
--   
filAreaRest :: Rest_Area -> [Rest] -> [Rest]
filAreaRest area [] = []
filAreaRest area (r:rs)
    | toLowerString(area) == toLowerString(getRestArea r) = r: filAreaRest area rs
    | otherwise = filAreaRest area rs

--
-- iv. give all restaurants that have a performance of 8 or higher
--
filHighRtgRest :: [Rest] -> [Rest]
filHighRtgRest [] = []
filHighRtgRest (r:rs)
    | (getAvgScore r) >= 8 = r : filHighRtgRest rs
    | otherwise = filHighRtgRest rs   

-- 
-- v. give the average performance for the restaurants in a certain area
-- 
avgAreaRtg :: Rest_Area -> [Rest] -> Float
avgAreaRtg area r = (totScore area r) / (numRestInArea area r)

-- 
-- vi. give the names of the restaurants a given chef has rated the service level, along with that rating
-- result for each restaurant.
-- 
getChfRtgAll :: Chef -> [Rest] -> [(Rest_Name, Score)]
getChfRtgAll chf r = zip filRestName filScore
    where
        filScore = concatScore chf filRating
        filRestName = [ getRestName r | r <- filRest ]
        filRating = [ getRestRtg r | r <- filRest ]
        filRest = filChfRtdRest chf r

-- 
-- vii. give the names of the restaurants a given chef has yet to rate the service level, along with that
-- restaurant performance.
--
getNotChfRtgAll :: Chef -> [Rest] -> [(Rest_Name, Float)] 
getNotChfRtgAll chef r = zip filRestName filAvgScore
    where
        filAvgScore = [ getAvgScore r | r <- filNotRtgRest ]
        filRestName = [ getRestName r | r <- filNotRtgRest ]
        filNotRtgRest = filChfNotRtdRest chef r

-- 
-- viii. allow a given chef rating to be entered (or updated) for a restaurant he has rated (note that
-- only the latest result from the chef should remain recorded)
--
updScore :: Score -> (Chef, Score) -> (Chef, Score)
updScore newScore (chefName, score) = (chefName, newScore)

iterRtg :: Chef -> Score -> [Rest_Rating] -> [Rest_Rating]
iterRtg chef newScore [] = [] ++ [(chef, newScore)]
iterRtg chef newScore (r:rs)
    | toLowerString(chef) == toLowerString(fst r) = updScore newScore r : iterRtg chef newScore rs
    | otherwise = r: iterRtg chef newScore rs

updRtg :: Chef -> Score -> Rest -> Rest
updRtg chef newScore (Rest id name area star rating) = (Rest id name area star newRating)
    where
        newRating = iterRtg chef newScore rating

updChfRt :: Chef -> Rest_Name -> Score -> [Rest] -> [Rest]
updChfRt chef name newScore [] = []
updChfRt chef name newScore (r:rs)
    | nameFound = updRtg chef newScore r : updChfRt chef name newScore rs
    | otherwise             = r : updChfRt chef name newScore rs
    where
        nameFound = toLowerString(name) == toLowerString(getRestName r)

-- updChfRt :: Chef -> Rest_Name -> Score -> [Rest] -> [Rest]
-- updChfRt chef name newScore r
--     | newScore >= 0 && newScore <= 9 = findRest chef name newScore r
--     | otherwise                = error "Score not in range"

--
-- demo
--
demo :: Int -> IO ()
demo 1 = putStrLn $ showAll $ addRest "999" "Dummy_Rest" "Dummy_Area" 1 testDatabase
demo 2 = putStrLn $ showAll testDatabase
demo 3 = putStrLn $ showAll $ filAreaRest "Orchard" testDatabase
demo 4 = putStrLn $ showAll $ filHighRtgRest testDatabase
demo 5 = putStrLn $ printf "%.2g" $ avgAreaRtg "Orchard" testDatabase
demo 6 = putStrLn $ strIntTup $ getChfRtgAll "Bryan" testDatabase
demo 7 = putStrLn $ strFltTup $ getNotChfRtgAll "Bryan" testDatabase -- below need to update case
demo 8 = putStrLn $ showAll $ updChfRt "Bryan" "Restaurant Zen" 0 testDatabase 
demo 88  = putStrLn $ showAll $ updChfRt "Bryan" "Restaurant Zen" 9 testDatabase

--
-- User interface
--
main :: IO ()
main = do
    putStr "Please enter the database filename without the extension [restaurantList]: "
    filenameInput <- getLine

    if filenameInput == ""
        then do
        let filenameInput = "restaurantList"
            
        let dbFileName = filenameInput ++ ".txt"

        contents <- readFile dbFileName
        let database = (read contents :: [Rest])

        putStrLn "Extracting database..."
        putStrLn "Starting Application."

        putStrLn $ showAll database 

        login dbFileName database 
    else do 
        let dbFileName = filenameInput ++ ".txt"

        contents <- readFile dbFileName
        let database = (read contents :: [Rest])

        putStrLn "Extracting database..."
        putStrLn "Starting Application."

        putStrLn $ showAll database 

        login dbFileName database 

login :: [Char] -> [Rest] -> IO ()
login dbFileName database = do
    putStrLn "Datebase loaded successful"
    putStr "\nPlease enter your username to continue: "
    chf <- getLine

    if (chf == "")
        then do
            login dbFileName database
    else do
        putStr "\ESC[2J"
        menu dbFileName database chf

menu :: [Char] -> [Rest] -> Chef -> IO ()
menu dbFileName database chf = do    

    putStrLn ("\n\n\n\n\nGood day, " ++ chf)
    putStrLn "--------------------------------------------------------------------------------\n"
    putStrLn " [1] Add a new restaurant to the database."
    putStrLn " [2] Display all restaurants in the database."
    putStrLn " [3] Display all restaurants operating in a certain area."
    putStrLn " [4] Display all restaurants that have a performance of 8 or higher."
    putStrLn " [5] Display average performance for the restaurants in a certain area."
    putStrLn " [6] Display restaurants and it's service level that you HAVE rated."
    putStrLn " [7] Display restaurants and it's average performance that you HAVE NOT rated."
    putStrLn " [8] Enter (or update) for a restaurant chef has rated."
    putStrLn " [9] Exit.\n"
    putStrLn "--------------------------------------------------------------------------------\n\n\n\n\n"
    putStr "Please enter your option [1-9]: "
    option <- getLine
    
    case option of
        "1" -> opt1 dbFileName chf database
        "2" -> opt2 dbFileName chf database
        "3" -> opt3 dbFileName chf database
        "4" -> opt4 dbFileName chf database
        "5" -> opt5 dbFileName chf database
        "6" -> opt6 dbFileName chf database
        "7" -> opt7 dbFileName chf database
        "8" -> opt8 dbFileName chf database
        "9" -> opt9 dbFileName database
        otherwise -> menu dbFileName database chf


opt1 :: [Char] -> Chef -> [Rest] -> IO ()
opt1 dbFileName chf database = do
    putStr "Please enter the restaurant ID: "
    inputRestID <- getLine
    opt101 dbFileName chf database inputRestID

opt101 :: [Char] -> Chef -> [Rest] -> Rest_ID -> IO ()
opt101 dbFileName chf database id = do
    if id == ""
        then do
            putStrLn (errEmptyStr "ID")
            opt1 dbFileName chf database
    else if not (uniqRestID id database)
        then do
            putStrLn "The ID entered has been assigned to an existing restaurant. Please enter a valid ID."
            opt1 dbFileName chf database
    else do 
        putStr "Please enter the restaurant name: "
        inputRestName <- getLine
        opt102 dbFileName chf database id inputRestName

opt102 :: [Char] -> Chef -> [Rest] -> Rest_ID -> Rest_Name -> IO ()
opt102 dbFileName chf database id name = do
    if name == ""
        then do
            putStrLn (errEmptyStr "Name")
            opt101 dbFileName chf database id
    else do 
        putStr "Please enter the restaurant area: "
        inputRestArea <- getLine
        opt103 dbFileName chf database id name inputRestArea

opt103 :: [Char] -> Chef -> [Rest] -> Rest_ID -> Rest_Name -> Rest_Area -> IO ()
opt103 dbFileName chf database id name area = do
    if area == ""
        then do
            putStrLn (errEmptyStr "Area")
            opt102 dbFileName chf database id name
    else do
        putStr "Please enter the restaurant's star [1-3]: "
        inputRestStar <- getLine
        if not (checkNum inputRestStar)
            then do
                putStrLn errInvalidInput
                opt103 dbFileName chf database id name area
        else if inputRestStar == ""
            then do
                putStrLn (errEmptyStr "Star")
                opt103 dbFileName chf database id name area
        else do 
            let star = (read inputRestStar :: Int)
            if (star >= 1 && star <= 3)
                then do 
                    opt104 dbFileName chf database id name area star
            else do
                putStrLn errInvalidInput
                opt103 dbFileName chf database id name area

opt104 :: [Char] -> Chef -> [Rest] -> Rest_ID -> Rest_Name -> Rest_Area -> Rest_Star -> IO ()
opt104 dbFileName chf database id name area star = do
    putStrLn "\nRestaurant has been added in the database."

    let update = addRest id name area star database
    let result = ("------------------------------------------------------" ++ printRest (findRestByID id update))
    putStrLn(result)
    putStr "\nDo you want to add another restaurant to the database? [N|Y] "
    addAnother <- getLine

    case addAnother of
        "Y" -> opt1 dbFileName chf update
        "y" -> opt1 dbFileName chf update
        otherwise -> menu dbFileName update chf
        
opt2 :: [Char] -> Chef -> [Rest] -> IO ()
opt2 dbFileName chf database = do
    putStr "\nDisplay all restaurants:\n------------------------------------------------------"
    putStrLn $ showAll database
    putStr "[ENT] "
    dummyInput <- getLine
    menu dbFileName database chf

opt3 :: [Char] -> Chef -> [Rest] -> IO ()
opt3 dbFileName chf database = do
    putStr "Please enter the area of the operating restaurants: "
    restArea <- getLine
    if restArea == ""
        then do
            putStrLn (errEmptyStr "Area")
            opt3 dbFileName chf database
    else do 
        let restAreaResult = filAreaRest restArea database
        if restAreaResult == []
            then do
                putStrLn "No restaurant was found in the area..."
                putStr "Do you want to enter another area of the operating restaurants? [N|Y] "
                continueOps <- getLine

                case continueOps of
                    "Y" -> opt3 dbFileName chf database
                    "y" -> opt3 dbFileName chf database
                    otherwise -> menu dbFileName database chf

        else do
            putStr ("\nDisplay all restaurants in " ++ restArea ++ ".\n------------------------------------------------------")
            putStrLn $ showAll restAreaResult
            putStr "Do you want to enter another area of the operating restaurants? [N|Y] "
            continueOps <- getLine

            case continueOps of
                "Y" -> opt3 dbFileName chf database
                "y" -> opt3 dbFileName chf database
                otherwise -> menu dbFileName database chf

opt4 :: [Char] -> Chef -> [Rest] -> IO ()
opt4 dbFileName chf database = do
    putStr "\nDisplay all restaurants with performance of 8 or higher.\n------------------------------------------------------"
    putStrLn $ showAll $ filHighRtgRest database
    putStr "[ENT] "
    dummyInput <- getLine
    menu dbFileName database chf

opt5 :: [Char] -> Chef -> [Rest] -> IO ()
opt5 dbFileName chf database = do
    putStr "Please enter the area of the operating restaurants: "
    restArea <- getLine
    if restArea == ""
        then do
            putStrLn (errEmptyStr "Area")
            opt5 dbFileName chf database
    else do
        let restAreaResult = filAreaRest restArea database
        if restAreaResult == []
            then do
                putStrLn "\nNo restaurant was found in the area...\n"
                putStr "Do you want to enter another area of the operating restaurants? [N|Y] "
                continueOps <- getLine

                case continueOps of
                    "Y" -> opt5 dbFileName chf database
                    "y" -> opt5 dbFileName chf database
                    otherwise -> menu dbFileName database chf
                
        else do
            let avgPerf = printf "%.2g" $ avgAreaRtg restArea database
            let numRest = printf "%.g" $ numRestInArea restArea database
            let totalScore = printf "%.2g" $  totScore restArea database
            putStrLn ("\nThere are " ++ numRest ++ " restaurants in " ++ restArea ++ " with a total performance score of " ++ totalScore ++ ".")
            putStrLn ("Average performance score: " ++ totalScore ++ " / " ++ numRest ++ " = " ++ avgPerf ++ ".")
            putStr "\nDo you want to enter another area of the operating restaurants? [N|Y] "
            continueOps <- getLine

            case continueOps of
                "Y" -> opt5 dbFileName chf database
                "y" -> opt5 dbFileName chf database
                otherwise -> menu dbFileName database chf    

opt6 :: [Char] -> Chef -> [Rest] -> IO ()
opt6 dbFileName chf database = do
    let result = getChfRtgAll chf database
    if result == []
        then do
        putStrLn ("\nThere is no restaurant rated.")
        putStr "[ENT] "
        dummyInput <- getLine
        menu dbFileName database chf    
    else do
        putStrLn ("\nDisplaying all restaurants rated by " ++ chf ++ ".\n------------------------------------------------------")
        putStrLn $ strIntTup $ result
        putStr "[ENT] "
        dummyInput <- getLine
        menu dbFileName database chf

opt7 :: [Char] -> Chef -> [Rest] -> IO ()
opt7 dbFileName chf database = do
    let result = getNotChfRtgAll chf database
    if result == []
        then do
        putStrLn ("\nThere is no restaurant not rated.")
        putStr "[ENT] "
        dummyInput <- getLine
        menu dbFileName database chf  
    else do
        putStrLn ("\nDisplaying all restaurants not rated by " ++ chf ++ ".\n------------------------------------------------------")
        putStrLn $ strFltTup $ result
        putStr "[ENT] "
        dummyInput <- getLine
        menu dbFileName database chf

opt8 :: [Char] -> Chef -> [Rest] -> IO ()
opt8 dbFileName chf database = do
    putStr "Please enter the name of the restaurant to be rated: "
    restName <- getLine
    if not (findRestOnly restName database)
        then do
            putStrLn "No restaurant record is found in the database, please ensure the name entered is correct."
            putStr "\nDo you want to enter another restaurant name? [N|Y] "
            continueOps <- getLine

            case continueOps of
                "Y" -> opt8 dbFileName chf database
                "y" -> opt8 dbFileName chf database
                otherwise -> menu dbFileName database chf    

    else if restName == ""
        then do
            putStrLn (errEmptyStr "Name")
            opt8 dbFileName chf database
    else do
        opt801 dbFileName chf database restName

opt801 :: [Char] -> Chef -> [Rest] -> Rest_Name -> IO ()
opt801 dbFileName chf database name = do
    putStr ("Please enter rate for " ++ name ++ " [0-9]: ")
    restRate <-getLine
    if not (checkNum restRate)
        then do
            putStrLn errInvalidInput
            opt801 dbFileName chf database name
    else if restRate == ""
        then do
            putStrLn (errEmptyStr "Rate")
            opt801 dbFileName chf database name
    else do 
        let convtRate = (read restRate :: Int)
        if (convtRate >= 0 && convtRate <= 9)
            then do 
                opt802 dbFileName chf database name convtRate
        else do
                putStrLn errInvalidInput
                opt801 dbFileName chf database name 

opt802 :: [Char] -> Chef -> [Rest] -> Rest_Name -> Score -> IO ()
opt802 dbFileName chf database name score = do

    putStrLn ("\n" ++ name ++ " rating has been added/update by "++ show(score) ++ ".")
    let update = updChfRt chf name score database
    let result = ("\n---------------------------------------- Previous ----" ++ printRest (findRestByName name database))
    let result2 = ("--------------------------------------- Updated <<<<<" ++ printRest (findRestByName name update))
    putStrLn(result)
    putStrLn(result2)
    putStr "\nDo you want to enter (or update) another restaurant rating? [N|Y] "
    continueOps <- getLine

    case continueOps of
        "Y" -> opt8 dbFileName chf update
        "y" -> opt8 dbFileName chf update
        otherwise -> menu dbFileName update chf  

opt9 :: [Char] -> [Rest] -> IO ()
opt9 dbFileName database = do
    putStrLn ("Saving to " ++ dbFileName ++ "...")
    putStrLn "Exiting application."
    let toString = show database
    writeFile dbFileName (toString)
