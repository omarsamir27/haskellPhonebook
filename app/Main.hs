module Main where
import Lib
import System.IO
import qualified System.IO.Strict as SIO
import Data.List
import Data.List.Split
import Data.List.Utils
import Data.Time.Calendar

--data Date = Date {day :: Int ,
--                  month :: Int ,
--                  year :: Int} deriving (Read,Eq)
--
--instance Show Date where
--    show date = show (day date) ++ "-" ++ show (month date) ++ "-" ++ show (year date)

createDate :: String -> Day
createDate date = let [day,month,year] = wordsBy (=='-') date in fromGregorian (read year)  (read month)  (read day)

reverseDayFormat :: Day -> String
reverseDayFormat date = (show day) ++ "-" ++ (show month) ++ "-" ++  (show year) 
                        where (year , month , day) = toGregorian date


data Contact = Contact { lastname :: String,
                         firstname :: String,
                         dob :: Day,
                         address :: String,
                         email :: String ,
                         phonenumber :: String
                         } deriving (Eq)
instance Show Contact  where
    show contact = firstname contact ++ "\t" ++ lastname contact ++ "\t" ++  (reverseDayFormat (dob contact)) ++ "\t" ++ address contact ++ "\t" ++ email contact ++ "\t" ++ phonenumber contact

   
   
   
createContact :: [String] -> Contact
createContact [lastname , firstname , dob , address , email , phonenumber] = Contact lastname firstname dob' address email phonenumber
                        where dob' = createDate dob

contactWrite :: Contact -> String
contactWrite contact = replace "\t" ","  (show contact)

addContact :: [Contact] -> String -> IOMode -> IO ()
addContact contacts filepath mode = do 
                            phonebook <- openFile filepath mode
                            mapM_ (hPutStrLn phonebook . contactWrite) contacts 
                            hClose phonebook
                           
searchContact :: [Contact] -> String -> [Contact]
searchContact phoneDB name = [contact | contact <- phoneDB , (lastname contact == name) || (firstname contact == name)]

contactPrinter :: [Contact] -> IO ()
contactPrinter =  mapM_ print

ageSort :: [Contact] -> [Contact]
ageSort = sortBy (\a b -> (dob a) `compare` (dob b) )

nameSort :: [Contact] -> [Contact]
nameSort = sortBy (\a b -> (firstname a) `compare` (firstname b) )
        
contactSort :: [Contact] -> String -> [Contact]
contactSort db "1" =  reverse $ ageSort db
contactSort db "2" = ageSort db
contactSort db "3" = nameSort db
contactSort db "4" = reverse $ nameSort db
contactSort db _ = db 
       
             



dispatch :: [Contact] -> String -> String-> IO ()
dispatch db "1" _ = contactPrinter db
dispatch db "2" _ = do
                putStrLn "Write Name to Search With"
                name <- getLine
                contactPrinter $ searchContact db name
dispatch db "3" filepath = do
                  putStrLn "Write Firstname: "
                  fname <- getLine
                  putStrLn "Write Lastname: "
                  lname <- getLine
                  putStrLn "Write Date of Birth (DD-MM-YYYY): "
                  bdate <- getLine                 
                  putStrLn "Write Email: "
                  mail <- getLine
                  putStrLn "Write Phonenumber: "
                  number <- getLine
                  putStrLn "Write Address: "
                  place <- getLine 
                  let toADD = createContact [lname , fname , bdate ,place , mail , number]                               
                  addContact [toADD] filepath AppendMode                 
dispatch db "4" filepath = do
                        putStrLn "Name to Delete: "
                        nametoDel <- getLine
                        addContact (db \\ searchContact db nametoDel) filepath WriteMode
dispatch db "5" _ = do
                    putStrLn "1. Age Ascendingly\n2. Age Descendingly\n 3. FName Ascendingly\n 4.FName Descendingly"
                    priority <- getLine
                    contactPrinter $ contactSort db priority
dispatch db _ filepath = do
                 putStrLn "Wrong Choice"
                 homeScreen  filepath
                 
homeScreen ::   String -> IO ()
homeScreen  filepath = do
            phoneBook <- openFile filepath ReadMode
            addressbook <- SIO.hGetContents phoneBook
            putStrLn "What to do"
            putStrLn "1. Print Book \n2. Search by name \n3. Add Contact \n4. Delete Contact \n5.Sort and Print"
            choice <- getLine
            let phoneDB = loadDB addressbook
            dispatch phoneDB choice filepath
            homeScreen filepath

loadDB :: String -> [Contact]
loadDB addressbook = map (createContact . wordsBy (== ',')) (lines addressbook)

main :: IO ()
main = do
    putStrLn "File Path"
    filepath <- getLine
    homeScreen  filepath
