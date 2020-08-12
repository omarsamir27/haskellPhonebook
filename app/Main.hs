module Main where
import Lib
import System.IO
import Data.List
import Data.List.Split
import Data.List.Utils

data Date = Date {day :: Int ,
                  month :: Int ,
                  year :: Int} deriving (Read,Eq)

instance Show Date where
    show date = show (day date) ++ "-" ++ show (month date) ++ "-" ++ show (year date)

createDate :: String -> Date
createDate date = let [x,y,z] = wordsBy (=='-') date in Date (read x)  (read y)  (read z)


data Contact = Contact { lastname :: String,
                         firstname :: String,
                         dob :: Date,
                         address :: String,
                         email :: String ,
                         phonenumber :: String
                         } deriving (Eq)
instance Show Contact  where
    show contact = firstname contact ++ "\t" ++ lastname contact ++ "\t" ++ show (dob contact) ++ "\t" ++ address contact ++ "\t" ++ email contact ++ "\t" ++ phonenumber contact

   
   
   
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



dispatch :: [Contact] -> String -> String-> IO ()
dispatch db "1" _ = contactPrinter db
dispatch db "2" _ = do
                print "Write Name to Search With"
                name <- getLine
                contactPrinter $ searchContact db name
dispatch db "3" filepath = do
                  print "Write Firstname: "
                  fname <- getLine
                  print "Write Lastname: "
                  lname <- getLine
                  print "Write Date of Birth (DD-MM-YYYY): "
                  bdate <- getLine                 
                  print "Write Email: "
                  mail <- getLine
                  print "Write Phonenumber: "
                  number <- getLine
                  print "Write Address: "
                  place <- getLine 
                  let toADD = createContact [lname , fname , bdate ,place , mail , number]                               
                  addContact [toADD] filepath AppendMode                 
dispatch db "4" filepath = do
                        print "Name to Delete: "
                        nametoDel <- getLine
                        addContact (db \\ searchContact db nametoDel) filepath WriteMode
dispatch db _ filepath = do
                 print "Wrong Choice"
                 homeScreen db filepath
                 
homeScreen :: [Contact] -> String -> IO ()
homeScreen phoneDB filepath = do
            print "What to do"
            putStrLn "1. Print Book \n2. Search by name \n3. Add Contact \n4. Delete Contact"
            choice <- getLine
            dispatch phoneDB choice filepath
            homeScreen phoneDB filepath

main :: IO ()
main = do
    print "File Path"
    filepath <- getLine
    phoneBook <- openFile filepath ReadMode
    addressbook <- hGetContents phoneBook
    --hClose phoneBook
    let phoneDB = map (createContact . wordsBy (== ',')) (lines addressbook)
    homeScreen phoneDB filepath