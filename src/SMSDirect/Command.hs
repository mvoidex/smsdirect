-- | Module contains functions to form URL
module SMSDirect.Command (
  -- * Types
  Command,
  Phone,
  Role(..),
  TypeList(..),
  Account(..),
  Person(..),
  Info(..),

  -- * Commands
  submitMessage,
  submitDB,
  editDB,
  statusDB,
  getDB,
  deleteDB,
  submitDispatch,
  statusDispatch,
  getUserInfo,
  statsDispatch,
  statusMessage,

  -- * General command functions
  smsdirect,
  url,
  command,

  -- * Helper functions
  account,
  utf8,
  phone,
  phones,
  dateTime,
  sender,
  smsdirectUrl,
  commandUrl

  ) where

import Data.Monoid
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.String
import Data.Time
import Data.URLEncoded
import Network.HTTP.Conduit
import System.Locale

data Command a = Command String URLEncoded (Response ByteString -> IO a)

type Phone = Integer

data Role = Database | BlackList deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance URLShow Role where
  urlShow = urlShow . succ . fromEnum

data TypeList = ByDatabase | ByPhoneList deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance URLShow TypeList where
  urlShow = urlShow . succ . fromEnum

data Account = Account {
  accountLogin :: Text,
  accountPass :: Text }
    deriving (Eq, Ord, Read, Show)

-- | Person type stored in DB
data Person = Person {
  personPhone :: Phone,
  personName :: Text,
  personSecondName :: Text,
  personLastName :: Text,
  personCity :: Text,
  personDate :: UTCTime,
  personSex :: Text }
    deriving (Eq, Ord, Read, Show)

-- | User info
data Info =
  InfoBalance |
  InfoManager |
  InfoCanChangeNumber |
  InfoHTTPGateDispatch
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance URLShow Info where
  urlShow = urlShow . fromEnum

-- | Submit one message
submitMessage :: Text -> Phone -> Text -> Maybe Text -> Command (Response ByteString)
submitMessage from to text start = command "submit_message" $ mconcat [
  "from" %= utf8 (sender from),
  "to" %= to,
  "text" %= utf8 text,
  "startdate" %=? fmap utf8 start]

-- | Create/update DB
submitDB :: Maybe Int -> Maybe Role -> Maybe Text -> [Person] -> Command (Response ByteString)
submitDB i r name ps = command "submit_db" $ mconcat [
  "id" %=? i,
  "role" %=? r,
  "lname" %=? fmap utf8 name,
  "qfile" %= utf8 ps']
  where
    ps' = T.unlines $ map showPerson ps
    showPerson :: Person -> Text
    showPerson (Person ph n sn ln city tm sx) = T.intercalate (fromString ";") [
      T.pack . show $ ph,
      n, sn, ln, city,
      T.pack . formatTime defaultTimeLocale "%d-%m-%Y" $ tm,
      sx]

-- | Remove records from DB
editDB :: Int -> [Phone] -> Command (Response ByteString)
editDB i ps = command "edit_db" $ mconcat [
  "id" %= i,
  "msisdn" %= utf8 (phones ps)]

-- | Get DB status
statusDB :: Int -> Command (Response ByteString)
statusDB i = command "status_db" ("id" %= i)

-- | Get DB list
getDB :: Command (Response ByteString)
getDB = command "get_db" mempty

-- | Delete DB
deleteDB :: Int -> Command (Response ByteString)
deleteDB i = command "delete_db" ("id" %= i)

submitDispatch ::
  TypeList ->
  Int ->
  -- ^ Id of DB
  [Phone] ->
  Text ->
  -- ^ Message with patterns
  Bool ->
  -- ^ Is wap-push?
  Maybe Int ->
  -- ^ Maximum messages per day
  Maybe Int ->
  -- ^ Maximum messages per hour
  [Int] ->
  -- ^ Hours to send in, for example [14..17] is to send messages from 14 to 17 (including ends)
  [Int] ->
  -- ^ Weekdays to send in, 0 is for sunday.
  UTCTime ->
  -- ^ Start date and time of dispatch
  -- If by abonent date is 'True', then date is ignored
  UTCTime ->
  -- ^ End date and time
  Maybe Text ->
  -- ^ Sender
  Bool ->
  -- ^ Message is pattern
  Bool ->
  -- ^ By abonent date (?)
  Bool ->
  -- ^ Use local time of receivers
  Command (Response ByteString)

submitDispatch tl i ps mess wap perDay perHour hours days start end from isPattern byAbonentDate localTime =
  command "submit_dispatch" $ mconcat [
    "typelist" %= tl,
    "list" %= i,
    "msisdn" %= utf8 (phones ps),
    "mess" %= utf8 mess,
    "isurl" %= fromEnum wap,
    "max_mess" %= maybe "" show perDay,
    "max_pess_per_hour" %= maybe "" show perHour,
    hours',
    days',
    "startdate" %= utf8 (dateTime start),
    "enddate" %= utf8 (dateTime end),
    "number" %=? fmap (utf8 . sender) from,
    "pattern" %= fromEnum isPattern,
    "byabonentdate" %= fromEnum byAbonentDate,
    "localtime" %= fromEnum localTime]
    where
      hours'
        | all (`elem` [0 .. 23]) hours = mconcat [(T.unpack . T.append (fromString "shour") . T.justifyRight 2 '0' . T.pack . show $ h) %= (1 :: Int) | h <- hours]
        | otherwise = error "Hours must be in range [0 .. 23]"
      days'
        | all (`elem` [0 .. 6]) days = mconcat [("sday" ++ show d) %= (1 :: Int) | d <- days]
        | otherwise = error "Days must be in range [0 .. 6]"

-- | Get dispatch status
statusDispatch :: Int -> Command (Response ByteString)
statusDispatch i = command "status_dispatch" ("id" %= i)

-- | Get user info
getUserInfo :: Info -> Command (Response ByteString)
getUserInfo m = command "get_user_info" ("mode" %= m)

-- | Dispatch stats
statsDispatch :: Int -> Command (Response ByteString)
statsDispatch i = command "stats_dispatch" ("id" %= i)

-- | Message status
statusMessage :: Int -> Command (Response ByteString)
statusMessage i = command "status_message" ("id" %= i)

-- | Run command with account
smsdirect :: Text -> Text -> Command a -> IO a
smsdirect login pass cmd@(Command _ _ onResult) = do
  req <- parseUrl (url login pass cmd)
  let
    req' = req {
      method = fromString "GET",
      secure = True,
      checkStatus = \_ _ -> Nothing }
  resp <- withManager $ httpLbs req'
  onResult resp

-- | Create url by command without any actions
url :: Text -> Text -> Command a -> String
url login pass (Command name args _) = commandUrl smsdirectUrl name $ account (Account login pass) `mappend` args

-- | Run command with login and pass
command :: String -> URLEncoded -> Command (Response ByteString)
command name args = Command name args return

-- | Account and password
account :: Account -> URLEncoded
account (Account login pass) = mconcat [
  "login" %= utf8 login,
  "pass" %= utf8 pass]

-- | Encode Text as UTF8
utf8 :: Text -> String
utf8 = C8.unpack . T.encodeUtf8

-- | Encode phone, allows only digits
phone :: Text -> Phone
phone p
  | T.all isDigit p = read . T.unpack $ p
  | otherwise = error $ "Phone must contain only digits: " ++ T.unpack p

-- | Separate phones with comma
phones :: [Phone] -> Text
phones = T.intercalate (fromString ",") . map (T.pack . show)

-- | Encode date-time
dateTime :: UTCTime -> Text
dateTime = T.pack . formatTime defaultTimeLocale "%H:%M %d.%m.%Y"

-- | Encode sender, allows digits, latin letters, '_', '-', '&' and '.'
-- and maximum length of 11
sender :: Text -> Text
sender p
  | T.all isValid p && T.length p == 11 = p
  | otherwise = error $ "Sender must contain only digits, latin letters or one of '_', '-', '&', '.': " ++ T.unpack p
  where
    isValid c = isDigit c || (isAscii c && isLetter c) || (c `elem` "_-&.")

smsdirectUrl :: String
smsdirectUrl = "https://www.smsdirect.ru"

commandUrl :: String -> String -> URLEncoded -> String
commandUrl baseUrl cmd args = (baseUrl ++ "/" ++ cmd) %? args
