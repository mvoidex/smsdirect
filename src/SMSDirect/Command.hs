-- | Module contains functions to form URL
module SMSDirect.Command (
  -- * Types
  Command,
  Phone,
  Role(..),
  ErrorCode,
  MessageId, DatabaseId, DispatchId,
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
  commandUrl,
  resultInt

  ) where

import qualified Control.Exception as E
import Control.Monad
import Data.Monoid
import Data.ByteString.Lazy (ByteString, toChunks)
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

data Command a = Command String URLEncoded (ByteString -> IO a)

type Phone = Integer

data Role = Database | BlackList deriving (Eq, Ord, Read, Show, Enum, Bounded)

type ErrorCode = Int

type MessageId = Int
type DatabaseId = Int
type DispatchId = Int

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
submitMessage :: Text -> Phone -> Text -> Maybe Text -> Command MessageId
submitMessage from to text start = command "submit_message" args resultInt where
  args = mconcat [
    "from" %= utf8 (sender from),
    "to" %= to,
    "text" %= utf8 text,
    "startdate" %=? fmap utf8 start]

-- | Create/update DB
submitDB :: Maybe DatabaseId -> Maybe Role -> Maybe Text -> [Person] -> Command DatabaseId
submitDB i r name ps = command "submit_db" args resultInt where
  args = mconcat [
    "id" %=? i,
    "role" %=? r,
    "lname" %=? fmap utf8 name,
    "qfile" %= utf8 ps']
  ps' = T.unlines $ map showPerson ps
  showPerson :: Person -> Text
  showPerson (Person ph n sn ln city tm sx) = T.intercalate (fromString ";") [
    T.pack . show $ ph,
    n, sn, ln, city,
    T.pack . formatTime defaultTimeLocale "%d-%m-%Y" $ tm,
    sx]

-- | Remove records from DB
editDB :: DatabaseId -> [Phone] -> Command DatabaseId
editDB i ps = command "edit_db" args resultInt where
  args = mconcat [
    "id" %= i,
    "msisdn" %= utf8 (phones ps)]

-- | Get DB status
statusDB :: DatabaseId -> Command Int
statusDB i = command "status_db" ("id" %= i) resultInt

-- | Get DB list
getDB :: Command ByteString
getDB = command "get_db" mempty return

-- | Delete DB
deleteDB :: DatabaseId -> Command ()
deleteDB i = command "delete_db" ("id" %= i) checkResult where
  checkResult r = do
    v <- resultInt r
    case v of
      0 -> return ()
      _ -> error $ "Unknown response code: " ++ show i

submitDispatch ::
  TypeList ->
  DatabaseId ->
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
  Command DispatchId

submitDispatch tl i ps mess wap perDay perHour hours days start end from isPattern byAbonentDate localTime =
  command "submit_dispatch" args resultInt where
    args = mconcat [
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
    hours'
      | all (`elem` [0 .. 23]) hours = mconcat [(T.unpack . T.append (fromString "shour") . T.justifyRight 2 '0' . T.pack . show $ h) %= (1 :: Int) | h <- hours]
      | otherwise = error "Hours must be in range [0 .. 23]"
    days'
      | all (`elem` [0 .. 6]) days = mconcat [("sday" ++ show d) %= (1 :: Int) | d <- days]
      | otherwise = error "Days must be in range [0 .. 6]"

-- | Get dispatch status
statusDispatch :: DispatchId -> Command Int
statusDispatch i = command "status_dispatch" ("id" %= i) resultInt

-- | Get user info
getUserInfo :: Info -> Command ByteString
getUserInfo m = command "get_user_info" ("mode" %= m) return

-- | Dispatch stats
statsDispatch :: DispatchId -> Command ByteString
statsDispatch i = command "stats_dispatch" ("id" %= i) return

-- | Message status
statusMessage :: MessageId -> Command Int
statusMessage i = command "status_message" ("id" %= i) resultInt

-- | Run command with account
smsdirect :: Text -> Text -> Command a -> IO (Either ErrorCode a)
smsdirect login pass cmd@(Command _ _ onResult) = do
  req <- parseUrl (url login pass cmd)
  let
    req' = req {
      method = fromString "GET",
      secure = True,
      checkStatus = \_ _ -> Nothing }
  (Response s _ h resp) <- withManager $ httpLbs req'
  case checkStatus req s h of
    Nothing -> liftM Right $ onResult resp
    Just _ -> maybe (return $ Left 0) (return . Left) $ fmap fst $ C8.readInt $ C8.concat $ toChunks resp

-- | Create url by command without any actions
url :: Text -> Text -> Command a -> String
url login pass (Command name args _) = commandUrl smsdirectUrl name $ account (Account login pass) `mappend` args

-- | Run command with login and pass
command :: String -> URLEncoded -> (ByteString -> IO a) -> Command a
command = Command

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
  | T.all isValid p && T.length p <= 11 = p
  | otherwise = error $ "Sender must contain only digits, latin letters or one of '_', '-', '&', '.': " ++ T.unpack p
  where
    isValid c = isDigit c || (isAscii c && isLetter c) || (c `elem` "_-&.")

smsdirectUrl :: String
smsdirectUrl = "https://www.smsdirect.ru"

commandUrl :: String -> String -> URLEncoded -> String
commandUrl baseUrl cmd args = (baseUrl ++ "/" ++ cmd) %? args

-- | Parse result as Int
resultInt :: ByteString -> IO Int
resultInt bs = maybe (error $ "Unable to parse as int: " ++ C8.unpack bs') (return . fst) . C8.readInt $ bs' where
  bs' = C8.concat . toChunks $ bs
