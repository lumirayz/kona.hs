{-# LANGUAGE OverloadedStrings #-}

----
-- Name: kona.hs
-- Author: lumirayz <lumirayz@gmail.com>
-- Description: A command-line utility that downloads images from http://konachan.com.
----

----
-- Imports
----
import System.Console.GetOpt
import Network.HTTP.Conduit
import qualified Network.URL as U
import Data.Attoparsec
import Control.Applicative ((<$>), (<*>), empty)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson
import Data.Maybe
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMChan
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.List
import System.Environment
import System.Directory

----
-- Image handling
----
data Rating
	= Safe
	| Questionable
	| Explicit
	deriving (Show)

data KonaImage = KonaImage {
	kiId :: Integer,
	kiCreation :: Integer,
	kiSize :: Integer,
	kiMd5hex :: String,
	kiUploader :: String,
	kiTags :: [String],
	kiRating :: Rating,
	kiScore :: Integer,
	kiPreviewUrl :: String,
	kiPreviewWidth :: Integer,
	kiPreviewHeight :: Integer,
	kiSampleUrl :: String,
	kiSampleWidth :: Integer,
	kiSampleHeight :: Integer,
	kiFullUrl :: String,
	kiFullWidth :: Integer,
	kiFullHeight :: Integer
} deriving (Show)

toRating :: String -> Rating
toRating "s" = Safe
toRating "q" = Questionable
toRating "e" = Explicit

instance FromJSON KonaImage where
	parseJSON (Object v) = KonaImage <$>
		v .: "id" <*>
		v .: "created_at" <*>
		v .: "file_size" <*>
		v .: "md5" <*>
		v .: "author" <*>
		(v .: "tags" >>= return . words) <*>
		(v .: "rating" >>= return . toRating) <*>
		v .: "score" <*>
		v .: "preview_url" <*>
		v .: "actual_preview_width" <*>
		v .: "actual_preview_height" <*>
		v .: "sample_url" <*>
		v .: "sample_width" <*>
		v .: "sample_height" <*>
		v .: "file_url" <*>
		v .: "width" <*>
		v .: "height"
	parseJSON _ = empty

konaSearchUrl :: String -> Integer -> Integer -> String
konaSearchUrl tags page limit =
	"http://konachan.com/post/index.json?" ++
	"tags=" ++ U.encString True U.ok_url tags ++
	"&page=" ++ (show page) ++
	"&limit=" ++ (show limit)

search :: String -> Integer -> Integer -> IO (Maybe [KonaImage])
search tags page limit = do
	jobj <- simpleHttp (konaSearchUrl tags page limit)
	let k = decode jobj :: Maybe [KonaImage]
	return k

downloadImage :: String -> IO (BL.ByteString, String)
downloadImage link = do
	body <- simpleHttp link
	return (body, last (splitOn link '.'))

downloadFullImage, downloadSampleImage, downloadPreviewImage :: KonaImage -> IO (BL.ByteString, String)
downloadFullImage (KonaImage {kiFullUrl = link}) = downloadImage link
downloadSampleImage (KonaImage {kiSampleUrl = link}) = downloadImage link
downloadPreviewImage (KonaImage {kiPreviewUrl = link}) = downloadImage link

----
-- Opt parsing
----
data Flag
	= Page (Maybe String)
	| Ammount (Maybe String)
	| ThreadCount (Maybe String)
	| ImageType (Maybe String)
	| Help
	deriving (Show)

flagDef =
	[ Option "p" ["page"] (OptArg Page "PAGE")
		"set page number"
	, Option "a" ["ammount"] (OptArg Ammount "AMMOUNT")
		"set ammount of results"
	, Option "t" ["threads"] (OptArg ThreadCount "THREADS")
		"set ammount of threads"
	, Option "i" ["image-type"] (OptArg ImageType "TYPE")
		"set image type ([p]review, [s]ample, [full])"
	, Option "h" ["help"] (NoArg Help)
		"view this help page"
	]

header = "Usage: runhaskell kona.hs [OPTS] tags..."

getPage, getAmmount, getThreadCount :: [Flag] -> Maybe Integer
getPage ((Page (Just p)):fs) = Just (read p)
getPage ((Page Nothing):fs) = Nothing
getPage (f:fs) = getPage fs
getPage [] = Nothing

getAmmount ((Ammount (Just a)):fs) = Just (read a)
getAmmount ((Ammount Nothing):fs) = Nothing
getAmmount (f:fs) = getAmmount fs
getAmmount [] = Nothing

getThreadCount ((ThreadCount (Just t)):fs) = Just (read t)
getThreadCount ((ThreadCount Nothing):fs) = Nothing
getThreadCount (f:fs) = getAmmount fs
getThreadCount [] = Nothing

getImageType :: [Flag] -> Maybe Int
getImageType ((ImageType (Just s)):fs) = Just $ case s of
	"p" -> 0
	"s" -> 1
	"f" -> 2
	"preview" -> 0
	"sample" -> 1
	"full" -> 2
	otherwise -> 0 -- i am very sorry, user, for not reporting this error! :(
getImageType ((ImageType Nothing):fs) = Nothing
getImageType (f:fs) = getImageType fs
getImageType [] = Nothing

getHelp :: [Flag] -> Bool
getHelp (Help:fs) = True
getHelp (f:fs) = getHelp fs
getHelp [] = False

----
-- Main
----
main :: IO ()
main = do
	args <- getArgs
	case getOpt RequireOrder flagDef args of
		(o, n, []) -> do
			let page = max (maybe 0 id $ getPage o) 0
			let amt = max (min (maybe 10 id $ getAmmount o) 100) 1
			let tc = max (maybe 5 id $ getThreadCount o) 1
			let imageType = maybe 0 id $ getImageType o
			let help = getHelp o
			if help then
				putStrLn $ usageInfo header flagDef
			else do
				let tags = intercalate " " n
				if length tags == 0 then
					putStrLn "Error: no tags?"
				else do
					putStrLn "Searching..."
					mimgs <- search tags page amt
					case mimgs of
						Nothing -> putStrLn "Search failure"
						Just imgs -> do
							putStrLn $ "Done, found " ++ show (length imgs) ++ " images."
							putStrLn "Downloading..."
							threadDelay 2000000
							doesDirectoryExist tags >>= ((flip unless) $ createDirectory tags)
							c <- atomically $ newTMChan
							imagesPending <- atomically $ newTVar (length imgs)
							forM_ imgs $ \img -> atomically $ writeTMChan c img
							replicateM_ (fromInteger tc) $ forkIO $ consumer tags imageType c imagesPending
							untilM_ ((atomically $ readTVar imagesPending) >>= return . (<= 0)) $ do
								threadDelay 500000
							putStrLn "Downloaded"
		(_, _, errs) -> putStrLn $ concat errs ++ usageInfo header flagDef

consumer :: String -> Int -> TMChan KonaImage -> TVar Int -> IO ()
consumer dir t c pending = do
	mimg <- atomically $ readTMChan c
	case mimg of
		Nothing -> return ()
		Just img -> do
			let f = case t of
				0 -> downloadPreviewImage
				1 -> downloadSampleImage
				2 -> downloadFullImage
			(body, ext) <- f img
			BL.writeFile (dir ++ "/" ++ show (kiId img) ++ "." ++ ext) body
			atomically $ readTVar pending >>= writeTVar pending . (+ (-1))
			consumer dir t c pending

----
-- Util
----
splitOn :: String -> Char -> [String]
splitOn s c = map T.unpack $ T.splitOn (T.singleton c) $ T.pack s

untilM_ :: IO Bool -> IO () -> IO ()
untilM_ cond body = do
	break <- cond
	unless break $ body >> untilM_ cond body
