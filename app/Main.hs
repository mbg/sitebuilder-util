--------------------------------------------------------------------------------
-- Utility program for managing Sitebuilder websites                          --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Main (main) where 

--------------------------------------------------------------------------------

import Data.Maybe
import Data.Text

import Options.Applicative

import System.Environment
import System.Exit
import System.FilePath

import Warwick.Config
import Warwick.Common
import Warwick.Sitebuilder

--------------------------------------------------------------------------------

data Command 
    = EditCommand {
        cPage :: Text,
        cFile :: FilePath,
        cComment :: Maybe Text
    }
    | UploadCommand {
        cPage :: Text,
        cFile :: FilePath,
        cSlug :: Maybe Text
    }

editP :: Parser Command 
editP = EditCommand <$> strOption (long "page" <> metavar "PAGE")
                    <*> strOption (long "file" <> metavar "FILE")
                    <*> optional (strOption (long "comment"))

uploadP :: Parser Command 
uploadP = UploadCommand <$> strOption (long "page" <> metavar "PAGE")
                        <*> strOption (long "file" <> metavar "FILE")
                        <*> optional (strOption (long "name"))

commandP :: Parser Command 
commandP = subparser (command "edit" (info editP (progDesc "Edit a file.")) <> 
                      command "upload" (info uploadP (progDesc "Upload a file.")))

opts :: ParserInfo Command 
opts = info (commandP <**> helper) idm

parseCmdLineArgs :: IO Command 
parseCmdLineArgs = execParser opts

--------------------------------------------------------------------------------

handleAPI :: IO (Either a b) -> IO b
handleAPI m = m >>= \case 
    Left err -> exitWith (ExitFailure (-1)) 
    Right _ -> exitSuccess

main :: IO ()
main = do 
    args <- parseCmdLineArgs

    username <- fromMaybe "" <$> lookupEnv "SB_USER"
    password <- fromMaybe "" <$> lookupEnv "SB_PASSWORD"

    let config = APIConfig {
        apiUsername = pack username,
        apiPassword = pack password
    }

    case args of 
        EditCommand{..} -> do 
            let comment = fromMaybe "" cComment

            handleAPI $ withAPI Live config $ 
                editPageFromFile cPage comment cFile
        UploadCommand{..} -> do 
            let name = fromMaybe "" cSlug

            handleAPI $ withAPI Live config $ 
                uploadFile cPage name cFile

--------------------------------------------------------------------------------