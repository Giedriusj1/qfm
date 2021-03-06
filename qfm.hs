    -- quick fuse mount
    -- Copyright (C) {2015}  {Giedrius Jonikas; giedriusj1@gmail.com}

    -- This program is free software; you can redistribute it and/or modify
    -- it under the terms of the GNU General Public License as published by
    -- the Free Software Foundation; either version 2 of the License, or
    -- (at your option) any later version.

    -- This program is distributed in the hope that it will be useful,
    -- but WITHOUT ANY WARRANTY; without even the implied warranty of
    -- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    -- GNU General Public License for more details.

    -- You should have received a copy of the GNU General Public License along
    -- with this program; if not, write to the Free Software Foundation, Inc.,
    -- 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.import System.Process
       
import System.Environment
import System.Process
import System.Directory
import System.IO.Unsafe         -- for file reading
import Data.List.Split          -- "cabal install split" is needed for this one
import Control.Monad.IfElse
import Data.Strings

type Hostname   = String
type MountPoint = String
type Nickname   = String
type Host = (Hostname, MountPoint, Nickname)    

helpAlias :: [[Char]]
helpAlias = ["help"]

mountAllAlias :: [[Char]]
mountAllAlias = ["all", "mountall", "a"]

unmountAllAlias :: [[Char]]
unmountAllAlias = ["unmountall", "ua"]

statsAlias :: [[Char]]
statsAlias = ["stats", "s"]

    
hostsList :: [Host]
hostsList = parsedList
    where contents = unsafePerformIO $ readFile (unsafePerformIO getHomeDirectory ++"/.qfmHosts")
          -- Split into lines and filter comments
          lineList = [n | n <- (splitOn "\n" contents),
                               strLen n /= 0,   -- filter empty
                               (n !! 0) /= '#'] -- filter comments
          
          parsedList = [ parseEntry n |
                        n <- [splitOn " " n | n <- lineList]]
                       
          parseEntry elemList
              | length line == 3 = (line !! 0, line !! 1, line !! 2) :: Host
              | length line == 2 = (line !! 0, line !! 1, "N/A")     :: Host
              -- filter out empty entries. This might happen with multiple spaces.
              where line = [n | n <- elemList, n /= ""]


helpFunc :: IO ()
helpFunc = do
  putStr "This is a help function"
  return ()

mountAllFunc :: IO ()
mountAllFunc = do
  putStr "mounting all\n"
  retCodes <- mapM mount hostsList

  return ()

unmountAllFunc :: IO ()
unmountAllFunc = do
  putStr "unmounting all\n"
  retCodes <- mapM unmount hostsList

  return ()


statsFunc :: IO ()         
statsFunc = do
  ret <- runAndWaitCommand "mount -l -t fuse.sshfs "
  return ()
             

main :: IO ()
main = do
  name <- getArgs
  print name
  
        
  cond [ ((length name < 1) , print "no arguments provided"),
         (name !! 0 `elem` helpAlias,     helpFunc),
         (name !! 0 `elem` mountAllAlias, mountAllFunc),
         (name !! 0 `elem` unmountAllAlias, unmountAllFunc),
         (name !! 0 `elem` statsAlias,    statsFunc)
       ]

  return ()

unmount :: Host -> IO()
unmount (hostname ,mountpoint,nickname) = do
  putStr $  "Unmounting: " ++ hostname ++ " Mountpoint: " ++ mountpoint ++ "\n"
  ret <- runAndWaitCommand $ "fusermount -u " ++ mountpoint
  putStr $ show ret ++ "\n"
  return ()
         
mount :: Host -> IO ()
mount (hostname ,mountpoint,nickname) = do
  putStr $  "Mounting: " ++ hostname ++ " Mountpoint: " ++ mountpoint ++ "\n"
  ret <- runAndWaitCommand $ "sshfs -o Ciphers=arcfour " ++ hostname ++ " " ++ mountpoint
  putStr $ show ret ++ "\n"
  return ()
    where
      
         
runAndWaitCommand programName = do
  procHandle <- runCommand programName
  retCode <- waitForProcess procHandle
  return retCode
