import Data.List
import System.Directory
import System.Environment

main = getArgs >>= \prefixes ->
       let displaylst = prefixes >>= \prefix -> -- wygeneruj listę
                        let lst io = (getDirectoryContents prefix) >>= ... -- to niestety generuje monadę IO
                          in
         in mapM_ print lst
          prefix <- prefixes
          files <- getDirectoryContents prefix
          files
          let lst = do prefix <- prefixes
                       do files <- getDirectoryContents prefix
            in mapM_ print lst
          files <- getDirectoryContents "."
          let lst = do prefix <- prefixes
                       let lst2 = files >>= \file ->
                                  if isPrefixOf prefix file then [file] else []
                         in if null lst2
                            then
                              if isSuffixOf "/" prefix
                              then
                                [concat ["find: `", prefix, "': Not a directory"]]
                              else
                                [concat ["find: `", prefix, "': No such file or directory"]]
                            else
                              lst2
            in mapM_ print lst
