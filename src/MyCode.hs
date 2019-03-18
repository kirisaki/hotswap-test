module MyCode
  ( myFunction
  ) where

myFunction :: Int -> IO ()
myFunction i = putStrLn $ "Adding to 10: " ++ show (10 + i)
