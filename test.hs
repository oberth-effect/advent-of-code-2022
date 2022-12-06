import Text.Read

parser :: String -> Maybe [String]
parser = traverse readMaybe . words

main :: IO ()
main = do
  c <- getContents
  print $ lines c
