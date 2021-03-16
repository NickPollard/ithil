import Compiler
import Parser
import Types

main :: IO ()
main = test

test :: IO ()
test = print $ do
  parsed <- parse "(\\x. add x 2) 3"
  return $ runCompile parsed

