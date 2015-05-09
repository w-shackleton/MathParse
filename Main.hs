import Parser
import Trans
import Grammar

main = do
  s <- getContents
  print $ eval $ etrans $ parse s
