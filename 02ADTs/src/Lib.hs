module Lib where

import Log

someFunc :: IO ()
someFunc = putStrLn "someFunc"

pop1Int :: String -> (Int, String)
pop1Int s =
  let sWords = words s
      rest = unwords $ tail sWords
   in (read $ head sWords, rest)

parseSeverity :: String -> (MessageType, String)
parseSeverity s =
  let (severity, rest) = pop1Int s
   in (Error severity, rest)

parseCommon :: MessageType -> String -> LogMessage
parseCommon kind s =
  let (timestamp, msg) = pop1Int s
   in LogMessage kind timestamp msg

parseMessage :: String -> LogMessage
parseMessage s =
  case s of
    'I':rest -> parseCommon Info rest
    'W':rest -> parseCommon Warning rest
    'E':rest -> uncurry parseCommon $ parseSeverity rest
    _ -> Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

insert :: LogMessage -> MessageTree -> MessageTree
insert msg tree =
  case msg of
    Unknown _ -> tree
    known -> insertKnown known tree

insertKnown :: LogMessage -> MessageTree -> MessageTree
insertKnown msg tree =
  let nodeToInsert = Node Leaf msg Leaf
   in case tree of
        Leaf -> nodeToInsert
        _ -> insertNonLeaf msg tree

insertNonLeaf :: LogMessage -> MessageTree -> MessageTree
insertNonLeaf msg (Node left treeMsg right) =
  let msgTs = getTimestamp msg
      treeTs = getTimestamp treeMsg
      -- I pray I understood laziness well
      itGoesLeft = Node left treeMsg $ insertKnown msg right
      itGoesRight = Node (insertKnown msg left) treeMsg right
      -- For alphatecial comparison if timestamps are equal
      msgTxt = getTxt msg
      treeTxt = getTxt treeMsg
   in case msgTs of
        ts
          | ts < treeTs -> itGoesLeft
        ts
          | ts > treeTs -> itGoesRight
        _ ->
          case msgTxt of
            txt
              | txt <= treeTxt -> itGoesLeft
            _ -> itGoesRight

getTimestamp :: LogMessage -> Int
getTimestamp (LogMessage _ ts _) = ts

getTxt :: LogMessage -> String
getTxt (LogMessage _ _ txt) = txt

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder tree =
  case tree of
    Leaf -> []
    Node left msg right -> inOrder left ++ [msg] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs =
  let filtered = filter isErr50AndMore msgs
   in map getTxt $ inOrder $ build filtered

isErr50AndMore :: LogMessage -> Bool
isErr50AndMore (LogMessage kind _ _) =
  case kind of
    Error n
      | n >= 50 -> True
    Warning -> True
    _ -> False
