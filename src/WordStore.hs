module WordStore where
import qualified Data.Vector as V

listOfEasyWords :: V.Vector String
listOfEasyWords = V.fromList ["monster",
  "reflect",
  "summer",
  "radical",
  "belly",
  "mosque",
  "cigarette",
  "allowance",
  "village",
  "elegant"]


listOfMediumWords :: V.Vector String
listOfMediumWords = V.fromList [
 "hammer",
 "acquaintance",
 "snail",
 "charge",
 "golf",
 "bald",
 "vague",
 "match",
 "interest",
 "infection" ]

listOfHardWords :: V.Vector String
listOfHardWords = V.fromList [
 "split",
 "sink",
 "just",
 "doubt",
 "space",
 "appoint",
 "view",
 "humanity",
 "diagram",
 "soldier"
 ]
