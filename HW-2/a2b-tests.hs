import Test.HUnit

import A2b

tests = test [
  --1. removeAllExcept
  "removeAllExcept 5 [1..10]" ~: [5] ~=? (removeAllExcept 5 [1..10]),
  
  --2. removeAll
  "removeAll 5 [1..10]" ~: [1,2,3,4,6,7,8,9,10] ~=? (removeAll 5 [1..10]),

  --3. substitute
  "substitute 5 1 [1..10]" ~: [1,2,3,4,1,6,7,8,9,10] ~=? (substitute 5 1 [1..10])
  ]

