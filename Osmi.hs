module Osmi where

import Sedmi

prostBroj:: Int -> Bool 

prostBroj n = djelitelji n == [1,n]