sredinaListe :: [a] -> [a]

sredinaListe xs = if length xs >1 then tail(init xs)
                  else xs