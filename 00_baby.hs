doubleUs x y = doubleMe x + doubleMe y
doubleMe x = x + x

doubleSmallNumber x = if x > 100 then x else x * 2
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

boomBangs xs = [if x < 10 then "BANG!" else "BOOM!" | x <- xs, odd x]
