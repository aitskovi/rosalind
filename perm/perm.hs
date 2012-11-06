perms :: [a] -> [[a]]
perms [] = []
perms x:xs = x +++  perms xs
