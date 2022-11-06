module Main (main) where

main::IO()
main = putStr (show (end [1..5]))

end :: [a]-> a
end []= error "The list is empty!"
end [a] = a  
end (_ : ab)= end ab
