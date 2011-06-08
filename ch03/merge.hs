merge([],bs) = bs
merge(as,[]) = as
merge (a:as,b:bs) | a <=b = a:merge(as,b:bs)
                  | a > b = b:merge(a:as,bs)

