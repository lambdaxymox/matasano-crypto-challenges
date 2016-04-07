module Util.Util
    (
        right,
    )
    where
    
right :: Either a b -> b
right (Left a)  = error "Got Left."
right (Right b) = b 
