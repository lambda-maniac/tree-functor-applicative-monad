data Tree a = End a
            | Node (Tree a) (Tree a)

instance Show a => Show (Tree a) where
    show (End  element   ) = (<>) ((<>) "(End " $ show element) ")"
    show (Node left right) = (<>) ((<>) ((<>) ((<>) "(Node " $ show left) " ") $ show right) ")"

instance Functor Tree where
    fmap f (End  element   ) = End $ f element
    fmap f (Node left right) = Node ((<$>) f left) $ (<$>) f right
         where (<$>) = fmap

instance Applicative Tree where
    pure = End
    (<*>) (End  f           ) tree = (<$>) f tree
    (<*>) (Node fleft fright) tree = Node ((<*>) fleft tree) ((<*>) fright tree)

instance Monad Tree where
    (>>=) (End  element   ) f = f element
    (>>=) (Node left right) f = Node ((>>=) left f) ((>>=) right f)
