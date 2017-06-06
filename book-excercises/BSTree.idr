module BSTree

data BSTree : Type -> Type where
     Empty : Ord elem => BSTree elem
     Node : Ord elem => (left : BSTree elem) -> (val : elem) ->
                        (right : BSTree elem) -> BSTree elem

insert : elem -> BSTree elem -> BSTree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right)
      = case compare x val of
             LT => Node (insert x left) val right
             EQ => orig
             GT => Node left val (insert x right)

listToTree : Ord elem => List elem -> BSTree elem
listToTree [] = Empty
listToTree [x] = Node Empty x Empty
listToTree (x :: xs) = insert x (listToTree xs)

treeToList : BSTree elem -> List elem
treeToList Empty = []
treeToList (Node left val right) = (treeToList left) ++ [val] ++ (treeToList right)




