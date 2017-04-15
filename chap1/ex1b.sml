type key = string
datatype 'a tree = LEAF | TREE of 'a tree * (key * 'a) * 'a tree

exception NotFound;

val empty = LEAF

fun insert (key, value, LEAF) = TREE(LEAF, (key, value), LEAF)
  | insert (key, value, TREE(l, (k, v), r)) =
    if key < k
      then TREE(insert (key, value, l), (k, v), r)
    else if key > k
      then TREE(l, (k, v), insert (key, value, r))
    else TREE(l, (key, value), r)

fun lookup (key, LEAF) = raise NotFound
  | lookup (key, TREE(l, (k, v), r)) =
    if key < k
      then lookup (key, l)
    else if key > k
      then lookup (key, r)
    else v

fun myFoldl f init [] = init
  | myFoldl f init ((k,v)::xs) = foldl f (f (k, v, init)) xs

fun createTree l = myFoldl insert LEAF l;

val mytree = createTree [("a",1), ("b",2), ("c",3), ("d",4), ("e",5)]
