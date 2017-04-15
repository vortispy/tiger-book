type key = string
datatype tree = LEAF | TREE of tree * key * tree

val empty = LEAF

fun insert (key, LEAF) = TREE(LEAF, key, LEAF)
  | insert (key, TREE(l, k, r)) =
    if key < k
      then TREE(insert (key, l), k, r)
    else if key > k
      then TREE(l, k, insert (key, r))
    else TREE(l, key, r)

fun member (key, LEAF) = false
  | member (key, TREE(l, k, r)) =
    if key < k
      then member (key, l)
    else if key > k
      then member (key, r)
    else true

val treeA = foldl insert LEAF ["t", "s", "p", "i", "p", "f", "b", "s", "t"]
val treeB = foldl insert LEAF ["a", "b", "c", "d", "e", "f", "g", "h", "i"]

fun depth dep LEAF = dep
  | depth dep (TREE(l, k, r)) =
    let val l_depth = depth (dep+1) l
      val r_depth = depth (dep+1) r
    in if l_depth >= r_depth then l_depth else r_depth
    end
(*
  1
 / \
2   3

      1
     / \
    2   3
 / \    / \
4   5  6   7
*)
