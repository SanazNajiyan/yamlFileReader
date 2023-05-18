# yamlFileReader

Lemma (a): The YamlTree type can be defined as YamlTree [(String, YamlTree)].

Lemma (b): The length function has the type length :: [a] -> Int.

Lemma (c): The length function has the property that length (xs ++ ys) = length xs + length ys.

Lemma (d): The longestPath function has the type longestPath :: YamlTree -> [String] :: Path (i.e., it returns a list of strings).

Lemma (e): The depth function has the type depth :: YamlTree -> Int (i.e., it returns an integer).

Lemma (f): For any non-empty list of strings xs, the length of the longest path in a YamlTree whose longest path starts with xs is equal to the length of xs plus the length of the longest path in the subtree rooted at the node that corresponds to xs.

Lemma (g) : function composition: (f . g) x = f (g x)

Lemma (r) : basic property of list concatination : (x : [ys]) = [x] ++ [ys]

Claim: For any YamlTree t, length . longestPath t = depth t

We will prove this claim by induction on the structure of YamlTree.

Base Case: YamlTree []
The longest path is the empty list, so length (longestPath (YamlTree [])) = length [] = 0. Also, the depth of the tree is 0. Therefore, length . longestPath (YamlTree []) = depth (YamlTree []) = 0.

Proof:

Induction Hypothesis:
Assume that length . longestPath = depth holds for all YamlTrees with depth of k

Inductive Step:
Consider a YamlTree with depth k+1. We can represent it as YamlTree ((s1, t1) : rest).

(1) By definition of depth, the depth of the current tree is the maximum of the depth of t1 or the maximum depth of any tree in rest, plus one. 
Therefore, the depth of the current tree is equal to max(depth(t1), max(depth(ti) | (si, ti) in rest)) + 1. 
if the tree is empty the depth is zero. If the node is single_leaf_then it is one. Note that the depth function takes a YamlTree as input.

(2) By definition of longestPath, the longest path in the current tree is either the path starting from s1 and the longest path in t1, or the longest path in one of the subtrees in rest. As the base case an empty tree has no path. 

*We deal with 2 cases in YamlTree ((s1, t1) : rest) 

case 1:
If the longest path in the current tree goes through t1, then by (2), the longest path is s1 and the rest of the nodes in the longest path of t1 which could be shown as length (s1 : longestPath t1)

length (s1 : longestPath t1) = length ([s1] ++ longestPath t1) -- by (r)

length ([s1] ++ longestPath t1) = length [s1] + length (longestPath t1) -- by (c)

length [s1] + length (longestPath t1) = length [s1] + (length . longestPath) t1 -- by (g) 

length [s1] + (length . longestPath) t1 = 1 + (length . longestPath) t1 -- by (b) length of singleton is 1

1 + (length . longestPath) t1 = 1 + depth t1 -- by I.H.

1 + depth t1 = 1 + k -- by I.H.

by (1) we could also see that (k + 1) is the depth (t1) + 1 which is the depth of t1.

Therefore, for case 1, we started from the longet path applied to t1 and arrived at the depth corresponding to t1 which is equal to k + 1 (the depth of the YamlTree) 

Case 2:
If the longest path in the current tree goes through one of the trees in rest, say (s2, t2), then by (2), the longest path is the longest path in t2 with s2 added in the beginning of the list. We can represent this as s2 : longestPath t2.

By definition of depth, the depth of the current tree is equal to the maximum of the depth of t1 or the maximum depth of any tree in rest, plus one. Therefore, the depth of the current tree is equal to max(depth(t2), max(depth(ti) | (si, ti) in rest and si != s2)) + 1.

length (longestPath (YamlTree ((s1, t1) : rest))) = length (s2 : longestPath t2) -- by (2) 

This case will be the same as case 1 since we deal with the data structure YamlTree with a list of tuples 

Both cases are essentially the same because they both deal with the YamlTree data structure represented as a list of tuples. In both cases, we start from the longest path applied to a subtree, and then we show that we arrive at the depth corresponding to the entire YamlTree. So, we only need to consider one case instead of two.

