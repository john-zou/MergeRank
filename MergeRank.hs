import Data.Sequence as S
import Data.Set as Set

-- finds a ranking when given a tournament G comprising vertices, edges
mergeRank :: Ord a => Seq a -> Set (a,a) -> Seq a
mergeRank vertices edges
    -- if there are fewer than 2 players, then just return the player
    | S.length vertices < 2 = vertices
    | otherwise = 
        -- split the list of players in half and recursively call mergeRank on each half, and merge the halves using the merge function
        let mid = (S.length vertices + 1) `div` 2 in
        merge S.empty (mergeRank (S.take mid vertices) edges) (mergeRank (S.drop mid vertices) edges) edges
    
-- merges two rankings together
merge :: Ord a => Seq a -> Seq a -> Seq a -> Set (a, a) -> Seq a
merge m a b edges
    -- if one of the rankings is empty, concatenate the other to the already-merged ranking m
    | S.length a == 0 = m >< b
    | S.length b == 0 = m >< a
    -- take the highest ranked player from both of the rankings, remove the winner from their original ranking, and add them to the end
    -- of the already-merged ranking m (and recursively call merge on the resulting sequences)
    | Set.member (S.index a 0, S.index b 0) edges = merge (m |> S.index a 0) (S.drop 1 a) b edges
    | otherwise = merge (m |> S.index b 0) a (S.drop 1 b) edges


-- Test code --
-- To test test case 1, for example, just execute: "test p1 e1" in the Haskell interpreter (GHCI) and "True" should be printed

-- Returns whether a permutation is a ranking:
isRanking :: Ord a => Seq a -> Set (a, a) -> Bool
isRanking permutation edges
    -- a permutation of length < 2 is always a ranking
    | S.length permutation < 2 = True
    -- a permutation (p1,...,pn) is a ranking if (p1,p2) is in E and (p2,...,pn) is a ranking
    | Set.member (S.index permutation 0, S.index permutation 1) edges = isRanking (S.drop 1 permutation) edges
    -- a permutation (p1,...,pn) is not a ranking if 
    | otherwise = False

-- Convenience function to run mergeRank on a sequence of vertices and a set of edges, and then returns whether it was a ranking that got returned --
test :: Ord a => Seq a -> Set (a,a) -> Bool
test p e = isRanking (mergeRank p e) e


------------------------------- Trivial test cases
-- Test case 1:
-- 1 player
-- We expect the ranking generated to just be this one player
p1 = S.fromList [1]
e1 = Set.empty

-- Test case 2:
-- 2 players
-- "1" beat "2", so we expect the ranking produced to be [1,2]
p2 = S.fromList[1,2]
e2 = Set.fromList[(1,2)]

-- Test case 3:
-- Like test case 2 but reverse order, we expect the ranking produced to be [2,1]
p3 = S.fromList[1,2]
e3 = Set.fromList[(2,1)]


--------------------------- Interesting test cases
-- Test case 4:
-- 3 players
-- Each player beat 1 other player
-- Multiple possible rankings:
-- [1,2,3], [2,3,1], [3,1,2]
-- The mergeRank algorithm deterministically produces [3,1,2] because:
-- it splits the initial list [1,2,3] into [1,2] and [3] because I arbitrarily set the midpoint to be the ceiling of len/2
-- [1,2] is split further into [1] and [2] and merged into [1,2]
-- [1,2] and [3] are merged together placing 3 in front because (3,1) is in the set of edges, resulting in [3,1,2]
p4 = S.fromList[1,2,3]
e4 = Set.fromList[(1,2),(2,3),(3,1)]

-- Test case 4'1:
-- exactly the same as test case 4 except the initial sequence is arbitrarily ordered differently
-- the alternate valid ranking [1,2,3] is deterministically produced in this case:
-- [2,3,1] is split into [2,3] and [1]
-- When [2,3] and [1] are merged, (1,2) is in the set of edges, resulting in [1,2,3]
p4'1 = S.fromList[2,3,1]
e4'1 = Set.fromList[(1,2),(2,3),(3,1)]

-- Test case 4'2:
-- exactly the same as test case 4'1 with a different initial sequence
-- this initial sequence results in the exact same ranking produced, [1,2,3]:
-- [3,2,1] is split into [3,2] and [1]
-- [3,2] is split into [3] and [2] and merged into [2,3] since (2,3) is in the set of edges
-- [2,3] and [1] are merged just like before
p4'2 = S.fromList[3,2,1]
e4'2 = Set.fromList[(1,2),(2,3),(3,1)]

-- Test case 5:
-- 3 players
-- player 2 beat both the others
-- player 1 beat player 3
-- only ranking: [2,1,3]
p5 = S.fromList[1,2,3]
e5 = Set.fromList[(2,1),(2,3),(1,3)]

-- Test case 6:
-- 8 players
-- player 1 beat everyone else
-- player 2 beat everyone but player 1
-- player 3 beat everyone but players 1 & 2, etc.
-- only ranking: [1..8]
-- The initial list has an arbitrary order that is reverse of the only valid ranking
-- The algorithm works like this:
-- [8,7,6,5] [4,3,2,1]
-- [8,7] [6,5] [4,3] [2,1]
-- [8] [7] [6] [5] [4] [3] [2] [1]
-- [7,8] [5,6] [3,4] [1,2]
-- Merging of [7,8] and [5,6] (rest is similar):
--      merge [] [7,8] [5,6], where the first empty "list" (actually a "sequence") represents the growing merged list
--      merge [5] [7,8] [6]
--      merge [5,6] [7,8] []
--      return [5,6,7,8] (edge condition of 1 of the arrays being empty calls for the remaining one to be added to the end of the merged list)
-- etc. and the final result is [1..8]
p6 = S.fromList [8,7,6,5,4,3,2,1]
e6 = Set.fromList[(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(2,3),(2,4),(2,5),(2,6),(2,7),(2,8),(3,4),(3,5),(3,6),(3,7),(3,8),(4,5),(4,6),(4,7),(4,8),(5,6),(5,7),(5,8),(6,7),(6,8),(7,8)]
