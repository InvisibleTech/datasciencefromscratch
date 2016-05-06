package org.invisibletech.datasciencefromscratch

object DataSciencester {

    // Trying to do this with immutable collections.  ^_^`
    val users = List(
        Map("id" -> 0, "name" -> "Hero"),
        Map("id" -> 1, "name" -> "Dunn"),
        Map("id" -> 2, "name" -> "Sue"),
        Map("id" -> 3, "name" -> "Chi"),
        Map("id" -> 4, "name" -> "Thor"),
        Map("id" -> 5, "name" -> "Clive"),
        Map("id" -> 6, "name" -> "Hicks"),
        Map("id" -> 7, "name" -> "Devin"),
        Map("id" -> 8, "name" -> "Kate"),
        Map("id" -> 9, "name" -> "Klein")
    )

    val friendships = List((0, 1), (0, 2), (1, 2), (1, 3), (2, 3), (3, 4), (4, 5), (5, 6), (5, 7), (6, 8), (7, 8), (8, 9))

    def createBiDirectionalRelations(relations: List[(Int, Int)]): List[(Int, Int)] = {
        relations.flatMap(r => List(r, (r._2, r._1)))
    }

    // Steps needed to create friends
    // friendships.flatMap (List(..., (_._2, _._1)))
    // Create bidirectional friendships
    // groupBy _._1
    // for graph edges in the list build new friend suppliers for the _._1  

}