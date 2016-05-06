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

    /*
        NOTE:
        A lot of the code here is an experiment in using immutable collections to parallel code written 
        in Python from the book: Data Science from Scratch

        That book makes heavy use of List and Map, and uses them in a mutable fashion.  This code is not
        an efficient or refined model.  In some cases it is extreme - like applyFriends which uses 
        lazy evaluation and currying like patterns to allow me to get a friend before I even know who 
        their friends are.  I am certain - due to lack of depth in Scala FP - there are better ways.

        I will try to keep driving this FP, Immutable Approach as long as possible.  Or until I surrender
        and follow along with Python creating non-FP, mutable Scala.  I am trying to learn more about ML
        by implementing it.  So I am trying to have fun.
    */

    val friendships = List((0, 1), (0, 2), (1, 2), (1, 3), (2, 3), (3, 4), (4, 5), (5, 6), (5, 7), (6, 8), (7, 8), (8, 9))

    def createBiDirectionalRelations(relations: List[(Int, Int)]): List[(Int, Int)] = {
        relations.flatMap(r => List(r, (r._2, r._1)))
    }

    def applyFriends(users: List[Map[String, Any]], friendships: List[(Int, Int)]) = {
        val biRelations = createBiDirectionalRelations(friendships)

        biRelations.groupBy(_._1).toList.map(g => users(g._1) + ("friends" -> g._2.map(r => (u: List[Map[String, Any]]) => (u(r._2)) )))
    }

    def resolveFriend(users: List[Map[String, Any]], selfIndex: Int, friendIndex: Int) = {
       users(selfIndex)("friends").asInstanceOf[List[scala.Function1[List[Map[String, Any]], Map[String, Any]]]](friendIndex)(users)
    }

    def totalConnections(users: List[Map[String, Any]], friendships: List[(Int, Int)]) = {
        val connectedUsers = applyFriends(users, friendships)

        connectedUsers.map(u => u("friends").asInstanceOf[List[_]].size).sum
    }

}