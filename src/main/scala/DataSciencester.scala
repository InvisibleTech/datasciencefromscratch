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


    type deferedFriend = scala.Function1[List[Map[String, Any]], Map[String, Any]]

    def createBiDirectionalRelations(relations: List[(Int, Int)]): List[(Int, Int)] = {
        relations.flatMap(r => List(r, (r._2, r._1)))
    }

    def applyFriends(users: List[Map[String, Any]], friendships: List[(Int, Int)]) = {
        val biRelations = createBiDirectionalRelations(friendships)

        biRelations.groupBy(_._1).toList.map(g => users(g._1) + ("friends" -> g._2.map(r => (u: List[Map[String, Any]]) => (u.filter(m => resolveField(m, "id") == r._2).head) )))
    }

    def resolveListOfFriendFunctions(connectedUser: Map[String, Any]) = {
        connectedUser.get("friends").map(_.asInstanceOf[List[deferedFriend]])
                        .getOrElse(List[deferedFriend]())
    }

    def reifyFriends(connectedUser: Map[String, Any], users: List[Map[String, Any]]) = {
      resolveListOfFriendFunctions(connectedUser).map(f => f(users))
    }

    def resolveFriendById(users: List[Map[String, Any]], selfId: Int, friendId: Int) = {
       reifyFriends(users.filter(m => resolveField(m, "id") == selfId).head, users)
            .filter(m => resolveField(m, "id") == friendId).head
    }

    def totalConnections(connectedUsers: List[Map[String, Any]]) = {
        connectedUsers.map(numberOfFriends(_)).sum
    }

    def numberOfFriends(connectedUser: Map[String, Any]) = {
        resolveListOfFriendFunctions(connectedUser).size
    }

    def averageNumberOfFriends(connectedUsers: List[Map[String, Any]]) = {
        totalConnections(connectedUsers) / connectedUsers.size.toDouble
    }

    def numberOfFriendsById(connectedUsers: List[Map[String, Any]]) = {
        connectedUsers.map(u => (u("id").asInstanceOf[Int], numberOfFriends(u)))
    }

    def sortedNumberOfFriendsById(numberOfFriendsById: List[(Int, Int)]) = {
        numberOfFriendsById.sorted(Ordering.by((_: Tuple2[Int, Int])._2).reverse)    
    }

    def resolveField[T](connectedUser: Map[String, Any], name: String) = {
        connectedUser.get(name).map(_.asInstanceOf[T]).getOrElse(null)
    }

    def countOfCommonFoFs(connectedUser: Map[String, Any], users: List[Map[String, Any]]) = {
      val friendsOfUser = reifyFriends(connectedUser, users)
      val listOfFriendIds = friendsOfUser.map(f => resolveField[Int](f, "id"))

      // Want to build a series of groupbys
      friendsOfUser.map(f => ((resolveField[Int](f, "id")),
                              reifyFriends(f, users)
                                .map(f => resolveField[Int](f, "id")) ++ listOfFriendIds ))

      // WIP not finished - no test.

    }
}