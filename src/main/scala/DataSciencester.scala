package org.invisibletech.datasciencefromscratch

object DataSciencester {
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

  type User = Map[String, Any]
  type UsersRaw = List[User]
  type Friendships = List[(Int, Int)]

  type DeferedFriend = scala.Function1[List[User], User]

  case class CompiledUser(id: Int, name: String, friends: List[scala.Function0[CompiledUser]])

  implicit class CompiledUserDirectory(rawData: (UsersRaw, Friendships)) {
    lazy val usersWithFriends: List[CompiledUser] = compile(rawData._1, rawData._2)
    lazy val usersById: Map[Int, CompiledUser] = usersWithFriends
                      .map(u => (u.id, u))
                      .toMap

    def createBiDirectionalRelations(relations: Friendships): Friendships = {
      relations.flatMap(r => List(r, (r._2, r._1)))
    }

    def compile(users: UsersRaw, friendships: Friendships) = {
      val biRelations = createBiDirectionalRelations(friendships)

      biRelations.groupBy(_._1).toList.map(g => 
                      CompiledUser(users(g._1)("id").asInstanceOf[Int],
                                    users(g._1)("name").asInstanceOf[String],   
                                    g._2.map(r => () => usersById(r._2)))
                      ).sortBy(u => u.id)
    }
  }
 
  def totalConnections(usersDirectory: CompiledUserDirectory) = {
    usersDirectory.usersWithFriends.map(_.friends.size).sum
  }

  def averageNumberOfFriends(usersDirectory: CompiledUserDirectory) = {
    totalConnections(usersDirectory) / usersDirectory.usersWithFriends.size.toDouble
  }

  def numberOfFriendsById(usersDirectory: CompiledUserDirectory) = {
    usersDirectory.usersWithFriends.map(u => (u.id, u.friends.size))
  }

  def sortedNumberOfFriendsById(numberOfFriendsById: Friendships) = {
    numberOfFriendsById.sorted(Ordering.by((_: Tuple2[Int, Int])._2).reverse)
  }

  def countOfCommonFoFs(compiledUser: CompiledUser, usersDirectory: CompiledUserDirectory) = {
    val listOfFriendIds = compiledUser.friends.map(f => f().id)

    compiledUser.friends.map(f => (f().id, f().friends.map(f => f().id)
      .filter(c => listOfFriendIds.contains(c))
      .size))
  }

  def compileInterestsToUsers(interests: List[(Int, String)]) = {
    interests.map(i => (i._2, i._1)).groupBy(_._1).map({ case (k, v) => (k, v.map(_._2)) })
  }
}