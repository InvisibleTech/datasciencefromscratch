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

  object CompiledUsers {
    def createBiDirectionalRelations(relations: Friendships): Friendships = {
      relations.flatMap(r => List(r, (r._2, r._1)))
    }

    def applyFriends(users: UsersRaw, friendships: Friendships) = {
      val biRelations = createBiDirectionalRelations(friendships)

      biRelations.groupBy(_._1).toList.map(g => users(g._1) +
        ("friends" -> g._2.map(r => (u: List[Map[String, Any]]) => (u.filter(m => resolveField(m, "id") == r._2).head))))
        .sortBy(m => m("id").asInstanceOf[Int])
    }
  }

  implicit class CompiledUsers(rawData: (UsersRaw, Friendships)) {
    lazy val usersWithFriends = CompiledUsers.applyFriends(rawData._1, rawData._2)
    lazy val usersById: Map[Int, Map[String, Any]] = usersWithFriends
                      .map(u => (u("id").asInstanceOf[Int], u))
                      .toMap

    def userById(id: Int) = {
      usersById(id)
    }
  }

  def resolveListOfFriendFunctions(connectedUser: User) = {
    connectedUser.get("friends").map(_.asInstanceOf[List[DeferedFriend]])
      .getOrElse(List[DeferedFriend]())
  }

  def reifyFriends(connectedUser: User, users: CompiledUsers) = {
    resolveListOfFriendFunctions(connectedUser).map(f => f(users.usersWithFriends))
  }
 
  def totalConnections(users: CompiledUsers) = {
    users.usersWithFriends.map(numberOfFriends(_)).sum
  }

  def numberOfFriends(connectedUser: User) = {
    resolveListOfFriendFunctions(connectedUser).size
  }

  def averageNumberOfFriends(users: CompiledUsers) = {
    totalConnections(users) / users.usersWithFriends.size.toDouble
  }

  def numberOfFriendsById(users: CompiledUsers) = {
    users.usersWithFriends.map(u => (u("id").asInstanceOf[Int], numberOfFriends(u)))
  }

  def sortedNumberOfFriendsById(numberOfFriendsById: Friendships) = {
    numberOfFriendsById.sorted(Ordering.by((_: Tuple2[Int, Int])._2).reverse)
  }

  def resolveField[T](connectedUser: User, name: String) = {
    connectedUser.get(name).map(_.asInstanceOf[T]).getOrElse(null)
  }

  def countOfCommonFoFs(connectedUser: User, users: CompiledUsers) = {
    val friendsOfUser = reifyFriends(connectedUser, users)
    val listOfFriendIds = friendsOfUser.map(f => resolveField[Int](f, "id"))

    friendsOfUser.map(f => ((resolveField[Int](f, "id")),
      reifyFriends(f, users)
      .map(f => resolveField[Int](f, "id"))
      .filter(c => listOfFriendIds.contains(c))
      .size))
  }

  def compileInterestsToUsers(interests: List[(Int, String)]) = {
    interests.map(i => (i._2, i._1)).groupBy(_._1).map({ case (k, v) => (k, v.map(_._2)) })
  }
}