package org.invisibletech.datasciencefromscratch

object DataSciencester {
  // Test data from the book.
  // Trying to do this with immutable collections.  ^_^`
  // A property of this simple case is that we order the  maps by id which match the indexes.
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
    Map("id" -> 9, "name" -> "Klein"))

  val friendships = List((0, 1), (0, 2), (1, 2), (1, 3), (2, 3), (3, 4), (4, 5), (5, 6), (5, 7), (6, 8), (7, 8), (8, 9))

  val interests = List((0, "Hadoop"), (0, "Big Data"), (0, "HBase"), (0, "Java"),
    (0, "Spark"), (0, "Storm"), (0, "Cassandra"),
    (1, "NoSQL"), (1, "MongoDB"), (1, "Cassandra"), (1, "HBase"),
    (1, "Postgres"), (2, "Python"), (2, "scikit-learn"), (2, "scipy"),
    (2, "numpy"), (2, "statsmodels"), (2, "pandas"), (3, "R"), (3, "Python"),
    (3, "statistics"), (3, "regression"), (3, "probability"),
    (4, "machine learning"), (4, "regression"), (4, "decision trees"),
    (4, "libsvm"), (5, "Python"), (5, "R"), (5, "Java"), (5, "C++"),
    (5, "Haskell"), (5, "programming languages"), (6, "statistics"),
    (6, "probability"), (6, "mathematics"), (6, "theory"),
    (7, "machine learning"), (7, "scikit-learn"), (7, "Mahout"),
    (7, "neural networks"), (8, "neural networks"), (8, "deep learning"),
    (8, "Big Data"), (8, "artificial intelligence"), (9, "Hadoop"),
    (9, "Java"), (9, "MapReduce"), (9, "Big Data"))

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

  def resolveFriendById(users: CompiledUsers, selfId: Int, friendId: Int) = {
    reifyFriends(users.usersWithFriends.filter(m => resolveField(m, "id") == selfId).head, users)
      .filter(m => resolveField(m, "id") == friendId).head
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

  def compileInterestsToUsers(insterests: List[(Int, String)]) = {
    interests.map(i => (i._2, i._1)).groupBy(_._1).map({ case (k, v) => (k, v.map(_._2)) })
  }
}