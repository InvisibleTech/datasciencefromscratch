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


  object UserWithRelationships {
    def createBiDirectionalRelations(relations: Friendships): Friendships = {
      relations.flatMap(r => List(r, (r._2, r._1)))
    }

    def apply(users: UsersRaw, friendships: Friendships) = {
      val biRelations = createBiDirectionalRelations(friendships)

      biRelations.groupBy(_._1).toList.map(g => users(g._1) +
        ("friends" -> g._2.map(r => (u: List[Map[String, Any]]) => (u.filter(m => resolveField(m, "id") == r._2).head))))
        .sortBy(m => m("id").asInstanceOf[Int])
    }
  }
/*
I have some serious questions about what is where.

I think this is getting worse.  So, let's come up with some kind of 
design theme.

[1] There is some compiling of Users.  This should produce a collection of CompiledUser
classes.  Not Maps.  Fuck that I mean, that is a Clojure, Python and Ruby way.  Which
is fine except this is Scala.  So we need a CompiledUser  which is a case class 
For the love of design use an ImmutableList for the friendships. See about apply
and unapply to help with cloning.

[2] CompiledUser(s) should be under a collecton by user Id as CompiledUsers which 
is the implicit class we compile from the User and Friendship data into a map
of CompiledUser objects keyed by id
*/

  implicit class CompiledUsers(rawData: (UsersRaw, Friendships)) {
    lazy val usersWithFriends = UserWithRelationships(rawData._1, rawData._2)
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