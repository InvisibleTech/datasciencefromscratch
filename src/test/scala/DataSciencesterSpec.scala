package org.invisibletech.datasciencefromscratch

import DataSciencester._

import org.scalatest.FunSpec

class DataSciencesterSpec extends FunSpec {
  describe("Relations can be made bidirectional") {
    it("should show graph edge both ways"){
      val result = createBiDirectionalRelations(List((1, 2)))  

      assert(result == List((1,2), (2, 1)))
    }
    it("will produce duplicates if 1 way relationships repeat relations") {
      val result = createBiDirectionalRelations(List((1, 2), (2, 1)))  
      
      assert(result == List((1,2), (2, 1), (2, 1), (1, 2)))
    }
  }

  describe("We should be able to establish friendships immutably and with delayed evaluation") {
    it("should allow following friends from myself and back again") {
        val friendships = List((0, 1))
        val users = List(
        Map("id" -> 0, "name" -> "Hero"),
        Map("id" -> 1, "name" -> "Dunn"))

        val result = applyFriends(users, friendships)

        assert(result.size == 2)

        val friendOfHero = resolveFriend(result, 0, 0)
        assert(friendOfHero("name") == "Dunn")

        val friendOfDunn = resolveFriend(result, 1, 0)
        assert(friendOfDunn("name") == "Hero")
    }
  }

  describe("We should be able to get the expected statistics") {
    it("total number of connections") {
      assert(totalConnections(applyFriends(users, friendships)) == 24)
    }

    it("number of friends of one user") {
      assert(numberOfFriends(applyFriends(users, friendships)(1)) == 3)
    }

    it("avg number of friends") {
      assert(averageNumberOfFriends(applyFriends(users, friendships)) == 2.4)
    }

    it("number of friends by id") {
      val friends = applyFriends(users, friendships)
      assert(numberOfFriendsById(friends).size == 10)
      assert(numberOfFriendsById(friends) == List((0,2), (5,3), (1,3), (6,2), (9,1), (2,3), (7,2), (3,3), (8,3), (4,2)))
    }

    it("sorted number of friends by id") {
      val friends = numberOfFriendsById(applyFriends(users, friendships))
      assert(sortedNumberOfFriendsById(friends).size == 10)
      assert(sortedNumberOfFriendsById(friends) == List((5,3), (1,3), (2,3), (3,3), (8,3), (0,2), (6,2), (7,2), (4,2), (9,1)))
    }
  }
}

