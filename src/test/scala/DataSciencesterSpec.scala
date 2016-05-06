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
        val friedships = List((0, 1))
        val users = List(
        Map("id" -> 0, "name" -> "Hero"),
        Map("id" -> 1, "name" -> "Dunn"))

        val result = applyFriends(users, friedships)

        assert(result.size == 2)

        val friendOfHero = resolveFriend(result, 0, 0)
        assert(friendOfHero("name") == "Dunn")

        val friendOfDunn = resolveFriend(result, 1, 0)
        assert(friendOfDunn("name") == "Hero")
    }
  }

  describe("We should be able to get the expected statistics") {
    it("we can get the total number of connections") {
      assert(totalConnections(users, friendships) == 24)
    }
  }
}

