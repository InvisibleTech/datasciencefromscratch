package org.invisibletech.datasciencefromscratch

import DataSciencester._
import DataSciencester.UserWithRelationships._

import org.scalatest.FunSpec

class UserWithRelationshipsSpec extends FunSpec {
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

        val result = UserWithRelationships(users, friendships)

        assert(result.size == 2)

        val herosFriends: List[DeferedFriend] = result(0)("friends").asInstanceOf[List[DeferedFriend]]
        assert(herosFriends.size == 1)

        val dunnsFriends: List[DeferedFriend] = result(1)("friends").asInstanceOf[List[DeferedFriend]]
        assert(dunnsFriends.size == 1)
    }
  }

}